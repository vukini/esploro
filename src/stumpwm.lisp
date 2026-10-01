;;;; stumpwm.lisp — asking the running StumpWM, through its Swank.
;;;;
;;;; StumpWM with Swank in it (Vikix starts one on 127.0.0.1:4004) runs any
;;;; Lisp it's sent. Esploro sends small forms, read in the STUMPWM
;;;; package, and reads back their value. When StumpWM isn't there,
;;;; STUMPWM-EVAL signals STUMPWM-UNREACHABLE and Esploro carries on
;;;; without knowing the windows.
;;;;
;;;; ESPLORO_SWANK_PORT (or Vikix's VIKIX_SWANK_PORT) changes the port.
;;;; When ~/.slime-secret exists, Swank lets in only a client that sends
;;;; its first line first, as SLIME does; so does this.

(in-package #:esploro)

(define-condition stumpwm-unreachable (error)
  ((reason :initarg :reason :reader stumpwm-unreachable-reason))
  (:report (lambda (c s) (format s "StumpWM couldn't be asked: ~a" (stumpwm-unreachable-reason c)))))

(defun unreachable (control &rest args)
  (error 'stumpwm-unreachable :reason (apply #'format nil control args)))

(defun swank-port ()
  (or (some (lambda (var)
              (let ((value (sb-posix:getenv var)))
                (and value (parse-integer value :junk-allowed t))))
            '("ESPLORO_SWANK_PORT" "VIKIX_SWANK_PORT"))
      4004))

(defun slime-secret ()
  "The first line of ~/.slime-secret, or NIL. Swank checks it the same way."
  (let ((file (join-path (home-folder) ".slime-secret")))
    (when (path-exists-p file)
      (with-open-file (in (native file) :external-format :utf-8)
        (read-line in nil nil)))))

(defun listener-uid (port)
  "The user owning what listens on 127.0.0.1:PORT, from /proc/net/tcp."
  (ignore-errors
   (with-open-file (in "/proc/net/tcp")
     (read-line in)
     (loop for line = (read-line in nil)
           while line
           do (let* ((cols (remove "" (split-on #\Space line) :test #'string=))
                     (local (nth 1 cols))
                     (colon (position #\: local)))
                (when (and (= (parse-integer local :start (1+ colon) :radix 16) port)
                           (string= (nth 3 cols) "0A")
                           (member (subseq local 0 colon) '("0100007F" "00000000") :test #'string=))
                  (return (parse-integer (nth 7 cols)))))))))

(defun split-on (char string)
  (loop with start = 0
        for at = (position char string :start start)
        collect (subseq string start at)
        while at do (setf start (1+ at))))

;;; Swank's framing: six hex digits giving the length in bytes, then the
;;; message as UTF-8.
(defun swank-send (stream text)
  (let ((body (sb-ext:string-to-octets text :external-format :utf-8)))
    (write-sequence (sb-ext:string-to-octets (format nil "~(~6,'0x~)" (length body))) stream)
    (write-sequence body stream)
    (force-output stream)))

(defun read-octets (stream n)
  (let ((buffer (make-array n :element-type '(unsigned-byte 8))))
    (unless (= (read-sequence buffer stream) n)
      (unreachable "StumpWM closed the connection"))
    buffer))

(defun swank-receive (stream)
  (let ((length (parse-integer (sb-ext:octets-to-string (read-octets stream 6)) :radix 16)))
    (sb-ext:octets-to-string (read-octets stream length) :external-format :utf-8)))

(defun read-foreign (text)
  "TEXT read as data: no #. and its symbols kept out of Esploro's package."
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*package* (find-package '#:esploro.read)))
      (read-from-string text))))

(defun lisp-string (text)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across text
          do (when (find c "\"\\") (write-char #\\ out))
             (write-char c out))
    (write-char #\" out)))

;;; The form runs in StumpWM's main thread (StumpWM isn't thread-safe;
;;; Swank's thread isn't it), gives up after a few seconds when that
;;; thread is busy (a menu open), catches its own errors so Swank's
;;; debugger never opens, and prints its value readably.
(defparameter *wrapper* "(let* ((done (sb-thread:make-semaphore))
       (result \"(:error \\\"StumpWM's main thread didn't answer (a menu or prompt open?)\\\")\")
       (cancelled nil)
       (job (lambda ()
              (unwind-protect
                   (unless cancelled
                     (setf result
                           (handler-case
                               (let ((*print-pretty* nil) (*print-readably* nil)
                                     (*print-circle* nil) (*package* (find-package :cl-user)))
                                 (prin1-to-string (list :ok (progn ~a))))
                             (error (e) (prin1-to-string (list :error (princ-to-string e)))))))
                (sb-thread:signal-semaphore done)))))
  (if (and (fboundp 'stumpwm::call-in-main-thread)
           (not (and (fboundp 'stumpwm::in-main-thread-p) (stumpwm::in-main-thread-p))))
      (funcall 'stumpwm::call-in-main-thread job)
      (funcall job))
  (unless (sb-thread:wait-on-semaphore done :timeout ~d) (setf cancelled t))
  (write-string result)
  nil)")

(defun stumpwm-eval (form-text &key (timeout 5))
  "Evaluate FORM-TEXT (Lisp, as text, read in the STUMPWM package) in the
running StumpWM and return its value, which must print readably as data
(strings, numbers, lists). STUMPWM-UNREACHABLE when StumpWM can't be
asked, or the form failed there."
  (let ((port (swank-port))
        (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (handler-case (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
             (error () (unreachable "nothing is listening on 127.0.0.1:~d" port)))
           ;; The port must be yours before the password goes to it.
           (let ((owner (listener-uid port)))
             (when (and owner (/= owner (sb-posix:getuid)))
               (unreachable "127.0.0.1:~d is another user's (uid ~d)" port owner)))
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t :element-type '(unsigned-byte 8)
                                 :buffering :full :timeout (+ timeout 5)))
                 (secret (slime-secret)))
             ;; Sent as it is, not as a Lisp string: Swank compares the raw
             ;; packet, as SLIME's slime-send-secret sends it.
             (when secret (swank-send stream secret))
             (swank-send stream
                         (format nil "(:emacs-rex (swank:eval-and-grab-output ~a) \"STUMPWM\" t 1)"
                                 (lisp-string (format nil *wrapper* form-text timeout))))
             (handler-case
                 (loop for reply = (swank-receive stream)
                       do (cond ((and (> (length reply) 8) (string= "(:return" reply :end2 8))
                                 (return (stumpwm-answer (read-foreign reply))))
                                ((and (> (length reply) 8) (string= "(:debug " reply :end2 8))
                                 ;; Swank's own debugger opened (the wrapper
                                 ;; never lets a form's error get there): leave it.
                                 (let ((thread (second (read-foreign reply))))
                                   (swank-send stream (format nil "(:emacs-rex (swank:throw-to-toplevel) \"STUMPWM\" ~d 2)" thread))
                                   (unreachable "Swank failed on the form")))))
               (sb-sys:interactive-interrupt (c) (error c))
               (stumpwm-unreachable (c) (error c))
               (error (e) (unreachable "~a" e)))))
      (sb-bsd-sockets:socket-close socket))))

(defun stumpwm-answer (reply)
  "The value inside Swank's (:return (:ok (OUTPUT VALUE)) ID)."
  (let ((how (second reply)))
    (unless (and (consp how) (string= (symbol-name (first how)) "OK"))
      (unreachable "Swank refused the form (~a): is it StumpWM's Swank?" how))
    (let ((answer (read-foreign (first (second how)))))
      (if (string= (symbol-name (first answer)) "OK")
          (second answer)
          (unreachable "~a" (second answer))))))
