;;;; phone.lisp — an iPhone on the cable, like a drive.
;;;;
;;;; libimobiledevice says which phones are plugged in (idevice_id) and
;;;; their names (ideviceinfo); ifuse mounts one at ~/iphone (where Vid's
;;;; `iphone' script mounts it too, so they share it); fusermount unmounts.
;;;; A phone not paired yet asks to be trusted first: unlocked, "Trust".

(in-package #:esploro)

(defun phone-folder ()
  (or (let ((env (sb-posix:getenv "ESPLORO_PHONE_FOLDER"))) (and env (plusp (length env)) env))
      (join-path (home-folder) "iphone")))

(defun program-output (program &rest args)
  "PROGRAM's standard output, trimmed; NIL when it isn't there or says nothing."
  (ignore-errors
   (let ((out (string-trim '(#\Newline #\Space)
                           (with-output-to-string (s)
                             (sb-ext:run-program "timeout" (list* "5" program args)
                                                 :search t :output s :error nil :input nil :wait t)))))
     (and (plusp (length out)) out))))

(defun phones ()
  "The phones plugged in: ((ID NAME) ...)."
  (let ((ids (program-output "idevice_id" "-l")))
    (when ids
      (loop for id in (remove "" (split-on #\Newline ids) :test #'string=)
            collect (list id (or (program-output "ideviceinfo" "-u" id "-k" "DeviceName") "iPhone"))))))

(defun phone-status ()
  "(:phones ((ID NAME) ...) MOUNTED-FOLDER-OR-NIL)."
  (let ((folder (phone-folder)))
    (list :phones (phones) (and (mounted-p folder) folder))))

(defun mount-phone (&optional id)
  "Mount the phone ID (the first plugged in) at the phone folder: the folder.
An error saying what to do when it can't."
  (let ((folder (phone-folder)))
    (if (mounted-p folder)
        folder
        (let ((id (or id (first (first (phones))))))
          (unless id (error "no iPhone is plugged in (or it's locked: unlock it)"))
          (unless (or (eql 0 (sb-ext:process-exit-code
                              (sb-ext:run-program "idevicepair" (list "-u" id "validate") :search t :output nil :error nil :wait t)))
                      (eql 0 (sb-ext:process-exit-code
                              (sb-ext:run-program "idevicepair" (list "-u" id "pair") :search t :output nil :error nil :wait t))))
            (error "the iPhone isn't paired: unlock it, tap Trust, then try again"))
          (ensure-folder folder)
          (unless (and (eql 0 (sb-ext:process-exit-code
                               (sb-ext:run-program "timeout" (list "20" "ifuse" "-u" id folder)
                                                   :search t :output nil :error nil :wait t)))
                       (mounted-p folder))
            (error "the iPhone couldn't be opened (is it unlocked?)"))
          folder))))

(defun unmount-phone ()
  "Unmount the phone folder: T, or NIL when something still has a file of it open."
  (let ((folder (phone-folder)))
    (or (not (mounted-p folder))
        (eql 0 (sb-ext:process-exit-code
                (sb-ext:run-program "fusermount" (list "-u" "--" folder) :search t :output nil :error nil :wait t))))))
