;;;; where.lisp — which window has a file open.
;;;;
;;;; StumpWM knows the windows, and each window's process (_NET_WM_PID).
;;;; /proc knows what that process and its children have open, what they
;;;; were started on, and (for a shell in a terminal) which folder they're
;;;; in. Emacs keeps files in buffers rather than open, so it is asked
;;;; itself. Together: for each file, the windows that have it, and how.

(in-package #:esploro)

(defstruct window
  id class title group pid)

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type t)
    (format stream "~a ~s on ~a" (window-class window) (window-title window) (window-group window))))

(defun stumpwm-windows ()
  "Every window StumpWM manages; NIL when StumpWM can't be asked."
  (handler-case
      (loop for (id class title group pid)
              in (stumpwm-eval "(mapcar (lambda (w)
                                          (list (xlib:window-id (window-xwin w))
                                                (window-class w) (window-title w)
                                                (group-name (window-group w))
                                                (first (ignore-errors (xlib:get-property (window-xwin w) :_net_wm_pid)))))
                                        (all-windows))")
            collect (make-window :id id :class class :title title :group group :pid pid))
    (stumpwm-unreachable () nil)))

(defun focus-window (id)
  "Bring window ID to the front, on its workspace. True when it was there."
  (handler-case
      (stumpwm-eval (format nil "(let ((w (find ~d (all-windows) :key (lambda (w) (xlib:window-id (window-xwin w))))))
                                   (when w (focus-all w) t))" id))
    (stumpwm-unreachable () nil)))

;;; --- Processes -----------------------------------------------------------------

(defun read-file-bytes (path)
  (ignore-errors
   (with-open-file (in path :element-type '(unsigned-byte 8))
     (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
       (loop for b = (read-byte in nil) while b do (vector-push-extend b bytes))
       bytes))))

(defun process-children ()
  "A table of each process's children, from /proc/*/stat."
  (let ((children (make-hash-table)))
    (dolist (name (ignore-errors (folder-names "/proc")) children)
      (let ((pid (parse-integer name :junk-allowed t)))
        (when (and pid (every #'digit-char-p name))
          (let* ((bytes (read-file-bytes (format nil "/proc/~d/stat" pid)))
                 (stat (and bytes (sb-ext:octets-to-string bytes :external-format :latin-1)))
                 ;; The command name, in parentheses, may hold spaces: the
                 ;; fields after it start at the last ).
                 (close (and stat (position #\) stat :from-end t)))
                 (fields (and close (split-on #\Space (subseq stat (+ close 2)))))
                 (ppid (and fields (parse-integer (second fields) :junk-allowed t))))
            (when ppid (push pid (gethash ppid children)))))))))

(defun process-tree (pid children)
  (cons pid (loop for child in (gethash pid children)
                  append (process-tree child children))))

(defun read-link (path)
  (ignore-errors (sb-posix:readlink path)))

(defun ordinary-path-p (path)
  (and path (plusp (length path)) (char= (char path 0) #\/)
       (notany (lambda (top) (or (string= path top) (path-inside-p path top)))
               '("/proc" "/sys" "/dev" "/run" "/tmp/.X11-unix"))
       (not (search " (deleted)" path))))

(defun process-open-files (pid)
  (let ((fd-folder (format nil "/proc/~d/fd" pid)))
    ;; Not 0, 1 and 2: those a program inherits (the session's log), they
    ;; aren't what it opened. Nor folders it holds open to watch them.
    (loop for fd in (ignore-errors (folder-names fd-folder))
          for target = (and (> (or (parse-integer fd :junk-allowed t) 0) 2)
                            (read-link (join-path fd-folder fd)))
          when (and (ordinary-path-p target) (not (directory-p target))) collect target)))

(defun process-arguments (pid cwd)
  "The arguments PID was started with that name a file or folder."
  (let ((bytes (read-file-bytes (format nil "/proc/~d/cmdline" pid))))
    (when bytes
      (let ((args (rest (split-on (code-char 0)
                                  (string-right-trim (string (code-char 0))
                                                     (sb-ext:octets-to-string bytes :external-format :utf-8))))))
        (loop for arg in args
              for path = (cond ((zerop (length arg)) nil)
                               ((char= (char arg 0) #\-) nil)
                               ((char= (char arg 0) #\/) (normalize-path arg))
                               (cwd (normalize-path (join-path cwd arg))))
              when (and path (ordinary-path-p path) (path-exists-p path)) collect path)))))

;;; --- Emacs ---------------------------------------------------------------------

(defun emacs-ask (elisp)
  "ELISP's value from the running Emacs (its server), read as data; NIL
when there's none. timeout keeps a busy Emacs from holding Esploro up."
  (ignore-errors
   (let ((out (with-output-to-string (s)
                (sb-ext:run-program "timeout" (list "2" "emacsclient" "-e" elisp)
                                    :search t :output s :error nil :wait t))))
     (and (plusp (length out)) (read-foreign out)))))

(defun emacs-buffers ()
  "Emacs's file buffers and its frames' windows: (VALUES ((FILE MODIFIED) ...) IDS)."
  (let ((answer (emacs-ask "(list (mapcar (lambda (b) (list (buffer-file-name b) (if (buffer-modified-p b) t nil)))
                                    (seq-filter (function buffer-file-name) (buffer-list)))
                            (delq nil (mapcar (lambda (f) (frame-parameter f 'outer-window-id)) (frame-list))))")))
    (values (first answer)
            (loop for id in (second answer)
                  for n = (and (stringp id) (parse-integer id :junk-allowed t))
                  when n collect n))))

(defun true-symbol-p (x)
  (and x (symbolp x) (string= (symbol-name x) "T")))

(defun where-text (places)
  "\"Emacs (unsaved), Alacritty on 2\": the windows that have a file."
  (format nil "~{~a~^, ~}"
          (remove-duplicates
           (loop for (window . how) in places
                 collect (format nil "~a~@[ on ~a~]~:[~; (unsaved)~]"
                                 (window-class window) (window-group window)
                                 (eq how :modified-buffer)))
           :test #'string=)))

;;; --- The map ---------------------------------------------------------------------

(defun scan-where (&key (windows (stumpwm-windows)))
  "Where each file is open: a table from a path to a list of (WINDOW . HOW),
HOW being :file (held open), :argument (started on it), :folder (a shell
or program working in it), :buffer or :modified-buffer (in Emacs)."
  (let ((map (make-hash-table :test 'equal))
        (children (process-children))
        (self (sb-posix:getpid)))
    (flet ((note (path window how)
             (let ((path (normalize-path path)))
               (when path
                 (pushnew (cons window how) (gethash path map) :test #'equal)))))
      (dolist (window windows)
        (let ((pid (window-pid window)))
          (when (and (integerp pid) (/= pid self))
            (dolist (p (process-tree pid children))
              (let ((cwd (read-link (format nil "/proc/~d/cwd" p))))
                (dolist (file (process-open-files p)) (note file window :file))
                (dolist (arg (process-arguments p cwd)) (note arg window :argument))
                ;; Every program has a folder; a folder means something only
                ;; for the window's children (the shell in a terminal), and
                ;; not when it's just home.
                (when (and cwd (/= p pid) (ordinary-path-p cwd)
                           (string/= (normalize-path cwd) (home-folder)))
                  (note cwd window :folder)))))))
      (multiple-value-bind (buffers frame-ids) (emacs-buffers)
        ;; An Emacs with no frame (a daemon) still has the file: a window
        ;; with no id stands for it.
        (let ((emacs-windows (or (remove-if-not (lambda (w) (member (window-id w) frame-ids)) windows)
                                 (remove-if-not (lambda (w) (equal (window-class w) "Emacs")) windows)
                                 (and buffers (list (make-window :class "Emacs" :title "no frame open"))))))
          (dolist (buffer buffers)
            (dolist (window emacs-windows)
              (note (first buffer) window
                    (if (true-symbol-p (second buffer)) :modified-buffer :buffer)))))))
    map))

(defun file-where (path map)
  "The (WINDOW . HOW) that have PATH, from MAP (SCAN-WHERE)."
  (gethash path map))

(defun window-files (id map)
  "The files window ID has, from MAP: ((PATH . HOW) ...)."
  (let ((files '()))
    (maphash (lambda (path places)
               (dolist (place places)
                 (when (eql (window-id (car place)) id)
                   (push (cons path (cdr place)) files))))
             map)
    (sort files #'string< :key #'car)))
