;;;; commands.lisp — a command is defined once, for a kind of file.
;;;;
;;;;   (define-file-command open-in-emacs ((path :text))
;;;;     "Open in Emacs."
;;;;     (launch "emacsclient" "-n" path))
;;;;
;;;;   (define-file-command duplicate ((path :file) :changes t)
;;;;     "Make a copy beside it."
;;;;     (list (list :copy path (free-name path " copy"))))
;;;;
;;;; A command either acts at once (open, show) or, with :changes t,
;;;; returns steps for the plan instead of touching files. The window's
;;;; menu offers each command for the kinds it names; the same registry is
;;;; for the other doors to come (rofi, Emacs, StumpWM keys, agents).

(in-package #:esploro)

(defstruct file-command
  name kinds doc function changes)

(defvar *file-commands* '()
  "Every file command, in the order they were defined.")

(defun file-command-label (command)
  "The command's name for people: open-in-emacs is \"Open in emacs\"."
  (let ((words (substitute #\Space #\- (string-downcase (symbol-name (file-command-name command))))))
    (setf (char words 0) (char-upcase (char words 0)))
    words))

(defun register-file-command (command)
  (let ((old (position (file-command-name command) *file-commands* :key #'file-command-name)))
    (if old
        (setf (nth old *file-commands*) command)
        (setf *file-commands* (append *file-commands* (list command))))
    command))

(defmacro define-file-command (name ((var kinds) &rest options) &body body)
  "Define the file command NAME for files of KINDS (a kind, a list of them,
or T for anything). OPTIONS: :changes T when BODY returns plan steps
rather than acting. BODY may start with a docstring, shown in menus."
  (let ((doc (when (and (stringp (first body)) (rest body)) (first body))))
    `(progn
       (register-file-command
        (make-file-command :name ',name
                           :kinds ',(if (listp kinds) kinds (list kinds))
                           :doc ,doc
                           :changes ,(getf options :changes)
                           :function (lambda (,var) ,@body)))
       ',name)))

(defun find-file-command (name)
  (find name *file-commands* :key #'file-command-name
                             :test (lambda (a b) (string-equal (string a) (string b)))))

(defun applies-p (command kind)
  (some (lambda (wanted) (kind-is kind wanted)) (file-command-kinds command)))

(defun commands-for (paths)
  "The commands that apply to every one of PATHS."
  (let ((kinds (remove-duplicates (mapcar #'path-kind paths))))
    (remove-if-not (lambda (command) (every (lambda (kind) (applies-p command kind)) kinds))
                   *file-commands*)))

(defun run-file-command (command paths)
  "Run COMMAND (one, or its name) on each of PATHS. Returns the steps it
proposes, for a command that changes files; NIL for one that acted."
  (let ((command (if (file-command-p command) command (find-file-command command))))
    (unless command (error "No file command ~a" command))
    (if (file-command-changes command)
        (loop for path in paths append (funcall (file-command-function command) path))
        (progn (dolist (path paths) (funcall (file-command-function command) path))
               nil))))

;;; --- Starting programs ---------------------------------------------------------

(defun launch (program &rest args)
  "Start PROGRAM on its own: setsid -f leaves it to init, so it outlives
Esploro and leaves no process to wait for."
  (sb-ext:run-program "setsid" (list* "-f" program args)
                      :search t :input nil :output nil :error nil :wait t)
  t)

(defun free-name (path suffix)
  "A path beside PATH that's free: \"notes copy.org\", \"notes copy 2.org\"..."
  (let* ((name (path-name path))
         (dot (position #\. name :from-end t))
         (dot (and dot (plusp dot) dot))
         (stem (subseq name 0 dot))
         (type (if dot (subseq name dot) "")))
    (loop for n from 1
          for candidate = (join-path (path-parent path)
                                     (format nil "~a~a~:[ ~d~;~*~]~a" stem suffix (= n 1) n type))
          unless (path-exists-p candidate) return candidate)))

(defparameter *terminal* (or (sb-posix:getenv "TERMINAL") "alacritty"))

(defun open-path (path &key where)
  "Open PATH: when a window already has it (WHERE, from SCAN-WHERE), go to
that window instead of opening it again; otherwise its usual program."
  (let ((places (and where (file-where path where))))
    (cond (places
           (let ((place (or (find-if (lambda (p) (member (cdr p) '(:buffer :modified-buffer))) places)
                            (first places))))
             (cond ((null (window-id (car place)))
                    ;; Emacs without a frame: a new frame, on the buffer.
                    (launch "emacsclient" "-c" "-n" path))
                   (t
                    (when (member (cdr place) '(:buffer :modified-buffer))
                      (emacs-ask (format nil "(let ((b (find-buffer-visiting ~a))) (when b (switch-to-buffer b)) nil)"
                                         (lisp-string path))))
                    (focus-window (window-id (car place)))))
             (car place)))
          (t (launch "xdg-open" path) nil))))

;;; --- The first commands --------------------------------------------------------

(define-file-command open-in-emacs ((path (:text :folder)))
  "Open in Emacs (dired for a folder)."
  (launch "emacsclient" "-n" path))

(define-file-command terminal-here ((path :folder))
  "Open a terminal in this folder."
  (launch *terminal* "--working-directory" path))

(define-file-command open-with-default ((path :file))
  "Open with its usual program, even when a window has it already."
  (launch "xdg-open" path))

(define-file-command duplicate ((path t) :changes t)
  "Plan a copy beside it."
  (list (list :copy path (free-name path " copy"))))

(define-file-command trash ((path t) :changes t)
  "Plan putting it in the Trash."
  (list (list :trash path)))
