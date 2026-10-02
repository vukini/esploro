;;;; main.lisp — the esploro command.
;;;;
;;;; The window is in Emacs (emacs/esploro.el); this is the core it calls
;;;; for anything that changes files, so every change is checked whole
;;;; first, journaled, and can be undone. Answers meant for Emacs are one
;;;; s-expression on standard output, which Emacs's `read' takes as it is.

(in-package #:esploro)

(defparameter *usage* "esploro [FOLDER]            show FOLDER in Esploro's window (in Emacs)
esploro --where [PATH...]   which windows have PATH open (or every file that's open)

For the window (each answers with one s-expression):
esploro apply [FILE]        apply the plan in FILE (or read from standard input)
esploro undo                undo the last applied plan
esploro where FOLDER        the files in FOLDER that a window has open
esploro open PATH           go to the window that has PATH, or open it
esploro trash-list          what's in the Trash
esploro restore NAME...     put these back from the Trash (a plan: undo puts them back)
esploro empty-trash         delete what's in the Trash, for good
esploro --help")

(defun current-folder ()
  (or (normalize-path (sb-posix:getcwd)) (home-folder)))

(defun absolute (path)
  (normalize-path (if (and (plusp (length path)) (char= (char path 0) #\/))
                      path
                      (join-path (current-folder) path))))

(defun answer (form)
  "FORM on standard output, for Emacs to read: keywords as :done, strings quoted."
  (with-standard-io-syntax
    (let ((*print-case* :downcase) (*print-readably* nil) (*package* (find-package '#:esploro.read)))
      (prin1 form)
      (terpri))))

(defun print-where (paths)
  (let ((map (scan-where)))
    (if paths
        (dolist (path paths)
          (let ((path (absolute path)))
            (format t "~a~:[  open nowhere~;~:*~{~%  ~a~}~]~%" path
                    (loop for (window . how) in (file-where path map)
                          collect (format nil "~a ~s~@[ on ~a~] (~(~a~))" (window-class window)
                                          (window-title window) (window-group window) how)))))
        (let ((paths '()))
          (maphash (lambda (path places) (push (cons path places) paths)) map)
          (loop for (path . places) in (sort paths #'string< :key #'car)
                do (format t "~a~40t ~a~%" path (where-text places)))))))

;;; --- What the window asks -----------------------------------------------------------

(defun cli-apply (source)
  "Apply a plan; a step that fails stops it there, keeping what's done."
  (let* ((steps (if (or (null source) (string= source "-"))
                    (read-forms *standard-input*)
                    (read-plan-file (absolute source))))
         (failed nil)
         (done (handler-case
                   (handler-bind ((step-failed
                                    (lambda (e)
                                      (setf failed (list (describe-step (step-failed-step e))
                                                         (step-failed-reason e)))
                                      (invoke-restart 'stop-here))))
                     (apply-plan steps))
                 (plan-refused (e) (answer (list :refused (plan-refused-problems e)))
                   (return-from cli-apply 1)))))
    (if failed
        (progn (answer (list :failed (first failed) (second failed) :done (length done))) 1)
        (progn (answer (list :done (length done))) 0))))

(defun cli-undo ()
  (handler-case
      (let ((steps (undo-last)))
        (answer (if steps
                    (list :undone (mapcar #'describe-step steps))
                    (list :nothing)))
        0)
    (plan-refused (e) (answer (list :refused (plan-refused-problems e))) 1)
    (step-failed (e) (answer (list :failed (describe-step (step-failed-step e)) (step-failed-reason e))) 1)))

(defun cli-where (folder)
  "((NAME . \"Emacs on 2 (unsaved)\") ...): the entries of FOLDER a window has."
  (let ((map (scan-where))
        (folder (absolute folder)))
    (answer (loop for name in (folder-names folder)
                  for places = (file-where (join-path folder name) map)
                  when places collect (cons name (where-text places))))
    0))

(defun cli-open (path)
  "Go to the window that has PATH, or open it. Text, and files Emacs already
has, are Emacs's to show (:emacs): the window is in Emacs, and knows which
of its frames to use."
  (let* ((path (absolute path))
         (places (file-where path (scan-where)))
         (elsewhere (find-if-not (lambda (p) (member (cdr p) '(:buffer :modified-buffer))) places)))
    (cond (elsewhere
           (focus-window (window-id (car elsewhere)))
           (answer (list :window (window-class (car elsewhere)))))
          ((or places (kind-is (path-kind path) :text))
           (answer (list :emacs)))
          (t (open-default path)
             (answer (list :opened))))
    0))

(defun window-code ()
  "The window's Emacs code beside this program (emacs/esploro.el, where the
repository is or Vikix builds it), or ESPLORO_EL; NIL when it's elsewhere,
and Emacs is to find it on its load-path."
  (let* ((program (ignore-errors (normalize-path (sb-ext:native-namestring
                                                  (truename sb-ext:*runtime-pathname*)))))
         (beside (and program (join-path (path-parent program) "emacs" "esploro.el"))))
    (or (sb-posix:getenv "ESPLORO_EL")
        (and beside (path-exists-p beside) beside))))

(defun cli-show (folder)
  "Show FOLDER in Esploro's window, which is in Emacs: through its server,
loading the window's code first when Emacs hasn't it yet."
  (let* ((code-file (window-code))
         (form (format nil "(progn (unless (featurep 'esploro) ~:[(require 'esploro)~;~:*(load ~a nil t)~]) (esploro ~a))"
                       (and code-file (lisp-string code-file)) (lisp-string folder)))
         (code (sb-ext:process-exit-code
               (sb-ext:run-program "emacsclient"
                                   (list "-n" "-e" form)
                                   :search t :input nil :output nil :error *error-output* :wait t))))
    (unless (zerop code)
      (format *error-output* "esploro: its window is in Emacs, and Emacs's server isn't answering (M-x server-start, or emacs --daemon)~%"))
    code))

(defun main-1 (args)
  (let ((command (first args)))
    (cond ((member command '("-h" "--help" "help") :test #'equal)
           (format t "~a~%" *usage*) 0)
          ((equal command "--where") (print-where (rest args)) 0)
          ((equal command "apply") (cli-apply (second args)))
          ((equal command "undo") (cli-undo))
          ((equal command "where") (cli-where (or (second args) ".")))
          ((equal command "open") (cli-open (second args)))
          ((equal command "trash-list") (answer (trash-entries)) 0)
          ((equal command "restore")
           (handler-case (progn (answer (list :done (length (restore-from-trash (rest args))))) 0)
             (plan-refused (e) (answer (list :refused (plan-refused-problems e))) 1)))
          ((equal command "empty-trash") (answer (list :emptied (empty-trash))) 0)
          ((and command (plusp (length command)) (char= (char command 0) #\-))
           (format *error-output* "esploro: what's ~a?~%~a~%" command *usage*) 2)
          (t
           (let ((folder (absolute (or command "."))))
             (cond ((and folder (directory-p folder)) (cli-show folder))
                   ((and folder (path-exists-p folder)) (cli-show (path-parent folder)))
                   (t (format *error-output* "esploro: ~a isn't a folder~%" command) 2)))))))

(defun main ()
  "The executable's start."
  (sb-ext:disable-debugger)
  (sb-ext:exit
   :code (handler-case (or (main-1 (rest sb-ext:*posix-argv*)) 0)
           (sb-sys:interactive-interrupt () 130)
           (error (e)
             (format *error-output* "esploro: ~a~%" e)
             1))))
