;;;; main.lisp — the esploro command.

(in-package #:esploro)

(defparameter *usage* "esploro [FOLDER]        the window, in FOLDER (or the folder you're in)
esploro --where [PATH]  which windows have PATH open (or every file that's open)
esploro --help")

(defun print-where (paths)
  (let ((map (scan-where)))
    (if paths
        (dolist (path paths)
          (let ((path (normalize-path (if (char= (char path 0) #\/) path (join-path (current-folder) path)))))
            (format t "~a~:[  open nowhere~;~:*~{~%  ~a~}~]~%" path
                    (loop for (window . how) in (file-where path map)
                          collect (format nil "~a ~s~@[ on ~a~] (~(~a~))" (window-class window)
                                          (window-title window) (window-group window) how)))))
        (let ((paths '()))
          (maphash (lambda (path places) (push (cons path places) paths)) map)
          (loop for (path . places) in (sort paths #'string< :key #'car)
                do (format t "~a~40t ~a~%" path (where-text places)))))))

(defun current-folder ()
  (or (normalize-path (sb-posix:getcwd)) (home-folder)))

(defun main ()
  "The executable's start: arguments, then the window or an answer."
  (sb-ext:disable-debugger)
  (handler-case (main-1 (rest sb-ext:*posix-argv*))
    (sb-sys:interactive-interrupt () (sb-ext:exit :code 130 :abort t))
    (error (e)
      (format *error-output* "esploro: ~a~%" e)
      (sb-ext:exit :code 1))))

(defun main-1 (args)
  (cond ((member (first args) '("-h" "--help") :test #'equal)
         (format t "~a~%" *usage*))
        ((equal (first args) "--where")
         (print-where (rest args)))
        ((and args (char= (char (first args) 0) #\-))
         (format *error-output* "esploro: what's ~a?~%~a~%" (first args) *usage*)
         (sb-ext:exit :code 2))
        (t
         (let ((folder (if args (normalize-path (if (char= (char (first args) 0) #\/)
                                                    (first args)
                                                    (join-path (current-folder) (first args))))
                           (current-folder))))
           (unless (and folder (directory-p folder))
             (format *error-output* "esploro: ~a isn't a folder~%" (first args))
             (sb-ext:exit :code 2))
           (run folder)))))
