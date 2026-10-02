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
    ;; Names keep their capitals.
    (dolist (name '("Esploro" "Emacs" "PDF") words)
      (let ((at (search (string-downcase name) words)))
        (when at (setf words (concatenate 'string (subseq words 0 at) name
                                          (subseq words (+ at (length name))))))))))

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

(defun command-output (program &rest args)
  "What PROGRAM prints, first line, or NIL."
  (ignore-errors
   (let ((out (with-output-to-string (s)
                (sb-ext:run-program program args :search t :output s :error nil :wait t))))
     (let ((line (string-trim '(#\Space #\Newline) (subseq out 0 (position #\Newline out)))))
       (and (plusp (length line)) line)))))

(defun desktop-file (name)
  "Where the desktop entry NAME (\"nvim.desktop\") is, in the XDG folders."
  (let ((dirs (cons (join-path (env-folder "XDG_DATA_HOME" ".local/share") "applications")
                    (mapcar (lambda (d) (join-path d "applications"))
                            (remove "" (split-on #\: (or (sb-posix:getenv "XDG_DATA_DIRS")
                                                         "/usr/local/share:/usr/share"))
                                    :test #'string=)))))
    (loop for dir in dirs
          for file = (join-path dir name)
          when (path-exists-p file) return file)))

(defun desktop-entry (file)
  "FILE's Exec= and Terminal= from its [Desktop Entry]: (VALUES EXEC TERMINAL-P)."
  (with-open-file (in (native file) :external-format :utf-8)
    (let ((section nil) exec terminal)
      (loop for line = (read-line in nil) while line
            do (cond ((and (plusp (length line)) (char= (char line 0) #\[)) (setf section line))
                     ((string/= section "[Desktop Entry]"))
                     ((and (not exec) (> (length line) 5) (string= "Exec=" line :end2 5))
                      (setf exec (subseq line 5)))
                     ((and (> (length line) 9) (string= "Terminal=" line :end2 9))
                      (setf terminal (string-equal (subseq line 9) "true")))))
      (values exec terminal))))

(defun exec-arguments (exec path)
  "EXEC (a desktop entry's command) as a list, with PATH for %f %F %u %U and
the other field codes left out."
  (let ((args '()) (used nil))
    (dolist (word (remove "" (split-on #\Space exec) :test #'string=))
      (cond ((member word '("%f" "%F" "%u" "%U") :test #'string=) (push path args) (setf used t))
            ((and (= (length word) 2) (char= (char word 0) #\%)))
            (t (push (string-trim "\"" word) args))))
    (unless used (push path args))
    (nreverse args)))

(defun open-default (path)
  "Open PATH with its usual program. Text goes to Emacs when its server is
running. A program meant for a terminal (nvim, less) gets one: xdg-open,
outside a big desktop, would start it with no terminal, unseen."
  (cond ((and (kind-is (path-kind path) :text) (emacs-ask "t"))
         (let ((frames (nth-value 1 (emacs-buffers))))
           (cond (frames
                  (launch "emacsclient" "-n" path)
                  (focus-window (first frames)))
                 ;; A daemon with no frame: -n alone would open nothing to see.
                 (t (launch "emacsclient" "-c" "-n" path)))))
        (t
         (let* ((mime (command-output "xdg-mime" "query" "filetype" path))
                (entry (and mime (command-output "xdg-mime" "query" "default" mime)))
                (file (and entry (desktop-file entry))))
           (multiple-value-bind (exec terminal) (if file (desktop-entry file) (values nil nil))
             (if (and exec terminal)
                 (apply #'launch *terminal* "-e" (exec-arguments exec path))
                 (launch "xdg-open" path)))))))

(defun open-path (path &key (where (scan-where)))
  "Open PATH: when a window already has it (WHERE, from SCAN-WHERE, fresh by
default), go to that window instead of opening it again; otherwise its
usual program. Returns the window gone to, or NIL."
  (let ((places (file-where path where)))
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
          (t (open-default path) nil))))

;;; --- The first commands --------------------------------------------------------

(define-file-command open-in-emacs ((path (:text :folder)))
  "Open in Emacs (dired for a folder)."
  (launch "emacsclient" "-n" path))

(define-file-command terminal-here ((path :folder))
  "Open a terminal in this folder."
  (launch *terminal* "--working-directory" path))

(define-file-command open-with-default ((path :file))
  "Open with its usual program, even when a window has it already."
  (open-default path))

(define-file-command duplicate ((path t) :changes t)
  "Plan a copy beside it."
  (list (list :copy path (free-name path " copy"))))

(define-file-command trash ((path t) :changes t)
  "Plan putting it in the Trash."
  (list (list :trash path)))

;;; --- More commands: each makes something new beside, never over --------------------

(defun tool-ok (program &rest args)
  "Run PROGRAM with ARGS, waiting; true when it worked."
  (ignore-errors
   (eql 0 (sb-ext:process-exit-code
           (sb-ext:run-program program args :search t :input nil :output nil :error nil :wait t)))))

(define-file-command copy-path ((path t))
  "Copy its path, to paste anywhere."
  (with-input-from-string (in path)
    (sb-ext:run-program "xclip" (list "-selection" "clipboard") :search t :input in :wait t)))

(define-file-command show-in-esploro ((path t))
  "Show it in Esploro, selected in its folder."
  (launch "esploro" path))

(define-file-command extract-here ((path :archive))
  "Extract it into a new folder beside it."
  (let* ((name (path-name path))
         (stem (subseq name 0 (or (search ".tar" name) (position #\. name :from-end t) (length name))))
         ;; Numbered on the whole name: a folder has no extension.
         (to (loop for n from 1
                   for candidate = (join-path (path-parent path) (if (= n 1) stem (format nil "~a ~d" stem n)))
                   unless (path-exists-p candidate) return candidate)))
    (ensure-folder to)
    (or (tool-ok "bsdtar" "-xf" path "-C" to)
        (tool-ok "tar" "-xf" path "-C" to)
        (and (string-equal (pathname-type (native path)) "zip") (tool-ok "unzip" "-q" path "-d" to)))))

(define-file-command compress ((path t))
  "Compress it into a .zip beside it."
  (let ((to (free-name (concatenate 'string path ".zip") "")))
    (tool-ok "sh" "-c" "cd \"$1\" && exec zip -qr \"$2\" \"$3\"" "sh" (path-parent path) to (path-name path))))

(define-file-command shrink ((path :image))
  "A copy at half the size beside it (\"photo small.jpg\")."
  (tool-ok "magick" path "-auto-orient" "-resize" "50%" (free-name path " small")))

;;; --- Your own commands ---------------------------------------------------------------

(defun load-user-commands ()
  "~/.config/esploro/commands.lisp: your define-file-command forms, read in
Esploro's package. A mistake there is said, and the rest goes on."
  (let ((file (join-path (env-folder "XDG_CONFIG_HOME" ".config") "esploro" "commands.lisp")))
    (when (path-exists-p file)
      (handler-case (let ((*package* (find-package '#:esploro))) (load (native file)) t)
        (error (e) (format *error-output* "esploro: ~a: ~a~%" file e) nil)))))
