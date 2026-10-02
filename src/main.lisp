;;;; main.lisp — the esploro command.
;;;;
;;;; The window is in Emacs (emacs/esploro.el); this is the core it calls
;;;; for anything that changes files, so every change is checked whole
;;;; first, journaled, and can be undone. Answers meant for Emacs are one
;;;; s-expression on standard output, which Emacs's `read' takes as it is.

(in-package #:esploro)

(defparameter *usage* "esploro [FOLDER]            show FOLDER in the Esploro window on your workspace
                            (StumpWM's), or in a new one when there's none there
esploro --new [FOLDER]      a new Esploro window, wherever others are
esploro FILE                its folder, with FILE selected
esploro                     (no folder) the workspace's: the project its windows are in
esploro reveal [--print]    the file behind the focused window, selected in its folder
                            (--print: only say which)
esploro commands [--lines] FILE...   the file commands that suit them (yours too:
                            ~/.config/esploro/commands.lisp)
esploro run NAME FILE...    run one: at once, or as a plan for your review
esploro recipe last|list|save NAME|forget NAME|run NAME|last FILE...
                            a change done again: the last one, or one kept by name
esploro query [--lines] TEXT [FOLDER]   the files below FOLDER (the one you're in)
                            that match TEXT: words a name holds, *.pdf, kind:pdf,
                            newer:7 or older:30 (days), larger:10M, smaller:1k,
                            -word for not; or a query as an s-expression
esploro search list|save NAME TEXT [FOLDER]|forget NAME|run [--lines] NAME
                            searches kept by name (Searches, down the side)
esploro propose FILE [WHY]  a plan for you to review in Esploro (an agent's): checked
                            first; nothing happens until you choose Apply
esploro --dbus              the running Emacs answers \"Show in folder\" (FileManager1)
esploro --where [PATH...]   which windows have PATH open (or every file that's open)

For the window (each answers with one s-expression):
esploro apply [FILE]        apply the plan in FILE (or read from standard input)
esploro check FILE          whether the plan in FILE could be applied; changes nothing
esploro undo                undo the last applied plan
esploro where FOLDER        the files in FOLDER that a window has open
esploro open PATH           go to the window that has PATH, or open it
esploro preview PATH        a PNG of PATH (a picture, PDF or video), made once and kept
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

(defun workspace-window ()
  "Where the window should be, as Emacs's `esploro' takes it: the X window id
of the Esploro window on StumpWM's current workspace, `new' when that has
none, or NIL when StumpWM can't be asked (any Esploro window will do)."
  (handler-case
      (or (stumpwm-eval "(let ((w (find \"Esploro\" (group-windows (current-group))
                                         :key (function window-title) :test (function string=))))
                            (and w (xlib:window-id (window-xwin w))))")
          "'new")
    (stumpwm-unreachable () nil)))

(defun cli-show (folder &key new file)
  "Show FOLDER in an Esploro window, which is Emacs's: through its server,
loading the window's code first when Emacs hasn't it yet. NEW: a new window;
else the one on this workspace (or a new one there)."
  (let* ((code-file (window-code))
         (where (if new "'new" (workspace-window)))
         (form (format nil "(progn (unless (featurep 'esploro) ~:[(require 'esploro)~;~:*(load ~a nil t)~]) (esploro ~a ~a~@[ ~a~]))"
                       (and code-file (lisp-string code-file)) (lisp-string folder)
                       (or where "nil") (and file (lisp-string file))))
         (code (sb-ext:process-exit-code
               (sb-ext:run-program "emacsclient"
                                   (list "-n" "-e" form)
                                   :search t :input nil :output nil :error *error-output* :wait t))))
    (unless (zerop code)
      (format *error-output* "esploro: its window is in Emacs, and Emacs's server isn't answering (M-x server-start, or emacs --daemon)~%"))
    code))

(defun cli-propose (file why)
  "An agent's plan, FILE: checked whole now (a plan with problems goes back
to it, with them), then shown to you in Esploro, on your workspace, to apply
or not. Nothing changes here."
  (propose-steps (read-plan-file (absolute file)) why))

(defun propose-steps (steps why)
  "STEPS for your review in Esploro (an agent's, or a command's that changes
files): checked whole first, then shown with WHY; nothing changes here."
  (let* ((problems (check-plan steps)))
    (cond (problems (answer (list :refused problems)) 1)
          (t
           ;; Its own copy: the agent's file may go before you decide.
           (let* ((kept (join-path (state-folder) "proposed"
                                   (format nil "~a.lisp" (remove #\: (timestamp (get-universal-time) "" "-")))))
                  (code-file (window-code))
                  (where (workspace-window))
                  (form (format nil "(progn (unless (featurep 'esploro) ~:[(require 'esploro)~;~:*(load ~a nil t)~]) (esploro-review-plan ~a ~a ~a))"
                                (and code-file (lisp-string code-file)) (lisp-string kept)
                                (lisp-string (or why "")) (or where "nil"))))
             (write-plan steps kept)
             (if (zerop (sb-ext:process-exit-code
                         (sb-ext:run-program "emacsclient" (list "-n" "-e" form)
                                             :search t :input nil :output nil :error nil :wait t)))
                 (progn (answer (list :proposed (length steps))) 0)
                 (progn (answer (list :error "Esploro's window (Emacs's server) isn't answering")) 1)))))))

(defun cli-check (file)
  "Whether the plan in FILE could be applied now, changing nothing: (:ok N),
or (:refused PROBLEMS). For a plan you've edited, before you apply it."
  (let ((steps (handler-case (read-plan-file (absolute file))
                 (error (e) (answer (list :refused (list (format nil "it can't be read: ~a" e))))
                   (return-from cli-check 1)))))
    (let ((problems (check-plan steps)))
      (cond (problems (answer (list :refused problems)) 1)
            (t (answer (list :ok (length steps))) 0)))))

(defun cli-commands (paths lines)
  "The file commands that suit every one of PATHS: (NAME LABEL DOC CHANGES)
each, or with LINES one a line, NAME, a tab, LABEL: what it does (for rofi)."
  (load-user-commands)
  (let ((commands (commands-for (mapcar #'absolute paths))))
    (if lines
        (dolist (c commands)
          (format t "~(~a~)~c~a~@[: ~a~]~%" (file-command-name c) #\Tab (file-command-label c)
                  (file-command-doc c)))
        (answer (mapcar (lambda (c) (list (string-downcase (symbol-name (file-command-name c)))
                                          (file-command-label c) (or (file-command-doc c) "")
                                          (and (file-command-changes c) t)))
                        commands)))
    0))

(defun cli-run (name paths)
  "Run the file command NAME on PATHS: one that acts, at once; one that
changes files, its steps go to Esploro for your review."
  (load-user-commands)
  (let* ((paths (mapcar #'absolute paths))
         (command (find-file-command name)))
    (cond ((null command) (answer (list :error (format nil "no command ~a" name))) 1)
          ((not (every (lambda (p) (applies-p command (path-kind p))) paths))
           (answer (list :error (format nil "~a isn't for ~{~a~^, ~}" (file-command-label command)
                                        (mapcar #'path-name paths))))
           1)
          ((file-command-changes command)
           (propose-steps (run-file-command command paths) (file-command-label command)))
          (t (run-file-command command paths) (answer (list :done (length paths))) 0))))

(defun cli-recipe (args)
  "esploro recipe last | list | save NAME | forget NAME | run NAME|last FILE..."
  (let ((what (first args)))
    (cond ((equal what "last")
           (let ((recipe (plan-recipe (last-applied-steps))))
             (answer (if recipe (list :recipe recipe (describe-recipe recipe))
                         (list :none "the last change isn't one thing into one folder (or there's none)")))
             (if recipe 0 1)))
          ((equal what "list")
           (answer (mapcar (lambda (r) (list (car r) (describe-recipe (cdr r)))) (read-recipes))) 0)
          ((equal what "save")
           (let ((recipe (plan-recipe (last-applied-steps))))
             (cond ((or (null (second args)) (string= (second args) "")) (answer (list :error "a recipe needs a name")) 1)
                   ((null recipe) (answer (list :none "the last change isn't one thing into one folder")) 1)
                   (t (save-recipe (second args) recipe)
                      (answer (list :saved (second args) (describe-recipe recipe))) 0))))
          ((equal what "forget") (forget-recipe (second args)) (answer (list :forgotten (second args))) 0)
          ((equal what "run")
           (let* ((name (second args))
                  (recipe (if (equal name "last") (plan-recipe (last-applied-steps))
                              (cdr (assoc name (read-recipes) :test #'string=)))))
             (if (null recipe)
                 (progn (answer (list :none (format nil "no recipe ~a" name))) 1)
                 (with-input-from-string (*standard-input*
                                          (with-output-to-string (out)
                                            (with-standard-io-syntax
                                              (let ((*print-case* :downcase))
                                                (dolist (s (recipe-steps recipe (mapcar #'absolute (cddr args))))
                                                  (prin1 s out) (terpri out))))))
                   (cli-apply "-")))))
          (t (answer (list :error "esploro recipe last | list | save NAME | forget NAME | run NAME FILE...")) 2))))

(defun answer-found (query root lines)
  (multiple-value-bind (paths more) (run-query query root)
    (if lines
        (handler-case (progn (format t "~{~a~%~}" paths) (finish-output))
          (stream-error () nil))
        (answer (list :found root (query-words query) paths more)))
    0))

(defun cli-query (args)
  "esploro query [--lines] TEXT [FOLDER]"
  (let* ((lines (equal (first args) "--lines"))
         (args (if lines (rest args) args))
         (root (absolute (or (second args) "."))))
    (handler-case
        (let ((query (parse-query (or (first args) ""))))
          (if (directory-p root) (answer-found query root lines)
              (progn (answer (list :error (format nil "~a isn't a folder" root))) 1)))
      (error (e) (answer (list :error (princ-to-string e))) 1))))

(defun cli-search (args)
  "esploro search list | save NAME TEXT [FOLDER] | forget NAME | run [--lines] NAME"
  (let ((what (first args)))
    (cond ((equal what "list")
           (answer (mapcar (lambda (s) (list (first s) (query-words (second s)) (third s))) (read-searches))) 0)
          ((equal what "save")
           (destructuring-bind (&optional name text folder) (rest args)
             (handler-case
                 (let ((query (parse-query (or text "")))
                       (root (absolute (or folder "."))))
                   (cond ((or (null name) (string= name "")) (answer (list :error "a search needs a name")) 1)
                         ((not (directory-p root)) (answer (list :error (format nil "~a isn't a folder" root))) 1)
                         (t (save-search name query root)
                            (answer (list :saved name (query-words query) root)) 0)))
               (error (e) (answer (list :error (princ-to-string e))) 1))))
          ((equal what "forget") (forget-search (second args)) (answer (list :forgotten (second args))) 0)
          ((equal what "run")
           (let* ((lines (equal (second args) "--lines"))
                  (name (if lines (third args) (second args)))
                  (search (find name (read-searches) :key #'first :test #'equal)))
             (if search (answer-found (second search) (third search) lines)
                 (progn (answer (list :none (format nil "no search ~a" name))) 1))))
          (t (answer (list :error "esploro search list | save NAME TEXT [FOLDER] | forget NAME | run NAME")) 2))))

(defun cli-dbus ()
  "Make the running Emacs answer org.freedesktop.FileManager1 (the browsers'
\"Show in folder\"): what the session bus runs when it's first asked."
  (let* ((code-file (window-code))
         (form (format nil "(progn (unless (featurep 'esploro) ~:[(require 'esploro)~;~:*(load ~a nil t)~]) (esploro-dbus-register))"
                       (and code-file (lisp-string code-file)))))
    (sb-ext:process-exit-code
     (sb-ext:run-program "emacsclient" (list "-e" form)
                         :search t :input nil :output nil :error *error-output* :wait t))))

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
          ((equal command "preview")
           (let* ((path (absolute (or (second args) "")))
                  (png (and path (path-exists-p path) (thumbnail path))))
             (answer (if png (list :thumbnail png) (list :none)))
             0))
          ((equal command "restore")
           (handler-case (progn (answer (list :done (length (restore-from-trash (rest args))))) 0)
             (plan-refused (e) (answer (list :refused (plan-refused-problems e))) 1)))
          ((equal command "empty-trash") (answer (list :emptied (empty-trash))) 0)
          ((equal command "--dbus") (cli-dbus))
          ((equal command "propose") (cli-propose (second args) (third args)))
          ((equal command "check") (cli-check (second args)))
          ((equal command "commands")
           (let ((lines (equal (second args) "--lines")))
             (cli-commands (if lines (cddr args) (rest args)) lines)))
          ((equal command "run") (cli-run (second args) (cddr args)))
          ((equal command "recipe") (cli-recipe (rest args)))
          ((equal command "query") (cli-query (rest args)))
          ((equal command "search") (cli-search (rest args)))
          ((and (equal command "reveal") (equal (second args) "--print"))
           ;; For a script (rofi's menu of commands): the file, or nothing.
           (let ((file (reveal-target)))
             (when file (format t "~a~%" file))
             (if file 0 1)))
          ((equal command "reveal")
           ;; Nothing behind it (a shell at home): the workspace's folder, or home.
           (let ((file (or (reveal-target) (ignore-errors (workspace-folder)) (home-folder))))
             (if (directory-p file) (cli-show file) (cli-show (path-parent file) :file file))))
          ((equal command "--new")
           (let ((folder (absolute (or (second args) "."))))
             (if (and folder (directory-p folder)) (cli-show folder :new t)
                 (progn (format *error-output* "esploro: ~a isn't a folder~%" (second args)) 2))))
          ((and command (plusp (length command)) (char= (char command 0) #\-))
           (format *error-output* "esploro: what's ~a?~%~a~%" command *usage*) 2)
          ((null command)
           ;; No folder named (Super+e): the workspace's, else the one you're in.
           (cli-show (or (ignore-errors (workspace-folder)) (current-folder))))
          (t
           (let ((folder (absolute (or command "."))))
             (cond ((and folder (directory-p folder)) (cli-show folder))
                   ;; A file: its folder, with the file selected.
                   ((and folder (path-exists-p folder)) (cli-show (path-parent folder) :file folder))
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
