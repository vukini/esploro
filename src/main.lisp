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
esploro recipe add NAME RECIPE   keep RECIPE, an s-expression, by name:
                            (:rename-by \"IMG_*\" \"Holiday #n\"), (:move-into \"/folder\")...
esploro rename-by [--plan] FROM TO FILE...   rename FILE... by a pattern, as a plan
                            for your review: * and ? in FROM take parts of a name,
                            #1, #2... in TO give them back, #n numbers them, ## is #;
                            FROM without * or ? is text to replace. Never two files
                            to one name, nor onto a name that's taken
esploro sort-by-kind [--plan] FILE...   FILE... into folders by kind, beside them
                            (Images, Videos, Audio, Documents, Text, Archives), as a
                            plan for your review; your own kinds and folders, as a
                            recipe: (:sort-by-kind (:image \"Pictures\") (:pdf \"/x/Docs\"))
esploro query [--lines] TEXT [FOLDER]   the files below FOLDER (the one you're in)
                            that match TEXT: words a name holds, *.pdf, kind:pdf,
                            newer:7 or older:30 (days), larger:10M, smaller:1k,
                            -word for not; or a query as an s-expression
esploro search list|save NAME TEXT [FOLDER]|forget NAME|run [--lines] NAME
                            searches kept by name (Searches, down the side)
esploro learn PROPOSED APPLIED   what you changed in an agent's plan, as rules in words
                            (kept in the state folder's corrections.lisp)
esploro learn --add RULE    RULE into ~/.config/esploro/sorting.md, under its learnt rules
esploro habits [--new]      what you keep doing (several plans moving like files into
                            one folder), each with a rule, a recipe and a search for
                            more like them; --new: only those not said before
esploro habits --dismiss KEY   that one, never offered again
esploro archive open PATH  PATH (a zip, a tarball, 7z, an ISO) opened read-only, like a
                            folder (archivemount): where it is
esploro archive close POINT | list   closing one; the ones open
esploro remote open SERVER  connect to user@host:folder (sshfs): where it is, like a folder
esploro remote close POINT | list | known   disconnect; those connected; those you've used
esploro tag add|remove NAME FILE...   a tag of yours on files (kept on the file itself,
                            user.xdg.tags; a plan, so undo takes it back); find them
                            with tag:NAME in a query
esploro tags [in FOLDER | of FILE... | forget NAME]   the tags you've used; those of a
                            folder's entries, or of files; one no longer offered
esploro phone [mount [ID] | unmount]   the iPhones plugged in, and which is mounted
                            (at ~/iphone, with ifuse); mounting one, unmounting it
esploro changes [--lines [N]]   every change Esploro made, newest first, in words
                            (--lines: as text, the N newest, each with its steps)
esploro changes undo ID     undo that one (checked whole first), not only the last
esploro duplicates [--plan] FOLDER   the files below that are the same, byte for byte
                            (1 KB or more, not another repository's); --plan: the
                            copies to the Trash, the oldest kept, for your review
esploro sizes [--lines] FOLDER   its entries by the space they take (du), biggest first
esploro sizes --below [--lines] FOLDER   the biggest things anywhere below it, each
                            taking a hundredth of it or more: a file, a folder of
                            smaller things, or the smaller things beside those in
                            a folder; no byte is counted twice
esploro sizes --trash       what the Trash takes, and how many things are in it
esploro recent [--lines]    the files opened lately, newest first: those opened through
                            Esploro, and GTK programs' (recently-used.xbel)
esploro workspaces          StumpWM's workspaces, each with the project its windows are about
esploro open-on N PATH...   go to workspace N and open PATH there (a folder in Esploro,
                            text in an Emacs frame there, the rest in its program)
esploro selection [--sexp]  the files selected in Esploro (or dired), the view used
                            last: one a line, for a shell or an agent
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
esploro project [PATH]      PATH's project (.git or log.md above it), and the windows
                            that have something in it
esploro focus ID            go to the window ID (StumpWM's)
esploro thumbnails [--size N] PATH...   small PNGs for the list (each made once)
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
         (*progress* (equal (sb-posix:getenv "ESPLORO_PROGRESS") "1"))
         (*steps-done* 0)
         (done (handler-case
                   (handler-bind ((step-failed
                                    (lambda (e)
                                      (setf failed (list (describe-step (step-failed-step e))
                                                         (step-failed-reason e)))
                                      (invoke-restart 'stop-here))))
                     (apply-plan steps))
                 (plan-refused (e) (answer (list :refused (plan-refused-problems e)))
                   (return-from cli-apply 1))
                 ;; Cancel (the window interrupts it): the step under way is
                 ;; taken back, what's done stays, journaled, so undo works.
                 (sb-sys:interactive-interrupt ()
                   (answer (list :cancelled *steps-done*))
                   (return-from cli-apply 130)))))
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
         (places (progn (note-opened path) (file-where path (scan-where))))
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

(defun keep-proposed (steps)
  "STEPS in a plan file of Esploro's own (the proposer's may go before you
decide), in the state folder's proposed/: its path."
  (let ((folder (join-path (state-folder) "proposed"))
        (stamp (remove #\: (timestamp (get-universal-time) "" "-"))))
    (loop for n from 1
          for kept = (join-path folder (format nil "~a~:[-~d~;~*~].lisp" stamp (= n 1) n))
          unless (path-exists-p kept) return (write-plan steps kept))))

(defun propose-steps (steps why &optional recipe)
  "STEPS for your review in Esploro (an agent's, or a command's or a
recipe's that changes files): checked whole first, then shown with WHY;
nothing changes here. RECIPE, when they're a recipe's: the review offers
to keep it by name."
  (let* ((problems (check-plan steps)))
    (cond (problems (answer (list :refused problems)) 1)
          (t
           (let* ((kept (keep-proposed steps))
                  (code-file (window-code))
                  (where (workspace-window))
                  (form (format nil "(progn (unless (featurep 'esploro) ~:[(require 'esploro)~;~:*(load ~a nil t)~]) (esploro-review-plan ~a ~a ~a~@[ '~a~]))"
                                (and code-file (lisp-string code-file)) (lisp-string kept)
                                (lisp-string (or why "")) (or where "nil")
                                (and recipe (recipe-text recipe)))))
             (if (zerop (sb-ext:process-exit-code
                         (sb-ext:run-program "emacsclient" (list "-n" "-e" form)
                                             :search t :input nil :output nil :error nil :wait t)))
                 (progn (answer (list :proposed (length steps))) 0)
                 (progn (answer (list :error "Esploro's window (Emacs's server) isn't answering")) 1)))))))

(defun recipe-text (recipe)
  "RECIPE as text, for Emacs and the recipes file alike."
  (with-standard-io-syntax
    (let ((*print-case* :downcase) (*print-readably* nil))
      (prin1-to-string recipe))))

(defun offer-plan (recipe paths plan-only)
  "RECIPE's plan for PATHS, for your review: shown in Esploro (through its
Emacs), or with PLAN-ONLY kept in a file and answered as (:plan FILE WHY
STEPS RECIPE), for the window to show itself. Refused with the problems
when it can't be done whole; (:none WHY) when there's nothing to do."
  (multiple-value-bind (steps problems why)
      (handler-case (recipe-plan recipe paths)
        (error (e) (answer (list :error (princ-to-string e)))
          (return-from offer-plan 1)))
    (cond (problems (answer (list :refused problems)) 1)
          ((null steps) (answer (list :none why)) 1)
          (plan-only (answer (list :plan (keep-proposed steps) why (length steps) recipe)) 0)
          (t (propose-steps steps why recipe)))))

(defun cli-rename-by (args)
  "esploro rename-by [--plan] FROM TO FILE..."
  (let* ((plan-only (equal (first args) "--plan"))
         (args (if plan-only (rest args) args)))
    (if (< (length args) 2)
        (progn (answer (list :error "esploro rename-by [--plan] FROM TO FILE...")) 2)
        (offer-plan (list :rename-by (first args) (second args)) (mapcar #'absolute (cddr args)) plan-only))))

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
          ((file-command-makes command)
           (handler-case (progn (answer (list :done (length paths) :made (run-file-command command paths))) 0)
             (error (e) (answer (list :error (princ-to-string e))) 1)))
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
           ;; :review after the ones whose plan waits for your review.
           (answer (mapcar (lambda (r) (list* (car r) (describe-recipe (cdr r))
                                              (and (reviewed-recipe-p (cdr r)) (list :review))))
                           (read-recipes)))
           0)
          ((equal what "add")
           (destructuring-bind (&optional name text) (rest args)
             (handler-case
                 (let ((recipe (parse-recipe (or text ""))))
                   (cond ((or (null name) (string= name "")) (answer (list :error "a recipe needs a name")) 1)
                         (t (save-recipe name recipe)
                            (answer (list :saved name (describe-recipe recipe))) 0)))
               (error (e) (answer (list :error (princ-to-string e))) 1))))
          ((equal what "save")
           (let ((recipe (plan-recipe (last-applied-steps))))
             (cond ((or (null (second args)) (string= (second args) "")) (answer (list :error "a recipe needs a name")) 1)
                   ((null recipe) (answer (list :none "the last change isn't one thing into one folder")) 1)
                   (t (save-recipe (second args) recipe)
                      (answer (list :saved (second args) (describe-recipe recipe))) 0))))
          ((equal what "forget") (forget-recipe (second args)) (answer (list :forgotten (second args))) 0)
          ((equal what "run")
           (let* ((plan-only (equal (second args) "--plan"))
                  (args (if plan-only (rest args) args))
                  (name (second args))
                  (recipe (if (equal name "last") (plan-recipe (last-applied-steps))
                              (cdr (assoc name (read-recipes) :test #'string=)))))
             (cond
               ((null recipe) (answer (list :none (format nil "no recipe ~a" name))) 1)
               ;; Each file its own way: the plan waits for your review.
               ((reviewed-recipe-p recipe) (offer-plan recipe (mapcar #'absolute (cddr args)) plan-only))
               (t
                 (with-input-from-string (*standard-input*
                                          (with-output-to-string (out)
                                            (with-standard-io-syntax
                                              (let ((*print-case* :downcase))
                                                (dolist (s (recipe-steps recipe (mapcar #'absolute (cddr args))))
                                                  (prin1 s out) (terpri out))))))
                   (cli-apply "-"))))))
          (t (answer (list :error "esploro recipe last | list | save NAME | add NAME RECIPE | forget NAME | run [--plan] NAME FILE...")) 2))))

(defun cli-sort-by-kind (args)
  "esploro sort-by-kind [--plan] FILE...: into Images, Documents... beside them."
  (let* ((plan-only (equal (first args) "--plan"))
         (args (if plan-only (rest args) args)))
    (if (null args)
        (progn (answer (list :error "esploro sort-by-kind [--plan] FILE...")) 2)
        (offer-plan (cons :sort-by-kind *kind-folders*) (mapcar #'absolute args) plan-only))))

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

(defun cli-selection (args)
  "esploro selection [--sexp]: what's selected in Esploro (or dired), used last."
  (multiple-value-bind (files folder) (emacs-selection)
    (cond ((equal (first args) "--sexp")
           (answer (if folder (list :selection folder files) (list :none "no Esploro or dired open in Emacs"))))
          (t (handler-case (progn (format t "~{~a~%~}" files) (finish-output))
               (stream-error () nil))))
    (if files 0 1)))

(defun cli-project (args)
  "esploro project [PATH]: PATH's project, and the windows that have something in it."
  (let* ((path (absolute (or (first args) ".")))
         (root (and path (project-root path))))
    (if root
        (progn (answer (list :project root (project-windows root))) 0)
        (progn (answer (list :none (format nil "~a isn't in a project (a folder with .git or log.md)"
                                           (short-path path))))
               1))))

(defun cli-learn (args)
  "esploro learn PROPOSED APPLIED | learn --add RULE"
  (cond ((equal (first args) "--add")
         (if (and (second args) (string/= (string-trim " " (second args)) ""))
             (progn (answer (list :added (add-sorting-rule (second args)))) 0)
             (progn (answer (list :error "esploro learn --add RULE")) 2)))
        ((and (first args) (second args))
         (let ((corrections (plan-corrections (read-plan-file (absolute (first args)))
                                              (read-plan-file (absolute (second args))))))
           (keep-corrections corrections)
           (answer (list :corrections (mapcar #'correction-rule corrections)))
           0))
        (t (answer (list :error "esploro learn PROPOSED APPLIED | learn --add RULE")) 2)))

(defun cli-habits (args)
  "esploro habits [--new] | habits --dismiss KEY"
  (cond ((equal (first args) "--dismiss")
         (if (second args)
             (progn (note-habit :dismissed (second args)) (answer (list :dismissed (second args))) 0)
             (progn (answer (list :error "esploro habits --dismiss KEY")) 2)))
        (t (let* ((new (equal (first args) "--new"))
                  (told (habits-seen))
                  (habits (if new
                              (remove-if (lambda (h) (member (cons :told (habit-key h)) told :test #'equal)) (habits))
                              (habits))))
             (when new (dolist (h habits) (note-habit :told (habit-key h))))
             (answer (mapcar #'habit-answer habits))
             0))))

(defun cli-archive (args)
  "esploro archive open PATH | close MOUNTPOINT | list"
  (let ((what (first args)) (path (and (second args) (absolute (second args)))))
    (cond ((and (equal what "open") path)
           (handler-case (progn (answer (list :archive (open-archive path) path)) 0)
             (error (e) (answer (list :error (princ-to-string e))) 1)))
          ((and (equal what "close") path)
           (if (close-archive path)
               (progn (answer (list :closed path)) 0)
               (progn (answer (list :busy path)) 1)))
          ((equal what "list") (answer (open-archives)) 0)
          (t (answer (list :error "esploro archive open PATH | close MOUNTPOINT | list")) 2))))

(defun cli-thumbnails (args)
  "esploro thumbnails [--size N] PATH...: small PNGs for a list, each made once."
  (let* ((size (if (equal (first args) "--size")
                   (or (parse-integer (or (second args) "") :junk-allowed t) 96)
                   96))
         (paths (if (equal (first args) "--size") (cddr args) args))
         (size (max 16 (min size 1024))))
    (answer (loop for p in paths
                  for path = (absolute p)
                  collect (cons path (and path (path-exists-p path) (ignore-errors (thumbnail path :size size))))))
    0))

(defun cli-workspaces ()
  (answer (loop for (number name count folder current) in (workspaces)
                collect (list number name count (and folder (short-path folder)) (and current t))))
  0)

(defun cli-open-on (number paths)
  "Go to workspace NUMBER and open PATHS there: a folder in an Esploro window,
text in an Emacs frame there (a new one when it has none), anything else in
its usual program."
  (let ((n (and number (parse-integer number :junk-allowed t)))
        (paths (remove nil (mapcar #'absolute paths))))
    (cond ((or (null n) (null paths)) (answer (list :error "esploro open-on NUMBER PATH...")) 2)
          ((not (go-to-workspace n)) (answer (list :error (format nil "there's no workspace ~a" number))) 1)
          (t
           (let* ((windows (stumpwm-windows))
                  (frame (emacs-frame-on (princ-to-string n) windows)))
             (dolist (path paths)
               (unless (directory-p path) (note-opened path))
               (cond ((directory-p path) (cli-show path))
                     ((kind-is (path-kind path) :text)
                      (if (and frame (emacs-ask (format nil "(let ((f (seq-find (lambda (f) (equal (frame-parameter f 'outer-window-id) ~s)) (frame-list))))
                                                               (when f (with-selected-frame f (find-file ~a)) t))"
                                                        (princ-to-string frame) (lisp-string path))))
                          (focus-window frame)
                          (launch "emacsclient" "-c" "-n" "-a" "" path)))
                     (t (open-default path)))))
           (answer (list :opened n (length paths)))
           0))))

(defun cli-recent (args)
  "esploro recent [--lines]: the files opened lately, newest first."
  (let ((files (recent-files)))
    (if (equal (first args) "--lines")
        (handler-case (progn (format t "~{~a~%~}" files) (finish-output)) (stream-error () nil))
        (answer (list :recent files)))
    0))

(defun cli-sizes (args)
  "esploro sizes [--below] [--lines] FOLDER: its entries by the space they take,
biggest first, or the biggest things anywhere below it; --trash: the Trash's."
  (let* ((below (and (member "--below" args :test #'string=) t))
         (lines (and (member "--lines" args :test #'string=) t))
         (words (remove-if (lambda (a) (member a '("--below" "--lines") :test #'string=)) args))
         (folder (absolute (or (first words) "."))))
    (cond ((equal (first words) "--trash")
           (answer (list :trash (trash-size) (length (trash-entries))))
           0)
          ((not (directory-p folder))
           (answer (list :error (format nil "~a isn't a folder" folder)))
           1)
          (t
           (let* ((sizes (if below (biggest-below folder) (folder-sizes folder)))
                  ;; (PATH SHOWN WHOLE), or (NAME . BYTES): as bytes and a whole path.
                  (rows (mapcar (lambda (entry)
                                  (if below
                                      (cons (second entry) (first entry))
                                      (cons (cdr entry) (join-path folder (car entry)))))
                                (second sizes))))
             (cond ((null sizes) (answer (list :error "du couldn't measure it")) 1)
                   (lines
                    (handler-case (progn (format t "~:{~d~c~a~%~}" (mapcar (lambda (row) (list (car row) #\Tab (cdr row))) rows))
                                         (finish-output))
                      (stream-error () nil))
                    0)
                   (t (answer (list* (if below :biggest :sizes) folder sizes)) 0)))))))

(defun cli-duplicates (args)
  "esploro duplicates [--plan] FOLDER: the files below FOLDER that are the
same, byte for byte; with --plan, the copies to the Trash, as a plan to review."
  (let* ((plan-only (equal (first args) "--plan"))
         (folder (absolute (or (if plan-only (second args) (first args)) "."))))
    (if (not (directory-p folder))
        (progn (answer (list :error (format nil "~a isn't a folder" folder))) 1)
        (let ((groups (find-duplicates folder)))
          (cond ((not plan-only) (answer (list :duplicates folder groups)) 0)
                ((null groups) (answer (list :none "no two files there are the same")) 1)
                (t (let* ((steps (duplicates-plan groups))
                          (wasted (reduce #'+ (mapcar (lambda (g) (* (first g) (length (cddr g)))) groups)))
                          (why (format nil "~d ~:*~[copies~;copy~:;copies~] of ~d ~:*~[files~;file~:;files~], the same byte for byte, to the Trash (~a back); the oldest of each kept"
                                       (length steps) (length groups) (size-words-short wasted))))
                     (answer (list :plan (keep-proposed steps) why (length steps) :duplicates))
                     0)))))))

(defun size-words-short (bytes)
  (loop for (unit . scale) in '(("GB" . 1073741824) ("MB" . 1048576) ("KB" . 1024))
        when (>= bytes scale) do (return (format nil "~,1f ~a" (/ bytes scale) unit))
        finally (return (format nil "~d bytes" bytes))))

(defun cli-changes (args)
  "esploro changes | changes undo ID"
  (if (equal (first args) "undo")
      (handler-case
          (let ((steps (undo-entry (or (second args) ""))))
            (answer (if steps (list :undone (mapcar #'describe-step steps))
                        (list :error "no such change, or it's undone already")))
            (if steps 0 1))
        (plan-refused (e) (answer (list :refused (plan-refused-problems e))) 1)
        (step-failed (e) (answer (list :failed (describe-step (step-failed-step e)) (step-failed-reason e))) 1))
      (if (equal (first args) "--lines")
          ;; As text, for a shell or an agent: a change a line (its time, what it
          ;; did, undone or not), then its steps, each indented.
          (let ((limit (or (and (second args) (parse-integer (second args) :junk-allowed t)) 30)))
            (handler-case
                (progn
                  (loop for (nil time summary undone steps) in (changes :limit limit)
                        do (format t "~a  ~a~:[~;  (undone)~]~%~{    ~a~%~}"
                                   (substitute #\Space #\T time) summary undone steps))
                  (finish-output))
              (stream-error () nil))
            0)
          (progn (answer (changes)) 0))))

(defun cli-remote (args)
  "esploro remote open SERVER | close POINT | list | known"
  (let ((what (first args)))
    (cond ((and (equal what "open") (second args))
           (handler-case (progn (answer (list :remote (open-remote (second args)) (second args))) 0)
             (error (e) (answer (list :error (princ-to-string e))) 1)))
          ((and (equal what "close") (second args))
           (if (close-remote (absolute (second args)))
               (progn (answer (list :closed (second args))) 0)
               (progn (answer (list :busy (second args))) 1)))
          ((equal what "list") (answer (open-remotes)) 0)
          ((equal what "known") (answer (known-servers)) 0)
          (t (answer (list :error "esploro remote open SERVER | close POINT | list | known")) 2))))

(defun cli-phone (args)
  "esploro phone | phone mount [ID] | phone unmount"
  (cond ((equal (first args) "mount")
         (handler-case (progn (answer (list :mounted (mount-phone (second args)))) 0)
           (error (e) (answer (list :error (princ-to-string e))) 1)))
        ((equal (first args) "unmount")
         (if (unmount-phone)
             (progn (answer (list :unmounted (phone-folder))) 0)
             (progn (answer (list :busy (phone-folder))) 1)))
        (t (answer (phone-status)) 0)))

(defun cli-tags (args)
  "esploro tags | tags in FOLDER | tags of FILE... | tags forget NAME"
  (let ((what (first args)))
    (cond ((null what) (answer (known-tags)) 0)
          ((and (equal what "in") (second args))
           (let ((found (folder-tags (absolute (second args)))))
             (remember-tags (loop for f in found append (rest f)))
             (answer found) 0))
          ((equal what "of")
           (answer (loop for p in (rest args)
                         for path = (absolute p)
                         for tags = (and path (file-tags path))
                         when tags collect (cons path tags)))
           0)
          ((and (equal what "forget") (second args))
           (forget-tag (second args)) (answer (list :forgotten (second args))) 0)
          (t (answer (list :error "esploro tags | tags in FOLDER | tags of FILE... | tags forget NAME")) 2))))

(defun cli-tag (args)
  "esploro tag add|remove NAME FILE...: a plan, applied (journaled: undo takes it back)."
  (destructuring-bind (&optional how tag &rest files) args
    (let ((how (cond ((equal how "add") :add) ((equal how "remove") :remove))))
      (if (or (null how) (null tag) (null files))
          (progn (answer (list :error "esploro tag add|remove NAME FILE...")) 2)
          (handler-case
              (let* ((paths (remove nil (mapcar #'absolute files)))
                     (steps (tag-steps how tag paths)))
                (when (eq how :add) (remember-tags (parse-tags tag)))
                (if (null steps)
                    (progn (answer (list :done 0)) 0)
                    (with-input-from-string (*standard-input*
                                             (with-output-to-string (out)
                                               (with-standard-io-syntax
                                                 (let ((*print-case* :downcase))
                                                   (dolist (s steps) (prin1 s out) (terpri out))))))
                      (cli-apply "-"))))
            (error (e) (answer (list :error (princ-to-string e))) 1))))))

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
          ((equal command "rename-by") (cli-rename-by (rest args)))
          ((equal command "sort-by-kind") (cli-sort-by-kind (rest args)))
          ((equal command "query") (cli-query (rest args)))
          ((equal command "search") (cli-search (rest args)))
          ((equal command "selection") (cli-selection (rest args)))
          ((equal command "learn") (cli-learn (rest args)))
          ((equal command "habits") (cli-habits (rest args)))
          ((equal command "archive") (cli-archive (rest args)))
          ((equal command "workspaces") (cli-workspaces))
          ((equal command "recent") (cli-recent (rest args)))
          ((equal command "sizes") (cli-sizes (rest args)))
          ((equal command "duplicates") (cli-duplicates (rest args)))
          ((equal command "changes") (cli-changes (rest args)))
          ((equal command "remote") (cli-remote (rest args)))
          ((equal command "phone") (cli-phone (rest args)))
          ((equal command "tags") (cli-tags (rest args)))
          ((equal command "tag") (cli-tag (rest args)))
          ((equal command "open-on") (cli-open-on (second args) (cddr args)))
          ((equal command "thumbnails") (cli-thumbnails (rest args)))
          ((equal command "project") (cli-project (rest args)))
          ((equal command "focus")
           (let ((id (and (second args) (parse-integer (second args) :junk-allowed t))))
             (if id (progn (focus-window id) (answer (list :focused id)) 0)
                 (progn (answer (list :error "esploro focus WINDOW-ID")) 2))))
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
  ;; Run by ssh as its SSH_ASKPASS: ask, answer, nothing else.
  (when (equal (sb-posix:getenv "ESPLORO_ASKPASS") "1")
    (askpass (or (second sb-ext:*posix-argv*) "Passphrase"))
    (sb-ext:exit :code 0))
  (sb-ext:exit
   :code (handler-case (or (main-1 (rest sb-ext:*posix-argv*)) 0)
           (sb-sys:interactive-interrupt () 130)
           (error (e)
             (format *error-output* "esploro: ~a~%" e)
             1))))
