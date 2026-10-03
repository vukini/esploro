;;;; plan.lisp — plan, then apply.
;;;;
;;;; Nothing is moved, copied, renamed or deleted at once. Each change is a
;;;; step in a plan, a plain list:
;;;;
;;;;   (:move "/from" "/to")      move (and rename) a file or folder
;;;;   (:copy "/from" "/to")      copy one, folders whole
;;;;   (:rename "/path" "name")   a new name in the same folder
;;;;   (:mkdir "/path")           a new folder
;;;;   (:trash "/path")           to the Trash (the freedesktop one)
;;;;
;;;; A plan can be looked at, edited as text, handed over by an agent, and
;;;; is checked whole before anything changes: any other step is refused,
;;;; whoever wrote it. Every applied plan is kept in a journal with its
;;;; inverse, which UNDO-LAST applies.

(in-package #:esploro)

(defparameter *plan-operations* '(:move :copy :rename :mkdir :trash)
  "What a plan may do. The journal's inverses also use :rmdir and :restore,
which only undo applies.")

(defparameter *undo-operations* (append *plan-operations* '(:rmdir :restore)))

;;; --- Where things are kept -------------------------------------------------

(defun env-folder (variable fallback)
  (or (normalize-path (sb-posix:getenv variable))
      (join-path (home-folder) fallback)))

(defun trash-folder ()
  (join-path (env-folder "XDG_DATA_HOME" ".local/share") "Trash"))

(defun state-folder ()
  (join-path (env-folder "XDG_STATE_HOME" ".local/state") "esploro"))

(defun ensure-folder (folder)
  (unless (directory-p folder)
    (ensure-folder (path-parent folder))
    (sb-posix:mkdir folder #o700))
  folder)

;;; --- Steps -----------------------------------------------------------------

(defun valid-name-p (name)
  (and (stringp name) (plusp (length name))
       (not (find #\/ name)) (not (member name '("." "..") :test #'string=))))

(defun valid-path-p (path)
  (and (stringp path) (equal (normalize-path path) path)))

(defun step-shape-problem (step allowed)
  "Why STEP isn't a step at all (its operation or its arguments), or NIL."
  (if (not (and (consp step) (keywordp (first step))))
      (format nil "~s isn't a step: a step is a list like (:move \"/from\" \"/to\")" step)
      (destructuring-bind (op &rest args) step
        (flet ((args-are (&rest checks)
                 (and (= (length args) (length checks))
                      (every #'funcall checks args))))
          (cond ((not (member op allowed))
                 (format nil "~(~s~) isn't something a plan can do (only ~{~(~s~)~^, ~})" op allowed))
                ((case op
                   ((:move :copy) (args-are #'valid-path-p #'valid-path-p))
                   (:rename (args-are #'valid-path-p #'valid-name-p))
                   ((:mkdir :trash :rmdir) (args-are #'valid-path-p))
                   (:restore (args-are #'valid-name-p #'valid-path-p)))
                 nil)
                (t (format nil "~(~s~) wasn't given the right things: ~s (paths are absolute, without . or ..)"
                           op step)))))))

(defun step-target (step)
  "The path a :rename step makes."
  (join-path (path-parent (second step)) (third step)))

(defun short-path (path &optional folder)
  "PATH as short as it can be said: from FOLDER when it's in it, else from ~."
  (let ((home (home-folder)))
    (cond ((and folder (path-inside-p path folder)) (subseq path (1+ (if (string= folder "/") 0 (length folder)))))
          ((string= path home) "~")
          ((path-inside-p path home) (concatenate 'string "~" (subseq path (length home))))
          (t path))))

(defun describe-step (step &optional folder)
  "STEP in a few words, for people; paths in FOLDER by their names alone."
  (flet ((s (path) (short-path path folder)))
    (destructuring-bind (op a &optional b) step
      (case op
        (:move (format nil "move ~a to ~a" (s a) (s b)))
        (:copy (format nil "copy ~a to ~a" (s a) (s b)))
        (:rename (format nil "rename ~a to ~a" (s a) b))
        (:mkdir (format nil "make the folder ~a" (s a)))
        (:trash (format nil "put ~a in the Trash" (s a)))
        (:rmdir (format nil "remove the empty folder ~a" (s a)))
        (:restore (format nil "bring ~a back from the Trash" (s b)))
        (t (format nil "~s" step))))))

;;; --- Checking a whole plan --------------------------------------------------
;;;
;;; The plan is played through on paper first: an overlay says what each
;;; step has done to the paths it touched (gone, now holding what a real
;;; path holds, a new empty folder), so step 3 is checked against the
;;; files as steps 1 and 2 will have left them.

(defun overlay-resolve (path overlay)
  "What PATH holds once the overlay's steps are done: (:real REAL-PATH),
:new-folder, or NIL for nothing."
  (let ((at path) (below '()))
    (loop
      (let ((state (gethash at overlay)))
        (cond ((eq state :gone) (return nil))
              ((eq state :new-folder) (return (if below nil :new-folder)))
              ((consp state)
               (let ((real (apply #'join-path (second state) below)))
                 (return (and (path-exists-p real) (list :real real)))))
              ((string= at "/")
               (return (and (path-exists-p path) (list :real path))))))
      (push (path-name at) below)
      (setf at (path-parent at)))))

(defun overlay-folder-p (path overlay)
  (let ((what (overlay-resolve path overlay)))
    (or (eq what :new-folder)
        (and (consp what) (directory-p (second what))))))

(defun check-step (step overlay)
  "The problem with STEP, given the overlay, or NIL after recording what
STEP does in it."
  (flet ((exists (p) (overlay-resolve p overlay))
         (folder (p) (overlay-folder-p p overlay)))
    (macrolet ((need (test &rest message)
                 `(unless ,test (return-from check-step (format nil ,@message)))))
      (destructuring-bind (op a &optional b) step
        (case op
          ((:move :copy :rename)
           (let ((to (if (eq op :rename) (step-target step) b)))
             (need (exists a) "~a isn't there" a)
             (need (not (exists to)) "~a is already there" to)
             (need (folder (path-parent to)) "~a isn't a folder" (path-parent to))
             (need (not (path-inside-p to a)) "~a can't go inside itself" a)
             (setf (gethash to overlay) (let ((what (exists a)))
                                          (if (eq what :new-folder) :new-folder (list :real (second what)))))
             (unless (eq op :copy)
               (setf (gethash a overlay) :gone))))
          (:mkdir
           (need (not (exists a)) "~a is already there" a)
           (need (folder (path-parent a)) "~a isn't a folder" (path-parent a))
           (setf (gethash a overlay) :new-folder))
          (:trash
           (need (exists a) "~a isn't there" a)
           (need (not (or (string= a "/") (string= a (home-folder))
                          (path-inside-p (home-folder) a)))
                 "~a holds your home folder: not put in the Trash" a)
           (need (not (or (string= a (trash-folder)) (path-inside-p a (trash-folder))))
                 "~a is already in the Trash" a)
           (setf (gethash a overlay) :gone))
          (:rmdir
           (need (folder a) "~a isn't a folder" a)
           (setf (gethash a overlay) :gone))
          (:restore
           (let ((in-trash (join-path (trash-folder) "files" a)))
             (need (path-exists-p in-trash) "~a isn't in the Trash any more" a)
             (need (not (exists b)) "~a is already there" b)
             (need (folder (path-parent b)) "~a isn't a folder" (path-parent b))
             (setf (gethash b overlay) (list :real in-trash)))))
        nil))))

(defvar *step-checks* '()
  "More checks for each step, from other parts of the core (archives.lisp:
nothing is written inside an archive opened read-only): functions of a step,
answering a problem in words, or NIL.")

(defun check-plan (steps &key (allowed *plan-operations*))
  "Everything wrong with the plan STEPS, as sentences (\"step 2: ...\");
NIL when it can be applied."
  (if (not (listp steps))
      (list "a plan is a list of steps")
      (let ((overlay (make-hash-table :test 'equal)))
        (loop for step in steps
              for n from 1
              for problem = (or (step-shape-problem step allowed)
                                (some (lambda (check) (funcall check step)) *step-checks*)
                                (check-step step overlay))
              when problem collect (format nil "step ~d: ~a" n problem)))))

;;; --- Doing a step -------------------------------------------------------------

(define-condition plan-refused (error)
  ((problems :initarg :problems :reader plan-refused-problems))
  (:report (lambda (c s)
             (format s "The plan wasn't applied; nothing changed:~{~%  ~a~}"
                     (plan-refused-problems c)))))

(define-condition step-failed (error)
  ((step :initarg :step :reader step-failed-step)
   (reason :initarg :reason :reader step-failed-reason))
  (:report (lambda (c s)
             (format s "Couldn't ~a: ~a" (describe-step (step-failed-step c))
                     (step-failed-reason c)))))

(defun syscall-reason (e)
  (handler-case (sb-int:strerror (sb-posix:syscall-errno e))
    (error () (princ-to-string e))))

(defun run-tool (step &rest command)
  "Run COMMAND (mv, cp) for STEP; a failure is the step's."
  (let ((code (sb-ext:process-exit-code
               (sb-ext:run-program (first command) (rest command)
                                   :search t :output nil :error nil :wait t))))
    (unless (eql code 0)
      (error 'step-failed :step step
                          :reason (format nil "~a failed (exit ~a)" (first command) code)))))

;;; --- Long copies: progress, and stopping safely ------------------------------------

(defvar *progress* nil
  "Non-nil: say how a long copy goes, on standard error, a form a line:
(:progress BYTES-DONE BYTES-ALL NAME).  The window asks for it.")
(defvar *bytes-done* 0 "Bytes copied by the plan's steps done so far.")
(defvar *steps-done* 0 "How many of the plan's steps are done, as it goes.")
(defvar *bytes-all* 0 "Bytes the plan's copies come to.")

(defun tree-size (path)
  "The bytes in PATH, a file or a folder and everything in it (links as links)."
  (let ((stat (file-stat path :follow nil)))
    (cond ((null stat) 0)
          ((= (stat-type stat) sb-posix:s-ifdir)
           (loop for name in (ignore-errors (folder-names path))
                 sum (tree-size (join-path path name))))
          (t (sb-posix:stat-size stat)))))

(defun same-disk-p (from to-folder)
  (let ((a (file-stat from :follow nil)) (b (file-stat to-folder)))
    (and a b (= (sb-posix:stat-dev a) (sb-posix:stat-dev b)))))

(defun say-progress (done name)
  (when *progress*
    (handler-case
        (with-standard-io-syntax
          (format *error-output* "(:progress ~d ~d ~s)~%" done *bytes-all* name)
          (finish-output *error-output*))
      (stream-error () nil))))

(defun copy-tree-step (step from to)
  "cp -a FROM to TO, saying how it goes. Stopped part way (the window's
Cancel, an interrupt), the part copied goes: TO wasn't there before."
  (let ((process (sb-ext:run-program "cp" (list "-a" "-T" "--" from to)
                                     :search t :output nil :error nil :wait nil))
        (finished nil))
    (unwind-protect
         (progn
           (loop for ticks from 0
                 while (sb-ext:process-alive-p process)
                 do (sleep 0.25)
                    (when (and *progress* (plusp ticks) (zerop (mod ticks 2)))
                      (say-progress (+ *bytes-done* (tree-size to)) (path-name from))))
           (unless (eql (sb-ext:process-exit-code process) 0)
             (error 'step-failed :step step
                                 :reason (format nil "cp failed (exit ~a)" (sb-ext:process-exit-code process))))
           (setf finished t))
      (unless finished
        (when (sb-ext:process-alive-p process)
          (sb-ext:process-kill process sb-unix:sigterm)
          (sb-ext:process-wait process))
        (when (path-exists-p to)
          (sb-ext:run-program "rm" (list "-rf" "--" to) :search t :output nil :error nil :wait t))))))

(defun move-path (step from to)
  (when (path-exists-p to)
    (error 'step-failed :step step :reason (format nil "~a is there now" to)))
  (handler-case (sb-posix:rename from to)
    (sb-posix:syscall-error (e)
      (if (= (sb-posix:syscall-errno e) sb-posix:exdev)
          ;; Another disk: rename(2) can't. A copy (which a stop takes back,
          ;; leaving FROM as it was), then FROM deleted, which nothing stops
          ;; half way: never the copy gone and FROM too.
          (progn
            (copy-tree-step step from to)
            ;; In a session of its own (setsid), so Cancel's interrupt,
            ;; sent to the whole group, doesn't reach it.
            (sb-sys:without-interrupts
              (let ((setsid (find-if #'path-exists-p '("/usr/bin/setsid" "/bin/setsid"))))
                (if setsid
                    (run-tool step setsid "-w" "rm" "-rf" "--" from)
                    (run-tool step "rm" "-rf" "--" from)))))
          (error 'step-failed :step step :reason (syscall-reason e))))))

(defun percent-encode (path)
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets path :external-format :utf-8)
          for char = (code-char byte)
          do (if (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                     (find char "/-_.~"))
                 (write-char char out)
                 (format out "%~2,'0X" byte)))))

(defun timestamp (&optional (time (get-universal-time)) (date-separator "-") (separator "T"))
  (multiple-value-bind (s m h day month year) (decode-universal-time time)
    (format nil "~d~a~2,'0d~a~2,'0d~a~2,'0d:~2,'0d:~2,'0d"
            year date-separator month date-separator day separator h m s)))

(defun native (path)
  "PATH for CL's file functions, read as it is: no wildcards in [ or *."
  (sb-ext:parse-native-namestring path))

(defun write-trash-info (info-file path)
  "Write the .trashinfo for PATH; NIL when INFO-FILE is taken meanwhile."
  (with-open-file (out (native info-file) :direction :output :if-exists nil
                                          :if-does-not-exist :create :external-format :utf-8)
    (when out
      (format out "[Trash Info]~%Path=~a~%DeletionDate=~a~%" (percent-encode path) (timestamp))
      t)))

(defun trash-path (step path)
  "Put PATH in the Trash as the freedesktop spec says (files/ and a
.trashinfo in info/), so file managers and `gio trash` see it too. Returns
the name it has there."
  (let* ((trash (trash-folder))
         (files (ensure-folder (join-path trash "files")))
         (info (ensure-folder (join-path trash "info")))
         (base (path-name path)))
    (loop for n from 1
          for name = (if (= n 1) base (format nil "~a.~d" base n))
          for file = (join-path files name)
          for info-file = (join-path info (concatenate 'string name ".trashinfo"))
          do (unless (or (path-exists-p file) (path-exists-p info-file))
               (when (write-trash-info info-file path)
                 (handler-bind ((error (lambda (e)
                                         (declare (ignore e))
                                         (ignore-errors (sb-posix:unlink info-file)))))
                   (move-path step path file))
                 (return name))))))

(defun restore-path (step name to)
  (let ((trash (trash-folder)))
    (move-path step (join-path trash "files" name) to)
    (ignore-errors (sb-posix:unlink (join-path trash "info" (concatenate 'string name ".trashinfo"))))))

(defun do-step (step)
  "Do STEP and return its inverse."
  (destructuring-bind (op a &optional b) step
    (handler-case
        (ecase op
          (:move (move-path step a b) (list :move b a))
          (:rename (move-path step a (step-target step))
           (list :rename (step-target step) (path-name a)))
          (:copy (when (path-exists-p b)
                   (error 'step-failed :step step :reason (format nil "~a is there now" b)))
           (copy-tree-step step a b)
           (list :trash b))
          (:mkdir (sb-posix:mkdir a #o777) (list :rmdir a))
          (:trash (list :restore (trash-path step a) a))
          (:rmdir (sb-posix:rmdir a) (list :mkdir a))
          (:restore (restore-path step a b) (list :trash b)))
      (sb-posix:syscall-error (e)
        (error 'step-failed :step step :reason (syscall-reason e))))))

;;; --- Applying a plan, and the journal ----------------------------------------------

(defun journal-folder ()
  (join-path (state-folder) "journal"))

(defun write-forms (path forms &key comment)
  (ensure-folder (path-parent path))
  (with-open-file (out (native path) :direction :output :if-exists :supersede :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-case* :downcase) (*print-readably* nil))
        (when comment (format out "~a~%" comment))
        (dolist (form forms) (prin1 form out) (terpri out)))))
  path)

(defun read-forms (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*package* (find-package '#:esploro.read)))
      (loop for form = (read stream nil stream)
            until (eq form stream) collect form))))

(defun read-plan (text)
  "The steps in TEXT (a plan's text); it may hold anything, which
CHECK-PLAN then judges."
  (with-input-from-string (in text) (read-forms in)))

(defun read-plan-file (path)
  (with-open-file (in (native path) :external-format :utf-8) (read-forms in)))

(defparameter *plan-file-comment*
  ";; An Esploro plan: one step a line, applied in order, nothing until you apply it.
;;   (:move \"/from\" \"/to\")   (:copy \"/from\" \"/to\")   (:rename \"/path\" \"new name\")
;;   (:mkdir \"/path\")          (:trash \"/path\")
;; Change, add or delete lines, save, and close (C-x # in Emacs).")

(defun write-plan (steps path)
  (write-forms path steps :comment *plan-file-comment*))

(defvar *journal-version* 0
  "Goes up each time the journal changes (a plan applied or undone), so the
window reads it again only then.")

(defun write-journal (steps inverse)
  ;; Named by the second, then a number the next free one: two plans in
  ;; one second (two esploro processes, or one applying several) each get
  ;; their own file, and sorting the names sorts them by age.
  (let* ((stamp (remove #\: (timestamp (get-universal-time) "" "-")))
         (path (loop for n from 1
                     for path = (join-path (journal-folder) (format nil "~a-~4,'0d.lisp" stamp n))
                     unless (path-exists-p path) return path)))
    (write-forms path (list (list :applied :time (timestamp) :steps steps :inverse inverse :undone nil)))
    (incf *journal-version*)))

(defun journal-entries ()
  "The applied plans, newest first, as (PATH . PLIST)."
  (let ((folder (journal-folder)))
    (when (directory-p folder)
      (let ((files (sort (remove-if-not (lambda (n) (let ((l (length n))) (and (> l 5) (string= ".lisp" n :start2 (- l 5)))))
                                        (folder-names folder))
                         #'string>)))
        (loop for name in files
              for path = (join-path folder name)
              for form = (ignore-errors (first (with-open-file (in (native path)) (read-forms in))))
              when (and (consp form) (eq (first form) :applied))
                collect (cons path (rest form)))))))

(defun apply-plan (steps &key (allowed *plan-operations*) (journal t))
  "Check the plan STEPS whole, then do it step by step; PLAN-REFUSED, with
nothing changed, when the check finds a problem. A step that fails
signals STEP-FAILED with restarts: RETRY-STEP, SKIP-STEP, STOP-HERE (keep
what's done) and UNDO-DONE (put back what's done). What was done goes in
the journal, so UNDO-LAST can take it back. Returns the steps done."
  (let ((problems (check-plan steps :allowed allowed)))
    (when problems (error 'plan-refused :problems problems)))
  (let ((done '()) (inverse '())
        (*bytes-done* 0)
        ;; What the copies (and moves to another disk) come to, for progress.
        (*bytes-all* (if *progress*
                         (loop for (op a b) in steps
                               when (or (eq op :copy)
                                        (and (eq op :move) (stringp b) (not (same-disk-p a (path-parent b)))))
                                 sum (tree-size a))
                         0)))
    (unwind-protect
         (block steps
           (dolist (step steps)
             (loop
               (restart-case
                   (progn (push (do-step step) inverse)
                          (push step done)
                          (incf *steps-done*)
                          (when (and *progress* (member (first step) '(:copy :move)))
                            (incf *bytes-done* (tree-size (third step)))
                            (say-progress *bytes-done* (path-name (second step))))
                          (return))
                 (retry-step ()
                   :report "Try this step again")
                 (skip-step ()
                   :report "Skip this step and go on"
                   (return))
                 (stop-here ()
                   :report "Stop here, keeping what's done"
                   (return-from steps))
                 (undo-done ()
                   :report "Stop, and put back what's done"
                   (let ((back inverse))
                     (setf done '() inverse '())
                     (dolist (step back) (ignore-errors (do-step step))))
                   (return-from steps))))))
      (when (and journal done)
        (write-journal (reverse done) inverse)))
    (reverse done)))

(defun undo-last ()
  "Undo the newest applied plan not yet undone. Returns its steps, or NIL
when there's nothing to undo. Refused (PLAN-REFUSED) when the files have
changed since so that it can't be undone whole."
  (let ((entry (find-if-not (lambda (e) (getf (cdr e) :undone)) (journal-entries))))
    (when entry
      (destructuring-bind (path &rest plist) entry
        (apply-plan (getf plist :inverse) :allowed *undo-operations* :journal nil)
        (setf (getf plist :undone) (timestamp))
        (write-forms path (list (cons :applied plist)))
        (incf *journal-version*)
        (getf plist :steps)))))

;;; --- The Trash, to look in, restore from and empty --------------------------------

(defun percent-decode (string)
  "PERCENT-ENCODE undone: %XX bytes, read as UTF-8."
  (let ((bytes (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with i = 0
          while (< i (length string))
          do (let ((c (char string i)))
               (if (and (char= c #\%) (<= (+ i 3) (length string))
                        (digit-char-p (char string (+ i 1)) 16) (digit-char-p (char string (+ i 2)) 16))
                   (progn (vector-push (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16) bytes)
                          (incf i 3))
                   (progn (loop for b across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                                do (vector-push b bytes))
                          (incf i)))))
    (sb-ext:octets-to-string (coerce bytes '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defun trash-entries ()
  "What's in the Trash, newest first, as (NAME ORIGINAL-PATH DELETION-DATE):
NAME in its files/, from the .trashinfo beside it in info/."
  (let* ((trash (trash-folder))
         (info (join-path trash "info"))
         (entries '()))
    (when (directory-p info)
      (dolist (file (folder-names info))
        (let ((l (length file)))
          (when (and (> l 10) (string= ".trashinfo" file :start2 (- l 10)))
            (let ((name (subseq file 0 (- l 10))) path date)
              (with-open-file (in (native (join-path info file)) :external-format :utf-8 :if-does-not-exist nil)
                (when in
                  (loop for line = (read-line in nil)
                        while line
                        do (cond ((and (> (length line) 5) (string= "Path=" line :end2 5))
                                  (setf path (percent-decode (subseq line 5))))
                                 ((and (> (length line) 13) (string= "DeletionDate=" line :end2 13))
                                  (setf date (subseq line 13)))))))
              (when (and path (path-exists-p (join-path trash "files" name)))
                (push (list name path (or date "")) entries)))))))
    (sort entries #'string> :key #'third)))

(defun restore-from-trash (names)
  "Put NAMES (as TRASH-ENTRIES calls them) back where they were, as a plan:
checked whole first, journaled, so undo puts them back in the Trash."
  (let ((entries (trash-entries)))
    (apply-plan (loop for name in names
                      for entry = (find name entries :key #'first :test #'string=)
                      collect (list :restore name (if entry (second entry) "")))
                :allowed *undo-operations*)))

(defun empty-trash ()
  "Delete everything in the Trash, for good. Returns how many were there."
  (let* ((trash (trash-folder))
         (n (length (trash-entries))))
    (dolist (sub '("files" "info"))
      (let ((folder (join-path trash sub)))
        (when (directory-p folder)
          (dolist (name (folder-names folder))
            (sb-ext:run-program "rm" (list "-rf" "--" (join-path folder name))
                                :search t :output nil :error nil)))))
    n))
