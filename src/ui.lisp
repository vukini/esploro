;;;; ui.lisp — Esploro's window (McCLIM).
;;;;
;;;; The folder's entries on the left, each one a presentation of its file:
;;;; what's on screen is the file object itself, so a click opens it, a
;;;; right click offers the file commands for its kind, and a shift-click
;;;; marks it. The plan on the right, applied or edited in Emacs with one
;;;; click. Beside each file, the windows that have it open; opening a file
;;;; that's open goes to its window.

(in-package #:esploro)

(clim:define-presentation-type file-entry ())
(clim:define-presentation-type folder-up ())

(clim:define-gesture-name :mark :pointer-button-press (:left :shift))

(clim:define-application-frame esploro ()
  ((folder :initarg :folder :accessor folder)
   (entries :initform '() :accessor entries)
   (marks :initform (make-hash-table :test 'equal) :accessor marks)
   (plan :initform '() :accessor plan)
   (where :initform (make-hash-table :test 'equal) :accessor where)
   (show-hidden :initform nil :accessor show-hidden)
   (help-shown :initform nil :accessor help-shown)
   (note :initform nil :accessor note))
  (:pretty-name "Esploro")
  (:menu-bar nil)
  (:panes
   (files :application
          :display-function 'display-files
          :scroll-bars :both
          :end-of-line-action :allow
          :text-style (clim:make-text-style :sans-serif :roman :normal))
   (plan-pane :application
              :display-function 'display-plan
              :scroll-bars :vertical
              :end-of-line-action :wrap*
              :text-style (clim:make-text-style :sans-serif :roman :small))
   (interactor :interactor :height 70))
  (:layouts
   (default (clim:vertically ()
              (clim:horizontally () (2/3 files) (1/3 plan-pane))
              interactor))))

;;; --- Reading the folder --------------------------------------------------------

(defun refresh (frame)
  (setf (entries frame) (handler-case (list-folder (folder frame) :hidden (show-hidden frame))
                          (sb-posix:syscall-error (e)
                            (setf (note frame) (format nil "Can't read ~a: ~a" (folder frame) (syscall-reason e)))
                            '()))
        (where frame) (scan-where))
  ;; Marks on files that went away go too.
  (maphash (lambda (path v) (declare (ignore v))
             (unless (path-exists-p path) (remhash path (marks frame))))
           (marks frame)))

(defun go-to (frame folder)
  (setf (folder frame) folder)
  (clrhash (marks frame))
  (refresh frame))

;;; --- Drawing ---------------------------------------------------------------------

(defun human-size (bytes)
  (cond ((null bytes) "")
        ((< bytes 1024) (format nil "~d B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1f KB" (/ bytes 1024)))
        ((< bytes (* 1024 1024 1024)) (format nil "~,1f MB" (/ bytes 1024 1024)))
        (t (format nil "~,1f GB" (/ bytes 1024 1024 1024)))))

(defun human-time (unix)
  (let ((time (+ unix (encode-universal-time 0 0 0 1 1 1970 0))))
    (multiple-value-bind (s m h day month year) (decode-universal-time time)
      (declare (ignore s))
      (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day h m))))

(defparameter *kind-inks*
  `((:folder . ,clim:+blue+) (:image . ,clim:+dark-magenta+) (:video . ,clim:+dark-red+)
    (:audio . ,clim:+dark-orange+) (:pdf . ,clim:+firebrick+) (:lisp . ,clim:+dark-green+)
    (:archive . ,clim:+saddle-brown+)))

(defun kind-ink (kind)
  (or (cdr (assoc kind *kind-inks*)) clim:+foreground-ink+))

(defun display-files (frame pane)
  (clim:with-text-style (pane (clim:make-text-style nil :bold :large))
    (write-string (short-path (folder frame)) pane))
  (terpri pane)
  (when (note frame)
    (clim:with-text-face (pane :italic)
      (write-string (note frame) pane))
    (terpri pane))
  (terpri pane)
  (unless (string= (folder frame) "/")
    (clim:with-output-as-presentation (pane (path-parent (folder frame)) 'folder-up)
      (write-string "..  (up)" pane))
    (terpri pane))
  (clim:formatting-table (pane :x-spacing 18)
    (dolist (entry (entries frame))
      (let ((path (entry-path entry))
            (places (file-where (entry-path entry) (where frame))))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (write-string (if (gethash path (marks frame)) "*" " ") pane))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane entry 'file-entry)
              (clim:with-drawing-options (pane :ink (kind-ink (entry-kind entry)))
                (write-string (entry-name entry) pane)
                (when (eq (entry-kind entry) :folder) (write-string "/" pane))
                (when (entry-link-p entry) (write-string " ->" pane)))))
          (clim:formatting-cell (pane :align-x :right)
            (write-string (human-size (entry-size entry)) pane))
          (clim:formatting-cell (pane)
            (write-string (human-time (entry-mtime entry)) pane))
          (clim:formatting-cell (pane)
            (when places
              (clim:with-drawing-options (pane :ink clim:+dark-cyan+)
                (format pane "open in ~a" (where-text places))))))))))

(defun display-plan (frame pane)
  (when (help-shown frame)
    (return-from display-plan (display-help pane)))
  (clim:with-text-style (pane (clim:make-text-style nil :bold nil))
    (write-string "Plan" pane))
  (terpri pane)
  (cond ((null (plan frame))
         (write-string "Nothing planned. Right-click a file for what can be done with it; changes wait here until applied. " pane)
         (clim:present '(com-help) 'clim:command :stream pane)
         (write-string " (or ?) shows what Esploro can do." pane)
         (terpri pane))
        (t
         (let ((problems (check-plan (plan frame))))
           (loop for step in (plan frame)
                 for n from 1
                 do (format pane "~d. ~a~%" n (describe-step step (folder frame))))
           (terpri pane)
           (when problems
             (clim:with-drawing-options (pane :ink clim:+dark-red+)
               (format pane "~{~a~%~}" problems)))
           (terpri pane)
           (unless problems
             (clim:present '(com-apply-plan) 'clim:command :stream pane)
             (write-string "   " pane))
           (clim:present '(com-edit-plan) 'clim:command :stream pane)
           (write-string "   " pane)
           (clim:present '(com-clear-plan) 'clim:command :stream pane)
           (terpri pane))))
  (terpri pane)
  (let ((last (find-if-not (lambda (e) (getf (cdr e) :undone)) (journal-entries))))
    (when last
      (format pane "Last applied (~a):~%~{  ~a~%~}" (getf (cdr last) :time)
              (mapcar (lambda (step) (describe-step step (folder frame))) (getf (cdr last) :steps)))
      (clim:present '(com-undo) 'clim:command :stream pane)
      (terpri pane))))

;;; --- What's marked ------------------------------------------------------------

(defun marked (frame)
  (loop for entry in (entries frame)
        when (gethash (entry-path entry) (marks frame)) collect (entry-path entry)))

(defun targets (frame entry)
  "The files a command on ENTRY acts on: all marked ones when ENTRY is one
of them, otherwise ENTRY alone."
  (let ((marked (marked frame)))
    (if (member (entry-path entry) marked :test #'string=) marked (list (entry-path entry)))))

(defun add-to-plan (frame steps)
  (setf (plan frame) (append (plan frame) steps)))

(defun resolve (frame text)
  "TEXT typed by someone as a path: ~ is home, relative is from the folder."
  (let ((text (string-trim " " text)))
    (normalize-path
     (cond ((string= text "~") (home-folder))
           ((and (> (length text) 1) (string= "~/" text :end2 2)) (join-path (home-folder) (subseq text 2)))
           ((and (plusp (length text)) (char= (char text 0) #\/)) text)
           (t (join-path (folder frame) text))))))

;;; --- Commands ---------------------------------------------------------------------

(define-esploro-command (com-open :name t) ((entry 'file-entry))
  (let ((frame clim:*application-frame*))
    (if (eq (entry-kind entry) :folder)
        (go-to frame (entry-path entry))
        (let ((window (open-path (entry-path entry))))
          (setf (note frame) (and window (format nil "~a is open in ~a: went there"
                                                 (entry-name entry) (where-text (file-where (entry-path entry) (where frame))))))))))

(clim:define-presentation-to-command-translator open-entry
    (file-entry com-open esploro :gesture :select :documentation "Open")
    (object)
  (list object))

(define-esploro-command (com-up :name t :keystroke (:up :meta)) ()
  (let ((frame clim:*application-frame*))
    (go-to frame (path-parent (folder frame)))))

(define-esploro-command (com-go-up-to :name nil) ((folder 'folder-up))
  (go-to clim:*application-frame* folder))

(clim:define-presentation-to-command-translator go-up
    (folder-up com-go-up-to esploro :gesture :select :documentation "Up")
    (object)
  (list object))

(define-esploro-command (com-go :name t) ((place 'string :prompt "folder"))
  (let* ((frame clim:*application-frame*)
         (path (resolve frame place)))
    (if (and path (directory-p path))
        (go-to frame path)
        (setf (note frame) (format nil "~a isn't a folder" place)))))

(define-esploro-command (com-toggle-mark :name t) ((entry 'file-entry))
  (let ((marks (marks clim:*application-frame*))
        (path (entry-path entry)))
    (if (gethash path marks) (remhash path marks) (setf (gethash path marks) t))))

(clim:define-presentation-to-command-translator mark-entry
    (file-entry com-toggle-mark esploro :gesture :mark :documentation "Mark")
    (object)
  (list object))

(define-esploro-command (com-unmark-all :name t) ()
  (clrhash (marks clim:*application-frame*)))

(define-esploro-command (com-file-menu :name nil) ((entry 'file-entry))
  (let* ((frame clim:*application-frame*)
         (paths (targets frame entry))
         (choice (clim:menu-choose
                  (append
                   (loop for command in (commands-for paths)
                         collect (list (format nil "~a~:[~; (plan)~]" (file-command-label command)
                                               (file-command-changes command))
                                       :value command
                                       :documentation (file-command-doc command)))
                   (list (list "Rename..." :value :rename)
                         (list "Move to..." :value :move)
                         (list "Copy to..." :value :copy)))
                  :label (if (rest paths) (format nil "~d marked" (length paths)) (entry-name entry)))))
    (case choice
      ((nil))
      (:rename (com-rename entry (clim:accept 'string :prompt "new name" :default (entry-name entry)
                                                      :insert-default t)))
      (:move (com-move-to paths (clim:accept 'string :prompt "move to")))
      (:copy (com-copy-to paths (clim:accept 'string :prompt "copy to")))
      (t (add-to-plan frame (run-file-command choice paths))))))

(clim:define-presentation-to-command-translator entry-menu
    (file-entry com-file-menu esploro :gesture :menu :documentation "What can be done with it")
    (object)
  (list object))

(defun into (frame paths target-text)
  "Steps taking PATHS to TARGET-TEXT: into it when it's a folder (or ends
in /), else, for one file, to that path."
  (let ((target (resolve frame target-text)))
    (cond ((null target) nil)
          ((or (directory-p target) (char= (char target-text (1- (length target-text))) #\/))
           (loop for path in paths collect (list (join-path target (path-name path)))))
          ((rest paths) (setf (note frame) "Several files go into a folder") nil)
          (t (list (list target))))))

(define-esploro-command (com-move-to :name nil) ((paths 't) (target 'string))
  (let ((frame clim:*application-frame*))
    (add-to-plan frame (loop for path in paths
                             for (to) in (into frame paths target)
                             collect (list :move path to)))))

(define-esploro-command (com-copy-to :name nil) ((paths 't) (target 'string))
  (let ((frame clim:*application-frame*))
    (add-to-plan frame (loop for path in paths
                             for (to) in (into frame paths target)
                             collect (list :copy path to)))))

(define-esploro-command (com-move-marked :name t) ((target 'string :prompt "to"))
  (com-move-to (marked clim:*application-frame*) target))

(define-esploro-command (com-copy-marked :name t) ((target 'string :prompt "to"))
  (com-copy-to (marked clim:*application-frame*) target))

(define-esploro-command (com-trash-marked :name t) ()
  (let ((frame clim:*application-frame*))
    (add-to-plan frame (loop for path in (marked frame) collect (list :trash path)))))

(define-esploro-command (com-rename :name t) ((entry 'file-entry) (name 'string :prompt "new name"))
  (add-to-plan clim:*application-frame* (list (list :rename (entry-path entry) name))))

(define-esploro-command (com-new-folder :name t) ((name 'string :prompt "name"))
  (let ((frame clim:*application-frame*))
    (add-to-plan frame (list (list :mkdir (join-path (folder frame) name))))))

(define-esploro-command (com-apply-plan :name t) ()
  (let ((frame clim:*application-frame*))
    (handler-case
        (handler-bind ((step-failed
                         (lambda (c)
                           ;; A step failed half way: ask, with Lisp's restarts.
                           (let ((restart (clim:menu-choose
                                           (loop for r in (compute-restarts c)
                                                 when (member (restart-name r) '(retry-step skip-step stop-here undo-done))
                                                   collect (list (princ-to-string r) :value r))
                                           :label (princ-to-string c))))
                             (invoke-restart (or restart 'stop-here))))))
          (let ((done (apply-plan (plan frame))))
            (setf (plan frame) '()
                  (note frame) (format nil "Applied ~d step~:p" (length done)))))
      (plan-refused (c) (setf (note frame) (princ-to-string c))))
    (refresh frame)))

(define-esploro-command (com-clear-plan :name t) ()
  (setf (plan clim:*application-frame*) '()))

(define-esploro-command (com-undo :name t :keystroke (#\z :control)) ()
  (let ((frame clim:*application-frame*))
    (handler-case (setf (note frame) (if (undo-last) "Undone" "Nothing to undo"))
      (plan-refused (c) (setf (note frame) (princ-to-string c))))
    (refresh frame)))

(define-esploro-command (com-edit-plan :name t) ()
  ;; The plan as text in Emacs; read back when Emacs lets go of it (C-x #).
  ;; emacsclient waits, so in a thread of its own; the result comes back
  ;; as a command, which McCLIM runs in the frame's own thread.
  (let* ((frame clim:*application-frame*)
         (file (write-plan (plan frame) (join-path (state-folder) "plan.lisp"))))
    (setf (note frame) "Editing the plan in Emacs: save, then C-x # to bring it back")
    (sb-thread:make-thread
     (lambda ()
       (let ((code (sb-ext:process-exit-code
                    (sb-ext:run-program "emacsclient" (list "-c" file) :search t :output nil :error nil))))
         (clim:execute-frame-command frame (list 'com-plan-edited file (eql code 0)))))
     :name "esploro emacs")))

(define-esploro-command (com-plan-edited :name nil) ((file 'string) (ok 'boolean))
  (let ((frame clim:*application-frame*))
    (if (not ok)
        (setf (note frame) "Emacs couldn't open the plan (is its server running?)")
        (handler-case (setf (plan frame) (read-plan-file file)
                            (note frame) "The plan, as edited")
          (error (e) (setf (note frame) (format nil "The edited plan couldn't be read: ~a" e)))))))

(define-esploro-command (com-refresh :name t :keystroke (:f5)) ()
  (refresh clim:*application-frame*))

(define-esploro-command (com-toggle-hidden :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (show-hidden frame) (not (show-hidden frame)))
    (refresh frame)))

(define-esploro-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

;;; --- Help -----------------------------------------------------------------------

(defparameter *help*
  '(("Mouse"
     ("click" "open it: a folder goes in; a file open in a window goes to that window")
     ("shift-click" "mark it (or unmark)")
     ("right-click" "what can be done with it; with all the marked ones when it's marked"))
    ("Keys"
     ("? or F1" "this help")
     ("Alt+Up" "the folder above")
     ("F5" "read the folder again")
     ("Ctrl+z" "undo the last applied plan")
     ("Tab" "completes a command's name as you type it")
     ("Ctrl+?" "what can be typed here"))
    ("Commands (type them on the Command: line; Tab completes)"
     ("Go FOLDER" "go to a folder: ~, ~/src, or a name in this one")
     ("Up" "the folder above")
     ("New Folder NAME" "plan a new folder here")
     ("Rename FILE NAME" "plan a new name (click the file when it asks)")
     ("Move Marked TO" "plan moving the marked files into a folder (end it with / for a new one)")
     ("Copy Marked TO" "the same, copying")
     ("Trash Marked" "plan putting the marked files in the Trash")
     ("Unmark All" "")
     ("Apply Plan" "do the plan; nothing changes before this")
     ("Edit Plan" "the plan as text in Emacs; save, then C-x # brings it back")
     ("Clear Plan" "forget the plan")
     ("Undo" "put back the last applied plan")
     ("Toggle Hidden" "show or hide files starting with a dot")
     ("Refresh" "read the folder again")
     ("Help" "this")
     ("Quit" ""))))

(define-esploro-command (com-help :name t :keystroke (:f1)) ()
  (let ((frame clim:*application-frame*))
    (setf (help-shown frame) (not (help-shown frame)))))

(defun display-help (pane)
  (loop for (title . rows) in *help*
        do (clim:with-text-face (pane :bold) (format pane "~a~%" title))
           (loop for (what does) in rows
                 do (clim:with-text-face (pane :bold) (write-string what pane))
                    (format pane "~:[  ~a~;~*~]~%" (string= does "") does))
           (terpri pane))
  (format pane "Each file's own commands (Open in emacs, Duplicate, ...) are on its right-click menu.~%~%")
  (clim:present '(com-help) 'clim:command :stream pane)
  (write-string " again hides this." pane)
  (terpri pane))

;;; "?" is help too. A key like this works anywhere on the command line,
;;; so a "?" can't be typed into a name there: rename such a file in the
;;; plan's text (Edit Plan) instead.
(dolist (key '((#\?) (#\? :shift)))   ; X sends ? with shift held
  (clim:add-keystroke-to-command-table 'esploro key :command '(com-help) :errorp nil))

;;; --- Starting --------------------------------------------------------------------

(defun run (&optional (folder (home-folder)))
  (let ((frame (clim:make-application-frame 'esploro :folder folder :width 1100 :height 700)))
    (refresh frame)
    (clim:run-frame-top-level frame)))
