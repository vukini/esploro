;;;; ui.lisp — Esploro's window (McCLIM).
;;;;
;;;; The folder's entries on the left, each one a presentation of its file:
;;;; what's on screen is the file object itself, so the right-click menu
;;;; offers the file commands for its kind. A selection, made with the
;;;; mouse or the keys as in any file manager (click, Ctrl and Shift; the
;;;; arrows, Ctrl and Shift); the one file selected is previewed on the
;;;; right, above the plan. Beside each file, the windows that have it open;
;;;; opening a file that's open goes to its window.

(in-package #:esploro)

(clim:define-presentation-type file-entry ())
(clim:define-presentation-type folder-up ())

(clim:define-gesture-name :toggle-select :pointer-button-press (:left :control))
(clim:define-gesture-name :extend-select :pointer-button-press (:left :shift))

(clim:define-application-frame esploro ()
  ((folder :initarg :folder :accessor folder)
   (entries :initform '() :accessor entries)
   (selection :initform (make-hash-table :test 'equal) :accessor selection)
   (cursor :initform nil :accessor cursor)   ; the index the keys move from, or :up on ".."
   (anchor :initform nil :accessor anchor)   ; where Shift extends from
   (last-click :initform nil :accessor last-click)
   (scroll-wanted :initform nil :accessor scroll-wanted)
   (row-height :initform 20 :accessor row-height)
   (rows-top :initform 0 :accessor rows-top)
   (plan :initform '() :accessor plan)
   (where :initform (make-hash-table :test 'equal) :accessor where)
   (show-hidden :initform nil :accessor show-hidden)
   (help-shown :initform nil :accessor help-shown)
   (patterns :initform (make-hash-table :test 'equal) :accessor patterns)
   (note :initform nil :accessor note))
  (:pretty-name "Esploro")
  (:menu-bar nil)
  (:panes
   ;; Incremental redisplay: after a key, only what changed is drawn again
   ;; (two rows for an arrow), not every pane from scratch, which flashed.
   (files :application
          :display-function 'display-files
          :incremental-redisplay t
          :scroll-bars :both
          :end-of-line-action :allow
          :text-style (clim:make-text-style :sans-serif :roman :normal))
   (preview :application
            :display-function 'display-preview
            :incremental-redisplay t
            :scroll-bars :vertical
            :end-of-line-action :wrap*
            :text-style (clim:make-text-style :sans-serif :roman :small))
   (plan-pane :application
              :display-function 'display-plan
              :incremental-redisplay t
              :scroll-bars :vertical
              :end-of-line-action :wrap*
              :text-style (clim:make-text-style :sans-serif :roman :small))
   (interactor :interactor :height 70))
  (:layouts
   (default (clim:vertically ()
              (clim:horizontally ()
                (3/5 files)
                (2/5 (clim:vertically () (2/3 preview) (1/3 plan-pane))))
              interactor))))

;;; --- Reading the folder --------------------------------------------------------

(defun refresh (frame)
  (setf (entries frame) (handler-case (list-folder (folder frame) :hidden (show-hidden frame))
                          (sb-posix:syscall-error (e)
                            (setf (note frame) (format nil "Can't read ~a: ~a" (folder frame) (syscall-reason e)))
                            '()))
        (where frame) (scan-where))
  ;; What went away isn't selected any more; the cursor stays in range.
  (maphash (lambda (path v) (declare (ignore v))
             (unless (path-exists-p path) (remhash path (selection frame))))
           (selection frame))
  (let ((n (length (entries frame))))
    (when (integerp (cursor frame))
      (setf (cursor frame) (if (zerop n) nil (min (cursor frame) (1- n)))))))

(defun go-to (frame folder &key cursor-on)
  "Show FOLDER, with the cursor on the entry whose path is CURSOR-ON (the
folder just left, going up), nothing selected."
  (setf (folder frame) folder)
  (clrhash (selection frame))
  (refresh frame)
  (let ((i (and cursor-on (position cursor-on (entries frame) :key #'entry-path :test #'string=))))
    (setf (cursor frame) i (anchor frame) i (scroll-wanted frame) t)))

;;; --- The selection ----------------------------------------------------------------

(defun selected (frame)
  "The selected paths, in the folder's order."
  (loop for entry in (entries frame)
        when (gethash (entry-path entry) (selection frame)) collect (entry-path entry)))

(defun cursor-entry (frame)
  "The entry under the cursor; none on the \"..\" row."
  (and (integerp (cursor frame)) (nth (cursor frame) (entries frame))))

(defun up-row-p (frame)
  "True when there is a \"..\" row: anywhere but /."
  (string/= (folder frame) "/"))

(defun select-only (frame i)
  (clrhash (selection frame))
  (let ((entry (nth i (entries frame))))
    (when entry (setf (gethash (entry-path entry) (selection frame)) t)))
  (setf (cursor frame) i (anchor frame) i (scroll-wanted frame) t))

(defun select-range (frame i)
  "Select from the anchor to I, and nothing else."
  (let ((from (or (anchor frame) (and (integerp (cursor frame)) (cursor frame)) i)))
    (clrhash (selection frame))
    (loop for k from (min from i) to (max from i)
          do (setf (gethash (entry-path (nth k (entries frame))) (selection frame)) t))
    (setf (anchor frame) from (cursor frame) i (scroll-wanted frame) t)))

(defun toggle-select (frame i)
  (let ((path (entry-path (nth i (entries frame))))
        (selection (selection frame)))
    (if (gethash path selection) (remhash path selection) (setf (gethash path selection) t))
    (setf (cursor frame) i (anchor frame) i)))

(defun quiet-command-line (frame)
  "Clear the command line's past: each key would otherwise leave an empty
Command: line behind."
  (let ((interactor (clim:find-pane-named frame 'interactor)))
    (when interactor (clim:window-clear interactor))))

(defun move-cursor (frame delta how)
  "Move the cursor DELTA rows (clamped). HOW: :only selects just the row
it lands on, :extend selects from the anchor to it, :keep moves only.
The \"..\" row above the files is row -1: Up from the first file lands
on it, and Return there goes up. Selecting more never reaches it."
  (quiet-command-line frame)
  (let* ((n (length (entries frame)))
         (lowest (if (and (up-row-p frame) (not (eq how :extend))) -1 0))
         (from (case (cursor frame)
                 (:up -1)
                 ;; With no cursor yet, Down starts at the top and Up at the bottom.
                 ((nil) (if (plusp delta) -1 n))
                 (t (cursor frame)))))
    (unless (and (zerop n) (= lowest 0))
      (let ((i (max lowest (min (1- n) (+ from delta)))))
        (if (= i -1)
            (progn (clrhash (selection frame))
                   (setf (cursor frame) :up (anchor frame) nil (scroll-wanted frame) t))
            (ecase how
              (:only (select-only frame i))
              (:extend (select-range frame i))
              (:keep (setf (cursor frame) i (scroll-wanted frame) t))))))))

(defun targets (frame entry)
  "The files a command on ENTRY acts on: the whole selection when ENTRY is
in it, otherwise ENTRY alone."
  (let ((selected (selected frame)))
    (if (member (entry-path entry) selected :test #'string=) selected (list (entry-path entry)))))

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

;;; --- Drawing the folder ---------------------------------------------------------------

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

(defparameter *selected-ink* (clim:make-rgb-color 0.80 0.88 1.0))
(defparameter *cursor-ink* (clim:make-rgb-color 0.25 0.45 0.85))

(defun kind-ink (kind)
  (or (cdr (assoc kind *kind-inks*)) clim:+foreground-ink+))

(defun entry-label (entry)
  (format nil "~a~:[~;/~]~:[~; ->~]" (entry-name entry)
          (eq (entry-kind entry) :folder) (entry-link-p entry)))

(defun text-width (pane string)
  (values (clim:text-size pane string)))

(defun display-files (frame pane)
  ;; Each part in an updating-output, remembered with what it showed (its
  ;; cache value): one whose value is the same isn't drawn again.
  (clim:updating-output (pane :unique-id 'header :cache-test #'equal
                              :cache-value (list (folder frame) (note frame)))
    (clim:with-text-style (pane (clim:make-text-style nil :bold :large))
      (write-string (short-path (folder frame)) pane))
    (terpri pane)
    (when (note frame)
      (clim:with-text-face (pane :italic)
        (write-string (note frame) pane))
      (terpri pane))
    (terpri pane))
  (clim:updating-output (pane :unique-id 'up-row :cache-test #'equal
                              :cache-value (list (folder frame) (eq (cursor frame) :up)
                                                 (clim:bounding-rectangle-width (clim:sheet-region pane))))
    (display-up-row frame pane))
  (display-rows frame pane))

(defun display-up-row (frame pane)
  (when (up-row-p frame)
    ;; Under the cursor (Up from the first file), it looks like a selected row.
    (when (eq (cursor frame) :up)
      (multiple-value-bind (x y) (clim:stream-cursor-position pane)
        (declare (ignore x))
        (let ((h (+ (clim:text-style-height (clim:medium-text-style pane) pane) 2))
              (w (clim:bounding-rectangle-width (clim:sheet-region pane))))
          (clim:draw-rectangle* pane 0 (- y 1) w (+ y h) :ink *selected-ink*)
          (clim:draw-rectangle* pane 1 (- y 1) (- w 2) (+ y h -1)
                                :filled nil :ink *cursor-ink* :line-thickness 1))))
    (clim:with-output-as-presentation (pane (path-parent (folder frame)) 'folder-up)
      (write-string "..  (up)" pane))
    (terpri pane)))

(defun display-rows (frame pane)
  ;; Rows drawn by hand rather than as a table, so a selected row can have
  ;; its background across the whole width, and the keys know where rows are.
  (let* ((entries (entries frame))
         (row-h (+ (clim:text-style-height (clim:medium-text-style pane) pane) 6))
         (gap 24)
         (name-w (max 150 (min 520 (loop for e in entries maximize (text-width pane (entry-label e))))))
         (sizes (mapcar (lambda (e) (human-size (entry-size e))) entries))
         (size-w (max 40 (loop for s in sizes maximize (text-width pane s))))
         (time-w (text-width pane "2026-10-01 22:17"))
         (x-name 10)
         (x-size-end (+ x-name name-w gap size-w))
         (x-time (+ x-size-end gap))
         (x-where (+ x-time time-w gap))
         (width (max (+ x-where 320)
                     (clim:bounding-rectangle-width (clim:sheet-region pane))))
         (top (nth-value 1 (clim:stream-cursor-position pane)))
         (blank (clim:pane-background pane)))
    (setf (row-height frame) row-h (rows-top frame) top)
    (loop for entry in entries
          for size in sizes
          for i from 0
          for y = (+ top (* i row-h))
          for text-y = (+ y 3)
          for places = (file-where (entry-path entry) (where frame))
          for selected = (gethash (entry-path entry) (selection frame))
          for here = (eql i (cursor frame))
          do (clim:updating-output
                 (pane :unique-id (entry-path entry) :cache-test #'equal
                       ;; All a row shows, and where: the same, it isn't redrawn.
                       :cache-value (list (and selected t) here (entry-label entry) size
                                          (entry-mtime entry) (entry-kind entry)
                                          (and places (where-text places))
                                          y width name-w size-w))
             (clim:with-output-as-presentation (pane entry 'file-entry)
               ;; The whole row is the presentation, background included.
               (clim:draw-rectangle* pane 0 y width (+ y row-h)
                                     :ink (if selected *selected-ink* blank))
               (when here
                 (clim:draw-rectangle* pane 1 (1+ y) (- width 2) (+ y row-h -1)
                                       :filled nil :ink *cursor-ink* :line-thickness 1))
               (clim:draw-text* pane (entry-label entry) x-name text-y
                                :align-y :top :ink (kind-ink (entry-kind entry)))
               (clim:draw-text* pane size x-size-end text-y :align-x :right :align-y :top)
               (clim:draw-text* pane (human-time (entry-mtime entry)) x-time text-y :align-y :top)
               (when places
                 (clim:draw-text* pane (format nil "open in ~a" (where-text places)) x-where text-y
                                  :align-y :top :ink clim:+dark-cyan+)))))
    (setf (clim:stream-cursor-position pane)
          (values 0 (+ top (* (length entries) row-h) 6)))))

(defmethod clim:redisplay-frame-panes :after ((frame esploro) &key force-p)
  (declare (ignore force-p))
  ;; After the keys move the cursor, scroll so its row is in view: once
  ;; every pane is drawn, as the list's new size is known only then.
  (when (and (scroll-wanted frame) (cursor frame))
    (setf (scroll-wanted frame) nil)
    (let* ((pane (clim:find-pane-named frame 'files))
           (viewport (clim:pane-viewport-region pane))
           (y (if (eq (cursor frame) :up)
                  0                      ; the ".." row, above the files
                  (+ (rows-top frame) (* (cursor frame) (row-height frame)))))
           (bottom (+ y (row-height frame))))
      (clim:with-bounding-rectangle* (vx vy vx2 vy2) viewport
        (declare (ignore vx2))
        (cond ((< y vy) (clim:scroll-extent pane vx (max 0 (- y (row-height frame)))))
              ((> bottom vy2) (clim:scroll-extent pane vx (+ (- bottom (- vy2 vy)) (row-height frame)))))))))

;;; --- The preview ---------------------------------------------------------------------

(defun preview-pattern (frame png)
  "PNG as a McCLIM pattern, read once."
  (or (gethash png (patterns frame))
      (setf (gethash png (patterns frame))
            (ignore-errors (clim:make-pattern-from-bitmap-file (native png) :format :png)))))

(defun display-preview (frame pane)
  ;; Drawn again only when what it shows changes: not for a key that moves
  ;; the cursor without changing the selection.
  (clim:updating-output (pane :unique-id 'preview :cache-test #'equal
                              :cache-value (list (help-shown frame) (folder frame) (selected frame)
                                                 (clim:bounding-rectangle-width (clim:sheet-region pane))))
    (display-preview-1 frame pane)))

(defun display-preview-1 (frame pane)
  (when (help-shown frame)
    (return-from display-preview-1 (display-help pane)))
  (let ((selected (selected frame)))
    (case (length selected)
      (0 (clim:with-text-face (pane :italic)
           (write-string "Select a file to see it here: click it, or the arrow keys. " pane))
         (clim:present '(com-help) 'clim:command :stream pane)
         (write-string " (or ?) shows what Esploro can do." pane)
         (terpri pane))
      (1 (preview-file frame pane (first selected)))
      (t (let ((sizes (loop for p in selected
                            for e = (find p (entries frame) :key #'entry-path :test #'string=)
                            when (and e (entry-size e)) sum (entry-size e))))
           (clim:with-text-face (pane :bold)
             (format pane "~d selected" (length selected)))
           (format pane ", ~a in files~%~%" (human-size sizes))
           (loop for p in selected repeat 40 do (format pane "~a~%" (path-name p)))
           (when (> (length selected) 40) (format pane "...~%")))))))

(defun preview-file (frame pane path)
  (let ((entry (find path (entries frame) :key #'entry-path :test #'string=))
        (kind (path-kind path)))
    (clim:with-text-style (pane (clim:make-text-style nil :bold :normal))
      (write-string (path-name path) pane))
    (terpri pane)
    (when entry
      (format pane "~(~a~)~:[~;, ~:*~a~]  ·  ~a~%" kind (and (entry-size entry) (human-size (entry-size entry)))
              (human-time (entry-mtime entry))))
    (let ((places (file-where path (where frame))))
      (when places
        (clim:with-drawing-options (pane :ink clim:+dark-cyan+)
          (format pane "open in ~a~%" (where-text places)))))
    (terpri pane)
    (case kind
      (:folder
       (let ((inside (ignore-errors (list-folder path :hidden (show-hidden frame)))))
         (format pane "~d item~:p~%~%" (length inside))
         (loop for e in inside repeat 50
               do (clim:with-drawing-options (pane :ink (kind-ink (entry-kind e)))
                    (format pane "~a~%" (entry-label e))))))
      ((:image :pdf :video)
       (let* ((png (thumbnail path))
              (pattern (and png (preview-pattern frame png))))
         (if pattern
             (multiple-value-bind (x y) (clim:stream-cursor-position pane)
               (declare (ignore x))
               (clim:draw-pattern* pane pattern 6 y)
               (setf (clim:stream-cursor-position pane)
                     (values 0 (+ y (clim:pattern-height pattern) 6))))
             (format pane "(no preview: ~a)~%" (or (file-description path) "unknown")))))
      ((:text :lisp)
       (let ((lines (text-head path)))
         (clim:with-text-style (pane (clim:make-text-style :fix :roman :small))
           (dolist (line lines) (write-string line pane) (terpri pane)))))
      (t (format pane "~a~%" (or (file-description path) ""))
         (let ((lines (text-head path :lines 30)))
           (when lines
             (terpri pane)
             (clim:with-text-style (pane (clim:make-text-style :fix :roman :small))
               (dolist (line lines) (write-string line pane) (terpri pane)))))))))

;;; --- The plan -----------------------------------------------------------------------

(defun display-plan (frame pane)
  ;; The journal is read from disk only when it has changed.
  (clim:updating-output (pane :unique-id 'plan :cache-test #'equal
                              :cache-value (list (copy-tree (plan frame)) (folder frame) *journal-version*
                                                 (clim:bounding-rectangle-width (clim:sheet-region pane))))
    (display-plan-1 frame pane)))

(defun display-plan-1 (frame pane)
  (clim:with-text-face (pane :bold) (write-string "Plan" pane))
  (terpri pane)
  (cond ((null (plan frame))
         (write-string "Nothing planned. Changes (right-click, Delete, F2, typed commands) wait here until applied." pane)
         (terpri pane))
        (t
         (let ((problems (check-plan (plan frame))))
           (loop for step in (plan frame)
                 for n from 1
                 do (format pane "~d. ~a~%" n (describe-step step (folder frame))))
           (when problems
             (clim:with-drawing-options (pane :ink clim:+dark-red+)
               (format pane "~{~a~%~}" problems)))
           (unless problems
             (clim:present '(com-apply-plan) 'clim:command :stream pane)
             (write-string "   " pane))
           (clim:present '(com-edit-plan) 'clim:command :stream pane)
           (write-string "   " pane)
           (clim:present '(com-clear-plan) 'clim:command :stream pane)
           (terpri pane))))
  (let ((last (find-if-not (lambda (e) (getf (cdr e) :undone)) (journal-entries))))
    (when last
      (terpri pane)
      (format pane "Last applied (~a):~%~{  ~a~%~}" (getf (cdr last) :time)
              (mapcar (lambda (step) (describe-step step (folder frame))) (getf (cdr last) :steps)))
      (clim:present '(com-undo) 'clim:command :stream pane)
      (terpri pane))))

;;; --- Opening ---------------------------------------------------------------------------

(defun open-entry (frame entry)
  (if (eq (entry-kind entry) :folder)
      (go-to frame (entry-path entry))
      (let ((window (open-path (entry-path entry))))
        (setf (note frame) (and window (format nil "~a is open in ~a: went there"
                                               (entry-name entry) (window-class window)))))))

(define-esploro-command (com-open :name t) ((entry 'file-entry))
  (open-entry clim:*application-frame* entry))

(define-esploro-command (com-open-current :name nil :keystroke (:down :meta)) ()
  (let* ((frame clim:*application-frame*)
         (entry (cursor-entry frame)))
    (cond (entry (open-entry frame entry))
          ((eq (cursor frame) :up) (com-up)))))

;;; Return on an empty command line opens the file under the cursor. (As a
;;; key of its own, Return couldn't also end a typed command.)
(defmethod clim:read-frame-command :around ((frame esploro) &key stream)
  (declare (ignore stream))
  (let ((command (call-next-method)))
    (if (and (null command) (or (cursor-entry frame) (eq (cursor frame) :up)))
        '(com-open-current)
        command)))

;;; --- The mouse --------------------------------------------------------------------------

(defparameter *double-click-time* 0.45)

(define-esploro-command (com-click :name nil) ((entry 'file-entry))
  ;; A click selects; a second click on the same file soon after opens it.
  (let* ((frame clim:*application-frame*)
         (i (position entry (entries frame)))
         (now (/ (get-internal-real-time) internal-time-units-per-second))
         (last (last-click frame)))
    (setf (last-click frame) (cons entry now))
    (cond ((and last (eq (car last) entry) (< (- now (cdr last)) *double-click-time*))
           (setf (last-click frame) nil)
           (open-entry frame entry))
          (i (select-only frame i)))))

(clim:define-presentation-to-command-translator click-entry
    (file-entry com-click esploro :gesture :select :documentation "Select (twice: open)")
    (object)
  (list object))

(define-esploro-command (com-click-toggle :name nil) ((entry 'file-entry))
  (let* ((frame clim:*application-frame*)
         (i (position entry (entries frame))))
    (when i (toggle-select frame i))))

(clim:define-presentation-to-command-translator toggle-entry
    (file-entry com-click-toggle esploro :gesture :toggle-select :documentation "Add to the selection, or take out")
    (object)
  (list object))

(define-esploro-command (com-click-extend :name nil) ((entry 'file-entry))
  (let* ((frame clim:*application-frame*)
         (i (position entry (entries frame))))
    (when i (select-range frame i))))

(clim:define-presentation-to-command-translator extend-entry
    (file-entry com-click-extend esploro :gesture :extend-select :documentation "Select up to here")
    (object)
  (list object))

(define-esploro-command (com-go-up-to :name nil) ((folder 'folder-up))
  (let ((frame clim:*application-frame*))
    (go-to frame folder :cursor-on (folder frame))))

(clim:define-presentation-to-command-translator go-up
    (folder-up com-go-up-to esploro :gesture :select :documentation "Up")
    (object)
  (list object))

(define-esploro-command (com-file-menu :name nil) ((entry 'file-entry))
  (let* ((frame clim:*application-frame*)
         (i (position entry (entries frame))))
    ;; Right-click on something not selected selects it, as elsewhere.
    (when (and i (not (gethash (entry-path entry) (selection frame))))
      (select-only frame i))
    (let* ((paths (targets frame entry))
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
                    :label (if (rest paths) (format nil "~d selected" (length paths)) (entry-name entry)))))
      (case choice
        ((nil))
        (:rename (com-rename entry (clim:accept 'string :prompt "new name" :default (entry-name entry)
                                                        :insert-default t)))
        (:move (com-move-to paths (clim:accept 'string :prompt "move to")))
        (:copy (com-copy-to paths (clim:accept 'string :prompt "copy to")))
        (t (add-to-plan frame (run-file-command choice paths)))))))

(clim:define-presentation-to-command-translator entry-menu
    (file-entry com-file-menu esploro :gesture :menu :documentation "What can be done with it")
    (object)
  (list object))

;;; --- The keys ----------------------------------------------------------------------------
;;;
;;; Keys like these work anywhere on the command line, so they can't be
;;; used for typing in it; none of them is a letter for that reason.

(defparameter *page* 15)

(defmacro define-cursor-key (name keystroke delta how)
  `(define-esploro-command (,name :name nil :keystroke ,keystroke) ()
     (move-cursor clim:*application-frame* ,delta ,how)))

(define-cursor-key com-cursor-down (:down) 1 :only)
(define-cursor-key com-cursor-up (:up) -1 :only)
(define-cursor-key com-extend-down (:down :shift) 1 :extend)
(define-cursor-key com-extend-up (:up :shift) -1 :extend)
(define-cursor-key com-move-down (:down :control) 1 :keep)
(define-cursor-key com-move-up (:up :control) -1 :keep)
(define-cursor-key com-page-down (:next) *page* :only)
(define-cursor-key com-page-up (:prior) (- *page*) :only)
(define-cursor-key com-extend-page-down (:next :shift) *page* :extend)
(define-cursor-key com-extend-page-up (:prior :shift) (- *page*) :extend)
(define-cursor-key com-first (:home :control) most-negative-fixnum :only)
(define-cursor-key com-last (:end :control) most-positive-fixnum :only)
(define-cursor-key com-extend-first (:home :control :shift) most-negative-fixnum :extend)
(define-cursor-key com-extend-last (:end :control :shift) most-positive-fixnum :extend)

(define-esploro-command (com-toggle-current :name nil :keystroke (#\Space :control)) ()
  (let ((frame clim:*application-frame*))
    (quiet-command-line frame)
    (when (integerp (cursor frame)) (toggle-select frame (cursor frame)))))

(define-esploro-command (com-select-all :name t :keystroke (#\a :control)) ()
  (let ((frame clim:*application-frame*))
    (dolist (entry (entries frame)) (setf (gethash (entry-path entry) (selection frame)) t))))

(define-esploro-command (com-select-none :name t :keystroke (:escape)) ()
  (clrhash (selection clim:*application-frame*)))

(define-esploro-command (com-trash-selected :name t :keystroke (:delete)) ()
  (let ((frame clim:*application-frame*))
    (add-to-plan frame (loop for path in (selected frame) collect (list :trash path)))))

(define-esploro-command (com-rename-current :name nil :keystroke (:f2)) ()
  (let* ((frame clim:*application-frame*)
         (entry (cursor-entry frame)))
    (when entry
      (com-rename entry (clim:accept 'string :prompt "new name" :default (entry-name entry)
                                             :insert-default t)))))

;;; --- Typed commands -------------------------------------------------------------------

(define-esploro-command (com-up :name t :keystroke (:up :meta)) ()
  (let ((frame clim:*application-frame*))
    (go-to frame (path-parent (folder frame)) :cursor-on (folder frame))))

(define-esploro-command (com-go :name t) ((place 'string :prompt "folder"))
  (let* ((frame clim:*application-frame*)
         (path (resolve frame place)))
    (if (and path (directory-p path))
        (go-to frame path)
        (setf (note frame) (format nil "~a isn't a folder" place)))))

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

(define-esploro-command (com-move-selected :name t) ((target 'string :prompt "to"))
  (com-move-to (selected clim:*application-frame*) target))

(define-esploro-command (com-copy-selected :name t) ((target 'string :prompt "to"))
  (com-copy-to (selected clim:*application-frame*) target))

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

(define-esploro-command (com-toggle-hidden :name t :keystroke (#\h :control)) ()
  (let ((frame clim:*application-frame*))
    (setf (show-hidden frame) (not (show-hidden frame)))
    (refresh frame)))

(define-esploro-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

;;; --- Help -----------------------------------------------------------------------

(defparameter *help*
  '(("Mouse"
     ("click" "select it, and see it on the right")
     ("double-click" "open it: a folder goes in; a file open in a window goes to that window")
     ("Ctrl+click" "add it to the selection, or take it out")
     ("Shift+click" "select everything from the last one clicked to here")
     ("right-click" "what can be done with it (with all of the selection when it's in it)"))
    ("Keys"
     ("Up, Down" "select the one above or below; Up from the first is \"..\", where Return goes up")
     ("Shift+Up, Shift+Down" "select more, up or down")
     ("Ctrl+Up, Ctrl+Down" "move without selecting; Ctrl+Space then adds or takes out")
     ("Page Up, Page Down" "a page at a time; Ctrl+Home, Ctrl+End to the first or last")
     ("Return, Alt+Down" "open it (Return when nothing is typed below)")
     ("Alt+Up" "the folder above")
     ("Ctrl+a, Escape" "select everything, nothing")
     ("Delete" "plan putting the selection in the Trash")
     ("F2" "plan a new name")
     ("Ctrl+z" "undo the last applied plan")
     ("Ctrl+h" "show or hide files starting with a dot")
     ("F5" "read the folder again")
     ("? or F1" "this help"))
    ("Commands (type them on the Command: line; Tab completes)"
     ("Go FOLDER" "go to a folder: ~, ~/src, or a name in this one")
     ("New Folder NAME" "plan a new folder here")
     ("Move Selected TO" "plan moving the selection into a folder (end it with / for a new one)")
     ("Copy Selected TO" "the same, copying")
     ("Apply Plan" "do the plan; nothing changes before this")
     ("Edit Plan" "the plan as text in Emacs; save, then C-x # brings it back")
     ("Clear Plan" "forget the plan")
     ("Up, Undo, Refresh, Select All, Select None, Trash Selected, Toggle Hidden, Quit" ""))))

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

;;; "?" is help too; so a "?" can't be typed into a name on the command
;;; line: rename such a file in the plan's text (Edit Plan) instead.
(dolist (key '((#\?) (#\? :shift)))   ; X sends ? with shift held
  (clim:add-keystroke-to-command-table 'esploro key :command '(com-help) :errorp nil))

;;; --- Starting --------------------------------------------------------------------

(defun run (&optional (folder (home-folder)))
  (let ((frame (clim:make-application-frame 'esploro :folder folder :width 1200 :height 760)))
    (refresh frame)
    (clim:run-frame-top-level frame)))
