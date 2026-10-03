;;;; habits.lisp — what you keep doing, noticed, and offered back.
;;;;
;;;; The apprentice, for files: the journal says where you've been moving
;;;; files (and corrections.lisp where you sent an agent's), so when several
;;;; plans have sent like files into one folder, Esploro says what they have
;;;; in common, "PDFs named invoice, from ~/Downloads, into ~/Work/Invoices",
;;;; and offers it back: as a rule for agents (sorting.md), as a recipe (move
;;;; into that folder), or as a search for more like them. It never acts on
;;;; its own; a habit you wave away isn't offered again.

(in-package #:esploro)

(defparameter *habit-least* 3 "A habit is at least this many files...")
(defparameter *habit-plans* 2 "...moved by at least this many plans.")
(defparameter *habit-journal-limit* 1000 "The newest plans looked at.")

(defun journal-moves ()
  "Every file you've moved or copied into a folder, from the plans applied and
not undone, and the corrections you made to agents' plans: (FILE FOLDER PLAN)."
  (append
   (loop for (path . entry) in (let ((all (journal-entries)))
                                 (subseq all 0 (min (length all) *habit-journal-limit*)))
         unless (getf entry :undone)
           append (loop for step in (getf entry :steps)
                        when (and (member (first step) '(:move :copy)) (stringp (third step)))
                          collect (list (second step) (path-parent (third step)) path)))
   (let ((file (corrections-file)))
     (when (path-exists-p file)
       (loop for form in (ignore-errors (read-plan-file file))
             when (and (consp form) (eq (first form) :correction) (eq (third form) :elsewhere)
                       (stringp (sixth form)))
               collect (list (fourth form) (sixth form) (second form)))))))

(defun name-words (name)
  "The words in NAME worth noticing: three letters or more, not only digits,
any case; its type left out."
  (let* ((dot (position #\. name :from-end t))
         (stem (if (and dot (plusp dot)) (subseq name 0 dot) name))
         (words '()) (start nil))
    (flet ((end-word (i)
             (when start
               (let ((w (string-downcase (subseq stem start i))))
                 (when (and (>= (length w) 3) (notevery #'digit-char-p w))
                   (pushnew w words :test #'string=)))
               (setf start nil))))
      (loop for i from 0 below (length stem)
            for c = (char stem i)
            do (if (alphanumericp c) (unless start (setf start i)) (end-word i))
            finally (end-word (length stem))))
    words))

(defun most-common (items &key (test #'equal))
  "The commonest of ITEMS, and how many times it's there."
  (let ((best nil) (count 0))
    (dolist (item (remove-duplicates items :test test))
      (let ((n (count item items :test test)))
        (when (> n count) (setf best item count n))))
    (values best count)))

(defparameter *kind-words*
  '((:image . "Pictures") (:video . "Videos") (:audio . "Sound files") (:pdf . "PDFs and e-books")
    (:lisp . "Lisp files") (:text . "Text files") (:archive . "Zips and other archives")))

(defun habit-of (folder moves)
  "What MOVES into FOLDER have in common, when it's something: a plist, or NIL."
  (let* ((names (mapcar (lambda (m) (path-name (first m))) moves))
         (n (length names))
         (kind (multiple-value-bind (k c) (most-common (mapcar #'type-kind names))
                 (and (not (eq k :file)) (>= (* c 10) (* n 8)) k)))
         (word (multiple-value-bind (w c) (most-common (mapcan #'name-words names) :test #'string=)
                 (and w (>= c *habit-least*) (>= (* c 10) (* n 6)) w)))
         (from (multiple-value-bind (f c) (most-common (mapcar (lambda (m) (path-parent (first m))) moves))
                 (and (>= (* c 10) (* n 8)) (string/= f folder) f))))
    (when (or kind word)
      (let ((query (remove nil (list (and word (list :name word)) (and kind (list :kind kind))))))
        (list :folder folder :count n :kind kind :word word :from from
              :query (if (rest query) (cons :and query) (first query)))))))

(defun habit-key (habit)
  (format nil "~a|~@[~(~a~)~]|~@[~a~]" (getf habit :folder) (getf habit :kind) (getf habit :word)))

(defun habit-what (habit)
  "The files the habit is about, in words: \"PDFs named invoice\"."
  (let ((kind (getf habit :kind)) (word (getf habit :word)))
    (format nil "~a~@[ named \"~a\"~]"
            (if kind (cdr (assoc kind *kind-words*)) "Files")
            word)))

(defun habit-rule (habit)
  (format nil "~a~@[ from ~a~] go in ~a." (habit-what habit)
          (and (getf habit :from) (short-path (getf habit :from)))
          (short-path (getf habit :folder))))

(defun habit-said (habit)
  (format nil "You've moved ~d ~a~@[ from ~a~] into ~a."
          (getf habit :count)
          ;; "Pictures" becomes "pictures"; "PDFs" stays as it is.
          (let ((what (habit-what habit)))
            (if (and (> (length what) 1) (lower-case-p (char what 1)))
                (string-downcase what :end 1)
                what))
          (and (getf habit :from) (short-path (getf habit :from)))
          (short-path (getf habit :folder))))

;;; --- Seen: waved away, or already told -------------------------------------------

(defun habits-seen-file () (join-path (state-folder) "habits-seen.lisp"))

(defun habits-seen ()
  "The keys of the habits you've waved away (:dismissed) or been told of (:told)."
  (let ((file (habits-seen-file)))
    (when (path-exists-p file)
      (loop for form in (ignore-errors (read-plan-file file))
            when (and (consp form) (member (first form) '(:dismissed :told)) (stringp (second form)))
              collect (cons (first form) (second form))))))

(defun note-habit (how key)
  (unless (member (cons how key) (habits-seen) :test #'equal)
    (write-forms (habits-seen-file)
                 (append (mapcar (lambda (s) (list (car s) (cdr s))) (habits-seen)) (list (list how key)))
                 :comment ";; Esploro's habits you've waved away (:dismissed) or been told of (:told).")))

(defun habits ()
  "What you keep doing that hasn't been waved away: the habits, the commonest first."
  (let ((by-folder (make-hash-table :test 'equal))
        (seen (habits-seen))
        (found '()))
    (dolist (m (journal-moves))
      (push m (gethash (second m) by-folder)))
    (maphash (lambda (folder moves)
               (let ((moves (remove-duplicates moves :key #'first :test #'string=)))
                 (when (and (>= (length moves) *habit-least*)
                            (>= (length (remove-duplicates (mapcar #'third moves) :test #'equal)) *habit-plans*)
                            (not (path-inside-p folder (trash-folder))))
                   (let ((habit (habit-of folder moves)))
                     (when (and habit (not (member (cons :dismissed (habit-key habit)) seen :test #'equal)))
                       (push habit found))))))
             by-folder)
    (sort found #'> :key (lambda (h) (getf h :count)))))

(defun habit-answer (habit)
  "HABIT for the window: (KEY SAID RULE RECIPE WORDS FROM)."
  (list (habit-key habit) (habit-said habit) (habit-rule habit)
        (list :move-into (getf habit :folder))
        (query-words (getf habit :query))
        (getf habit :from)))
