;;;; recipes.lisp — a change made once, done again on other files.
;;;;
;;;; The last plan applied (from the journal), seen as what it did to its
;;;; files: moved them into one folder, copied them into one, or put them
;;;; in the Trash. That is a recipe, (:move-into FOLDER), (:copy-into
;;;; FOLDER) or (:trash), which makes the same plan for other files. Named
;;;; ones are kept in ~/.config/esploro/recipes.lisp, plain data. A recipe
;;;; runs as a plan: checked first, journaled, undone like any other.
;;;;
;;;; Some recipes send each file its own way, and their plans wait for
;;;; your review before anything changes: (:rename-by FROM TO), names
;;;; changed by a pattern, and (:sort-by-kind (KIND FOLDER)...), files
;;;; into several folders by kind. They're written, not learnt from the
;;;; journal.

(in-package #:esploro)

(defun plan-recipe (steps)
  "What STEPS did, as a recipe for other files; NIL when they don't do one
thing to one folder (renames, or moves into several)."
  ;; A folder made for them (mkdir, then the moves into it) is part of it:
  ;; the folder is there now, for the next files.
  (let ((acts (remove :mkdir steps :key #'first)))
    (flet ((into (op)
             (and (every (lambda (s) (eq (first s) op)) acts)
                  (let ((folders (remove-duplicates (mapcar (lambda (s) (path-parent (third s))) acts)
                                                    :test #'string=)))
                    (and (= (length folders) 1) (first folders))))))
      (cond ((null acts) nil)
            ((every (lambda (s) (eq (first s) :trash)) acts) (list :trash))
            ((into :move) (list :move-into (into :move)))
            ((into :copy) (list :copy-into (into :copy)))))))

(defun describe-recipe (recipe)
  (case (first recipe)
    (:trash "put in the Trash")
    (:move-into (format nil "move into ~a" (short-path (second recipe))))
    (:copy-into (format nil "copy into ~a" (short-path (second recipe))))
    (:rename-by (format nil "rename ~a to ~a" (second recipe) (third recipe)))
    (:sort-by-kind (format nil "sort by kind into ~{~a~^, ~}"
                           (remove-duplicates (mapcar (lambda (e) (short-path (second e))) (rest recipe))
                                              :test #'string= :from-end t)))
    (t (format nil "~s" recipe))))

(defun recipe-valid-p (recipe)
  "Whether RECIPE is one Esploro knows, with the right things in it."
  (and (consp recipe)
       (case (first recipe)
         (:trash (null (rest recipe)))
         ((:move-into :copy-into) (and (= (length recipe) 2) (valid-path-p (second recipe))))
         (:rename-by (and (= (length recipe) 3) (stringp (second recipe)) (plusp (length (second recipe)))
                          (stringp (third recipe))))
         (:sort-by-kind (and (rest recipe)
                             (every (lambda (entry)
                                      (and (consp entry) (= (length entry) 2) (member (first entry) *sort-kinds*)
                                           (kind-folder-valid-p (second entry))))
                                    (rest recipe)))))))

(defun reviewed-recipe-p (recipe)
  "Whether RECIPE's plan waits for your review, rather than being done at
once: a rename by a pattern, or a sorting, where each file goes its own
way. (Moving into one folder is done at once, and undone with undo.)"
  (member (first recipe) '(:rename-by :sort-by-kind)))

(defun recipe-steps (recipe paths)
  "The plan RECIPE makes for PATHS. A file already in the folder is left be;
one whose name is taken there gets a free one (\"notes 2.txt\")."
  (ecase (first recipe)
    (:trash (mapcar (lambda (p) (list :trash p)) paths))
    ((:move-into :copy-into)
     (let ((folder (second recipe))
           (op (if (eq (first recipe) :move-into) :move :copy)))
       (loop for p in paths
             for to = (join-path folder (path-name p))
             unless (and (eq op :move) (string= (path-parent p) folder))
               collect (list op p (if (path-exists-p to) (free-name to "") to)))))))

(defun last-applied-steps ()
  "The steps of the newest applied plan not undone; NIL when there's none."
  (let ((entry (find-if-not (lambda (e) (getf (cdr e) :undone)) (journal-entries))))
    (and entry (getf (cdr entry) :steps))))

(defun recipes-file ()
  (join-path (env-folder "XDG_CONFIG_HOME" ".config") "esploro" "recipes.lisp"))

(defun read-recipes ()
  "Your named recipes, as (NAME . RECIPE)."
  (let ((file (recipes-file)))
    (when (path-exists-p file)
      (loop for form in (ignore-errors (read-plan-file file))
            when (and (consp form) (eq (first form) :recipe) (stringp (second form))
                      (recipe-valid-p (third form)))
              collect (cons (second form) (third form))))))

(defun write-recipes (recipes)
  (write-forms (recipes-file)
               (mapcar (lambda (r) (list :recipe (car r) (cdr r))) recipes)
               :comment ";; Esploro's recipes: changes kept to do again, by name (Recipes, on the right-click menu).
;; (:recipe \"NAME\" (:move-into \"/folder\")), (:copy-into \"/folder\") or (:trash);
;; (:rename-by \"IMG_*.jpg\" \"Holiday #n.jpg\"): * and ? take parts of a name, #1 #2 give them back,
;; #n is a number, ## a #. (:sort-by-kind (:image \"Images\") (:pdf \"/home/me/Documents\") ...):
;; each kind to its folder, beside the file or a whole path; kinds :image :video :audio :pdf
;; :lisp :text :archive, :file (anything else) and :folder.")
  recipes)

(defun save-recipe (name recipe)
  (write-recipes (append (remove name (read-recipes) :key #'car :test #'string=)
                         (list (cons name recipe)))))

(defun forget-recipe (name)
  (write-recipes (remove name (read-recipes) :key #'car :test #'string=)))

;;; --- Renames by a pattern -----------------------------------------------------------
;;;
;;; (:rename-by FROM TO): FROM is a name with wildcards, * any run of
;;; characters (as few as will do) and ? any one, matched in any case; TO
;;; says the new name, #1, #2... standing for what each wildcard took, #n
;;; for a number counting the files (01, 02... as wide as the count
;;; needs) and ## for a #. A FROM without wildcards is text to replace,
;;; wherever a name holds it. A name that doesn't fit FROM is left as it
;;; is. The plan is made whole or not at all: two files given one name,
;;; or a name already taken, and it's refused, saying which.

(defun wildcards-p (pattern)
  (or (find #\* pattern) (find #\? pattern)))

(defun pattern-parts (pattern name)
  "When NAME fits PATTERN, the parts its wildcards took, in order, and T;
else NIL and NIL."
  (labels ((m (p n)
             ;; A list holding the parts' list, or NIL for no fit.
             (cond ((= p (length pattern)) (and (= n (length name)) (list '())))
                   ((char= (char pattern p) #\*)
                    (loop for i from n to (length name)
                          for rest = (m (1+ p) i)
                          when rest return (list (cons (subseq name n i) (first rest)))))
                   ((= n (length name)) nil)
                   ((char= (char pattern p) #\?)
                    (let ((rest (m (1+ p) (1+ n))))
                      (and rest (list (cons (string (char name n)) (first rest))))))
                   ((char-equal (char pattern p) (char name n)) (m (1+ p) (1+ n))))))
    (let ((fit (m 0 0)))
      (values (first fit) (and fit t)))))

(defun expand-template (template parts number width)
  "TEMPLATE with #1..#9 as PARTS says, #n as NUMBER (WIDTH digits at least)
and ## as #. Anything else after a # is an error."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length template))
          do (let ((c (char template i)))
               (if (char/= c #\#)
                   (progn (write-char c out) (incf i))
                   (let ((next (and (< (1+ i) (length template)) (char template (1+ i)))))
                     (cond ((eql next #\#) (write-char #\# out))
                           ((eql next #\n) (format out "~v,'0d" width number))
                           ((and next (digit-char-p next) (char/= next #\0))
                            (let ((k (digit-char-p next)))
                              (unless (<= k (length parts))
                                (error "#~d, but the pattern has ~d wildcard~:p (* or ?) to give it" k (length parts)))
                              (write-string (nth (1- k) parts) out)))
                           (t (error "after a # comes 1 to 9 (a wildcard's part), n (a number) or # (a #)")))
                     (incf i 2)))))))

(defun replace-all (text old new)
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          do (write-string text out :start start :end (or at (length text)))
             (if at (progn (write-string new out) (setf start (+ at (length old)))) (loop-finish)))))

(defun pattern-new-names (from to paths)
  "(PATH . NEW-NAME) for each of PATHS that FROM fits, in their order, and
the paths it doesn't fit."
  (when (string= from "") (error "say what to look for in the names"))
  ;; A TO that can't be made is said at once, whether or not a name fits.
  (expand-template to (make-list (if (wildcards-p from) (count-if (lambda (c) (find c "*?")) from) 1)
                                 :initial-element "")
                   1 1)
  (let* ((fitting (if (wildcards-p from)
                      (loop for p in paths
                            for (parts fits) = (multiple-value-list (pattern-parts from (path-name p)))
                            when fits collect (cons p parts))
                      (loop for p in paths
                            when (search from (path-name p)) collect (cons p (list from)))))
         (width (length (princ-to-string (length fitting)))))
    (values (loop for (p . parts) in fitting
                  for n from 1
                  for new = (expand-template to parts n width)
                  collect (cons p (if (wildcards-p from) new (replace-all (path-name p) from new))))
            (remove-if (lambda (p) (assoc p fitting :test #'string=)) paths))))

(defun order-renames (names)
  "NAMES, (PATH . NEW-NAME) each, as steps in an order that frees each
name before it's taken, and the problems: names that aren't names, two
files given one, a name already taken, names that would go round. With
problems there are no steps: the plan is made whole or not at all."
  (let ((problems '()))
    (flet ((refuse () (return-from order-renames (values '() (nreverse problems)))))
      (loop for (path . name) in names
            unless (valid-name-p name)
              do (push (format nil "~a would be named ~s: a name isn't empty, has no /, and isn't . or .."
                               (path-name path) name)
                       problems))
      (when problems (refuse))
      (let ((renames (mapcar (lambda (r) (cons (car r) (join-path (path-parent (car r)) (cdr r)))) names)))
        (loop for (path . to) in renames
              for same = (remove to renames :key #'cdr :test-not #'string=)
              do (cond ((rest same)
                        ;; Said once, at the first of them.
                        (when (string= (car (first same)) path)
                          (push (format nil "~{~a~^, ~} would all be named ~a"
                                        (mapcar (lambda (r) (path-name (car r))) same) (path-name to))
                                problems)))
                       ((and (path-exists-p to) (not (find to renames :key #'car :test #'string=)))
                        (push (format nil "~a would be named ~a, which is already there" (path-name path) (path-name to))
                              problems))))
        (when problems (refuse))
        ;; A rename waits while its new name is still another's old one.
        (let ((pending renames) (steps '()))
          (loop while pending
                do (let ((ready (remove-if (lambda (r) (find (cdr r) pending :key #'car :test #'string=)) pending)))
                     (when (null ready)
                       (push (format nil "~{~a~^, ~} would take each other's names: rename one of them first"
                                     (mapcar (lambda (r) (path-name (car r))) pending))
                             problems)
                       (refuse))
                     (dolist (r ready) (push (list :rename (car r) (path-name (cdr r))) steps))
                     (setf pending (remove-if (lambda (r) (member r ready)) pending))))
          (values (nreverse steps) '()))))))

(defun some-names (paths &optional (most 3))
  (format nil "~{~a~^, ~}~:[~;, and ~d more~]" (mapcar #'path-name (subseq paths 0 (min most (length paths))))
          (> (length paths) most) (- (length paths) most)))

(defun rename-by-plan (from to paths)
  "The plan renaming PATHS by the pattern FROM to TO: its steps, what it
can't do (if anything, there are no steps), and in words what it does."
  (multiple-value-bind (news unfit) (pattern-new-names from to paths)
    (let ((renames (loop for (p . new) in news
                         unless (string= new (path-name p))
                           collect (cons p new))))
      (multiple-value-bind (steps problems) (order-renames renames)
        (values steps
                (or problems (and steps (check-plan steps)))
                (format nil "Rename ~s to ~s: ~[nothing to rename~:;~:*~d file~:p~]~@[; left as they are, not fitting: ~a~]"
                        from to (length steps) (and unfit (some-names unfit))))))))

;;; --- Sorting by kind into several folders ---------------------------------------------
;;;
;;; (:sort-by-kind (:image "Images") (:pdf "Documents") ...): each file
;;; goes to the folder for the first kind it is (:lisp is a :text too,
;;; :file is anything but a folder, :folder a folder). A folder named
;;; without a / at the start is beside the file ("Images" in the file's
;;; own folder, so sorting a search's files sorts each in its place); a
;;; whole path gathers them. A folder that isn't there is made. A name
;;; taken there gets a free one ("notes 2.txt"), said in the review; a
;;; file of no kind named, or already in its folder, stays.

(defparameter *sort-kinds* '(:image :video :audio :pdf :lisp :text :archive :file :folder)
  "The kinds a sorting can name.")

(defparameter *kind-folders*
  '((:image "Images") (:video "Videos") (:audio "Audio") (:pdf "Documents")
    (:text "Text") (:archive "Archives"))
  "Where Sort by Kind sends each kind, unless a recipe says otherwise.")

(defun kind-folder-valid-p (folder)
  "Whether FOLDER can be a sorting's folder: a whole path, or a name (or
names with /) below the file's folder."
  (and (stringp folder) (plusp (length folder))
       (if (char= (char folder 0) #\/)
           (valid-path-p folder)
           (every #'valid-name-p (split-path folder)))))

(defun kind-folder-for (kind kinds)
  (second (find-if (lambda (entry) (kind-is kind (first entry))) kinds)))

(defun free-path-in (folder name taken)
  "FOLDER/NAME, or when that's there or in TAKEN (a table), \"NAME 2\",
\"NAME 3\"... with its type kept: the first that's free."
  (let* ((dot (position #\. name :from-end t))
         (dot (and dot (plusp dot) dot))
         (stem (subseq name 0 dot))
         (type (if dot (subseq name dot) "")))
    (loop for n from 1
          for candidate = (join-path folder (if (= n 1) name (format nil "~a ~d~a" stem n type)))
          unless (or (path-exists-p candidate) (gethash candidate taken)) return candidate)))

(defun sort-by-kind-plan (kinds paths)
  "The plan sorting PATHS into folders by kind, as KINDS says: its steps,
the problems that stop it, and in words what it does (where they go,
what stays and why, which got another name)."
  (let ((steps '()) (problems '()) (stays '()) (renamed '())
        (counts '()) (made (make-hash-table :test 'equal)) (taken (make-hash-table :test 'equal)))
    (labels ((make-folder (folder)
               ;; Made once, its missing folders above it first.
               (unless (or (gethash folder made) (directory-p folder))
                 (make-folder (path-parent folder))
                 (setf (gethash folder made) t)
                 (push (list :mkdir folder) steps)))
             (stay (path why) (push (cons why path) stays)))
      (dolist (path paths)
        (let* ((kind (path-kind path))
               (name (and kind (kind-folder-for kind kinds)))
               (folder (and name (normalize-path (if (char= (char name 0) #\/) name
                                                     (join-path (path-parent path) name))))))
          (cond ((null kind) (stay path "not there"))
                ((null name) (stay path (if (eq kind :folder) "folders" "of no kind named")))
                ((or (string= (path-parent path) folder)
                     ;; In an Images of its own already, found below (a search).
                     (and (char/= (char name 0) #\/)
                          (let ((tail (concatenate 'string "/" (string-right-trim "/" name))))
                            (string-equal tail (path-parent path)
                                          :start2 (max 0 (- (length (path-parent path)) (length tail)))))))
                 (stay path "already in their folder"))
                ((or (string= folder path) (path-inside-p folder path)) (stay path "the folder itself"))
                ((and (path-exists-p folder) (not (directory-p folder)))
                 (push (format nil "~a can't go into ~a: that's a file, not a folder"
                               (path-name path) (short-path folder))
                       problems))
                (t (make-folder folder)
                   (let ((to (free-path-in folder (path-name path) taken)))
                     (setf (gethash to taken) t)
                     (unless (string= (path-name to) (path-name path))
                       (push (format nil "~a as ~a" (path-name path) (path-name to)) renamed))
                     (push (list :move path to) steps)
                     (let ((count (assoc folder counts :test #'string=)))
                       (if count (incf (cdr count)) (push (cons folder 1) counts)))))))))
    (setf steps (nreverse steps))
    (values (if problems '() steps)
            (or (nreverse problems) (and steps (check-plan steps)))
            (format nil "Sort by kind: ~:[nothing to move~;~:*~{~a~^, ~}~]~@[. Staying where they are: ~{~a~^; ~}~]~@[. Given another name, theirs being taken there: ~{~a~^, ~}~]"
                    (loop for (folder . n) in (reverse counts)
                          collect (format nil "~d into ~a" n (short-path folder)))
                    (loop for why in (remove-duplicates (mapcar #'car (reverse stays)) :test #'string= :from-end t)
                          for these = (reverse (mapcar #'cdr (remove why stays :key #'car :test-not #'string=)))
                          collect (format nil "~a (~a)" (some-names these) why))
                    (reverse renamed)))))

(defun recipe-plan (recipe paths)
  "The plan RECIPE makes for PATHS: its steps, the problems that stop it
(then there are no steps to do), and in words what it does."
  (case (first recipe)
    (:rename-by (rename-by-plan (second recipe) (third recipe) paths))
    (:sort-by-kind (sort-by-kind-plan (rest recipe) paths))
    (t (let ((steps (recipe-steps recipe paths)))
         (values steps (and steps (check-plan steps)) (describe-recipe recipe))))))

(defun parse-recipe (text)
  "The recipe written in TEXT, an s-expression; an error saying why when
it isn't one."
  (let ((forms (handler-case (read-plan text)
                 (error () (error "~a can't be read as an s-expression" text)))))
    (unless (and (= (length forms) 1) (recipe-valid-p (first forms)))
      (error "~a isn't a recipe: (:move-into \"/folder\"), (:copy-into \"/folder\"), (:trash) (:rename-by \"FROM\" \"TO\") or (:sort-by-kind (:image \"Images\") ...)"
             text))
    (first forms)))
