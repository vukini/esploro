;;;; recipes.lisp — a change made once, done again on other files.
;;;;
;;;; The last plan applied (from the journal), seen as what it did to its
;;;; files: moved them into one folder, copied them into one, or put them
;;;; in the Trash. That is a recipe, (:move-into FOLDER), (:copy-into
;;;; FOLDER) or (:trash), which makes the same plan for other files. Named
;;;; ones are kept in ~/.config/esploro/recipes.lisp, plain data. A recipe
;;;; runs as a plan: checked first, journaled, undone like any other.

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
    (t (format nil "~s" recipe))))

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
            when (and (consp form) (eq (first form) :recipe) (stringp (second form)) (consp (third form))
                      (member (first (third form)) '(:trash :move-into :copy-into)))
              collect (cons (second form) (third form))))))

(defun write-recipes (recipes)
  (write-forms (recipes-file)
               (mapcar (lambda (r) (list :recipe (car r) (cdr r))) recipes)
               :comment ";; Esploro's recipes: changes kept to do again, by name (Recipes, on the right-click menu).
;; (:recipe \"NAME\" (:move-into \"/folder\")), (:copy-into \"/folder\") or (:trash).")
  recipes)

(defun save-recipe (name recipe)
  (write-recipes (append (remove name (read-recipes) :key #'car :test #'string=)
                         (list (cons name recipe)))))

(defun forget-recipe (name)
  (write-recipes (remove name (read-recipes) :key #'car :test #'string=)))
