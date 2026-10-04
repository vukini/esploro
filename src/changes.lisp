;;;; changes.lisp — the journal, for people: what was done, and undoing any of it.
;;;;
;;;; Each applied plan is in the journal with its inverse. Here each is said
;;;; in a few words ("moved 3 files into ~/Work"), and any one can be undone,
;;;; not only the last: its inverse is checked whole first, so when the files
;;;; have moved on since, nothing changes and the answer says why.

(in-package #:esploro)

(defun count-words (n one many)
  (format nil "~d ~a" n (if (= n 1) one many)))

(defun summarize-steps (steps)
  "STEPS (a plan) in a few words: \"moved 3 files into ~/Work\"."
  (let* ((acts (remove :mkdir steps :key #'first))
         (ops (remove-duplicates (mapcar #'first acts)))
         (n (length acts))
         (one (and (= n 1) (path-name (second (first acts))))))
    (flet ((into (op)
             (let ((folders (remove-duplicates (mapcar (lambda (s) (path-parent (third s))) acts) :test #'string=)))
               (if (= (length folders) 1)
                   (format nil "~a ~a into ~a" op (or (and one (format nil "\"~a\"" one)) (count-words n "file" "files"))
                           (short-path (first folders)))
                   (format nil "~a ~a" op (count-words n "file" "files"))))))
      (cond ((null acts)
             (let ((made (remove :mkdir steps :key #'first :test-not #'eq)))
               (if (= (length made) 1) (format nil "made the folder ~a" (short-path (second (first made))))
                   (format nil "made ~a" (count-words (length made) "folder" "folders")))))
            ((rest ops)
             (format nil "~a: ~{~a~^, ~}" (count-words n "change" "changes")
                     (loop for (op . words) in '((:move . "moved ~d") (:copy . "copied ~d") (:rename . "renamed ~d")
                                                 (:trash . "~d to the Trash") (:restore . "~d back from the Trash"))
                           for k = (count op acts :key #'first)
                           when (plusp k) collect (format nil words k))))
            ;; Copies beside their originals: duplicates.
            ((and (eq (first ops) :copy)
                  (every (lambda (s) (string= (path-parent (second s)) (path-parent (third s)))) acts))
             (format nil "duplicated ~a in ~a" (if one (format nil "\"~a\"" one) (count-words n "file" "files"))
                     (short-path (path-parent (second (first acts))))))
            ((eq (first ops) :move) (into "moved"))
            ((eq (first ops) :copy) (into "copied"))
            ((eq (first ops) :trash)
             (format nil "put ~a in the Trash" (if one (format nil "\"~a\"" one) (count-words n "file" "files"))))
            ((eq (first ops) :rename)
             (let ((folders (remove-duplicates (mapcar (lambda (s) (path-parent (second s))) acts) :test #'string=)))
               (if one
                   (format nil "renamed \"~a\" to \"~a\"" one (third (first acts)))
                   (format nil "renamed ~a~@[ in ~a~]" (count-words n "file" "files")
                           (and (= (length folders) 1) (short-path (first folders)))))))
            ((eq (first ops) :restore) (format nil "brought ~a back from the Trash" (count-words n "file" "files")))
            (t (count-words (length steps) "change" "changes"))))))

(defun journal-id (path) (path-name path))

(defun changes (&key (limit 200))
  "The journal, newest first: ((ID TIME SUMMARY UNDONE (STEP-WORDS ...) FOLDER) ...)."
  (loop for (path . entry) in (journal-entries)
        for n from 0 below limit
        for steps = (getf entry :steps)
        collect (list (journal-id path) (getf entry :time) (summarize-steps steps)
                      (and (getf entry :undone) t)
                      (mapcar #'describe-step steps)
                      (let ((first (find-if (lambda (s) (member (first s) '(:move :copy :rename :mkdir :trash))) steps)))
                        (and first (path-parent (if (member (first first) '(:move :copy)) (third first) (second first))))))))

(defun undo-entry (id)
  "Undo the journal's entry ID (a file name in the journal folder): its
steps, or NIL when there's no such entry or it's undone already. Refused
(PLAN-REFUSED) when the files have moved on since."
  (let ((entry (find id (journal-entries) :key (lambda (e) (journal-id (car e))) :test #'string=)))
    (when (and entry (not (getf (cdr entry) :undone)))
      (destructuring-bind (path &rest plist) entry
        (apply-plan (getf plist :inverse) :allowed *undo-operations* :journal nil)
        (setf (getf plist :undone) (timestamp))
        (write-forms path (list (cons :applied plist)))
        (incf *journal-version*)
        (getf plist :steps)))))
