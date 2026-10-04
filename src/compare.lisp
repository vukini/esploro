;;;; compare.lisp — two folders side by side: what's only in one, what differs.
;;;;
;;;; Both are walked together. A name in one and not the other is "only
;;;; there" (a whole folder is said once, with how many files it holds); a
;;;; file in both is the same when its size and time are (or, the times
;;;; differing, when its bytes are), else it differs, and the newer side is
;;;; noted. From that, a plan that brings one up to date with the other:
;;;; what's missing is copied, what's older is replaced (the old one to the
;;;; Trash); what's only there, or newer there, is left alone, unless the
;;;; plan is to mirror, when what's only there goes to the Trash.

(in-package #:esploro)

(defparameter *compare-limit* 200000 "At most this many names looked at.")

(defun count-files (path)
  "The files in PATH, a folder and everything below it (1 for a file)."
  (let ((stat (file-stat path :follow nil)))
    (cond ((null stat) 0)
          ((= (stat-type stat) sb-posix:s-ifdir)
           (loop for name in (ignore-errors (folder-names path))
                 sum (count-files (join-path path name))))
          (t 1))))

(defun same-file-p (a b sa sb)
  "Whether files A and B (their lstats SA, SB) hold the same."
  (cond ((= (stat-type sa) sb-posix:s-iflnk)
         (and (= (stat-type sb) sb-posix:s-iflnk) (equal (read-link a) (read-link b))))
        ((/= (sb-posix:stat-size sa) (sb-posix:stat-size sb)) nil)
        ((= (sb-posix:stat-mtime sa) (sb-posix:stat-mtime sb)) t)
        (t (same-bytes-p a b))))

(defun compare-folders (a b)
  "What differs between folders A and B: (VALUES ONLY-A ONLY-B DIFFER SAME),
ONLY-A and ONLY-B lists of (RELATIVE-PATH FILES) (a folder said once),
DIFFER of (RELATIVE-PATH NEWER), NEWER :a, :b or :kind (a file one side, a
folder the other), SAME how many files are the same. NIL when there's too
much to look at."
  (let ((only-a '()) (only-b '()) (differ '()) (same 0) (seen 0))
    (labels ((rel (prefix name) (if (string= prefix "") name (concatenate 'string prefix "/" name)))
             (walk (da db prefix)
               (let ((names-a (ignore-errors (folder-names da)))
                     (names-b (ignore-errors (folder-names db))))
                 (dolist (name (sort (remove-duplicates (append names-a names-b) :test #'string=) #'string<))
                   (when (> (incf seen) *compare-limit*) (return-from compare-folders nil))
                   (let* ((pa (join-path da name)) (pb (join-path db name))
                          (sa (and (member name names-a :test #'string=) (file-stat pa :follow nil)))
                          (sb (and (member name names-b :test #'string=) (file-stat pb :follow nil))))
                     (flet ((dirp (s) (= (stat-type s) sb-posix:s-ifdir)))
                       (cond ((and sa (not sb)) (push (list (rel prefix name) (count-files pa)) only-a))
                             ((and sb (not sa)) (push (list (rel prefix name) (count-files pb)) only-b))
                             ((not (or sa sb)))
                             ((and (dirp sa) (dirp sb)) (walk pa pb (rel prefix name)))
                             ((or (dirp sa) (dirp sb)) (push (list (rel prefix name) :kind) differ))
                             ((same-file-p pa pb sa sb) (incf same))
                             (t (push (list (rel prefix name)
                                            (if (>= (sb-posix:stat-mtime sa) (sb-posix:stat-mtime sb)) :a :b))
                                      differ)))))))))
      (walk a b ""))
    (values (nreverse only-a) (nreverse only-b) (nreverse differ) same)))

(defun update-plan (from to &key mirror)
  "The plan that brings folder TO up to date with FROM: (VALUES STEPS
LEFT-ALONE), LEFT-ALONE how many differing files were newer in TO, or of
another kind there. With MIRROR, what's only in TO goes to the Trash."
  (multiple-value-bind (only-from only-to differ) (compare-folders from to)
    (let ((steps '()) (left 0))
      (dolist (entry only-from)
        (push (list :copy (join-path from (first entry)) (join-path to (first entry))) steps))
      (dolist (entry differ)
        (if (eq (second entry) :a)
            (progn (push (list :trash (join-path to (first entry))) steps)
                   (push (list :copy (join-path from (first entry)) (join-path to (first entry))) steps))
            (incf left)))
      (when mirror
        (dolist (entry only-to)
          (push (list :trash (join-path to (first entry))) steps)))
      (values (nreverse steps) left))))
