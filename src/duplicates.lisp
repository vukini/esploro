;;;; duplicates.lisp — files that are the same, byte for byte.
;;;;
;;;; Below a folder, files of one size are compared: a hash of their first
;;;; and last 64 KB, then of all of them, and last byte for byte (cmp), so
;;;; only files that are truly the same are called copies. Each group keeps
;;;; its oldest; the others are offered for the Trash, as a plan to review.
;;;; Left out: files under 1 KB (a small file twice is usually meant), and
;;;; files inside a git repository other than the one looked in (the same
;;;; LICENSE in two projects is each project's own).

(in-package #:esploro)

(defparameter *duplicate-least* 1024 "Files smaller than this aren't looked at.")
(defparameter *duplicate-files-limit* 200000 "At most this many files looked at.")

(defun file-md5 (path &key head)
  "PATH's MD5, of all of it, or with HEAD of its first and last HEAD bytes."
  (ignore-errors
   (if (null head)
       (sb-md5:md5sum-file (native path))
       (with-open-file (in (native path) :element-type '(unsigned-byte 8))
         (let* ((length (file-length in))
                (buffer (make-array (min length (* 2 head)) :element-type '(unsigned-byte 8))))
           (if (<= length (* 2 head))
               (read-sequence buffer in)
               (progn (read-sequence buffer in :end head)
                      (file-position in (- length head))
                      (read-sequence buffer in :start head)))
           (sb-md5:md5sum-sequence buffer))))))

(defun same-bytes-p (a b)
  "Whether files A and B are the same, byte for byte (cmp)."
  (eql 0 (sb-ext:process-exit-code
          (sb-ext:run-program "cmp" (list "-s" "--" a b) :search t :output nil :error nil :wait t))))

(defun group-by (items key &key (test 'equal))
  "ITEMS in lists of those with the same KEY (singles left out)."
  (let ((table (make-hash-table :test test)) (groups '()))
    (dolist (item items) (push item (gethash (funcall key item) table)))
    (maphash (lambda (k v) (declare (ignore k)) (when (rest v) (push v groups))) table)
    groups))

(defun find-duplicates (folder)
  "The groups of files below FOLDER that are the same, byte for byte: ((SIZE
OLDEST COPY ...) ...), the most space wasted first."
  (let ((files '()) (count 0)
        (own-repo (project-root folder)))
    (each-candidate
     folder
     (lambda (path name folder-p)
       (declare (ignore name))
       (when (>= (incf count) *duplicate-files-limit*) (return-from find-duplicates nil))
       (unless folder-p
         (let ((stat (file-stat path :follow nil)))
           (when (and stat (= (stat-type stat) sb-posix:s-ifreg)
                      (>= (sb-posix:stat-size stat) *duplicate-least*)
                      ;; Another repository's files are its own.
                      (let ((root (project-root path)))
                        (or (null root) (equal root own-repo)
                            (not (path-exists-p (join-path root ".git"))))))
             (push (list path (sb-posix:stat-size stat) (sb-posix:stat-mtime stat)) files))))
       nil))
    (let ((groups '()))
      (dolist (same-size (group-by files #'second :test 'eql))
        (dolist (same-head (group-by same-size (lambda (f) (file-md5 (first f) :head 65536)) :test 'equalp))
          (dolist (same-all (group-by same-head (lambda (f) (file-md5 (first f))) :test 'equalp))
            ;; The oldest is kept; each other one must match it byte for byte.
            (let* ((sorted (sort (copy-list same-all) #'< :key #'third))
                   (keep (first (first sorted)))
                   (copies (remove-if-not (lambda (f) (same-bytes-p keep (first f))) (rest sorted))))
              (when copies
                (push (list* (second (first sorted)) keep (mapcar #'first copies)) groups))))))
      (sort groups #'> :key (lambda (g) (* (first g) (length (cddr g))))))))

(defun duplicates-plan (groups)
  "The copies in GROUPS (FIND-DUPLICATES's), to the Trash: steps."
  (loop for (nil nil . copies) in groups
        append (mapcar (lambda (c) (list :trash c)) copies)))
