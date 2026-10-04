;;;; archives.lisp — an archive opened like a folder, read-only.
;;;;
;;;; A zip, a tarball, a 7z or an ISO is mounted with archivemount (FUSE,
;;;; read-only) in the cache folder's esploro/archives/, and shown there:
;;;; looked at, previewed, opened and copied out of like any folder. Nothing
;;;; is written inside: a plan that would is refused, saying to copy out or
;;;; Extract Here. The window closes an archive (unmounts it) when no view
;;;; shows it any more. What's open is kept in the state folder's
;;;; archives.lisp: (:open ARCHIVE MOUNTPOINT).

(in-package #:esploro)

(defun archives-folder ()
  (join-path (env-folder "XDG_CACHE_HOME" ".cache") "esploro" "archives"))

(defun archives-file () (join-path (state-folder) "archives.lisp"))

(defun mounted-p (folder)
  "Whether FOLDER is a mount point now (/proc/self/mounts)."
  (with-open-file (in "/proc/self/mounts" :external-format :latin-1)
    (loop for line = (read-line in nil)
          while line
          thereis (let* ((start (1+ (or (position #\Space line) -1)))
                         (end (position #\Space line :start start)))
                    ;; Spaces in the path are written \040 there.
                    (string= (unoctal (subseq line start end)) folder)))))

(defun unoctal (text)
  "TEXT with /proc/mounts's \\ooo escapes undone."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length text))
          do (if (and (char= (char text i) #\\) (<= (+ i 4) (length text))
                      (every #'digit-char-p (subseq text (1+ i) (+ i 4))))
                 (progn (write-char (code-char (parse-integer text :start (1+ i) :end (+ i 4) :radix 8)) out)
                        (incf i 4))
                 (progn (write-char (char text i) out) (incf i))))))

(defun open-archives ()
  "The archives open now: ((ARCHIVE . MOUNTPOINT) ...), what isn't mounted
any more left out."
  (let ((file (archives-file)))
    (when (path-exists-p file)
      (loop for form in (ignore-errors (read-plan-file file))
            when (and (consp form) (eq (first form) :open) (stringp (second form)) (stringp (third form))
                      (mounted-p (third form)))
              collect (cons (second form) (third form))))))

(defun write-open-archives (open)
  (write-forms (archives-file) (mapcar (lambda (o) (list :open (car o) (cdr o))) open)
               :comment ";; Esploro's archives open read-only: (:open ARCHIVE MOUNTPOINT)."))

(defun archive-program ()
  (find-if #'path-exists-p '("/usr/bin/archivemount" "/usr/local/bin/archivemount")))

(defun open-archive (archive)
  "Mount ARCHIVE read-only, or find it mounted: its mount point. Signals an
error saying why when it can't."
  (let ((open (open-archives)))
    (or (cdr (assoc archive open :test #'string=))
        (let ((program (archive-program)))
          (unless program (error "opening archives needs archivemount (vikix esploro setup brings it)"))
          (ensure-folder (archives-folder))
          (let ((point (loop for n from 1
                             for name = (if (= n 1) (path-name archive) (format nil "~a ~d" (path-name archive) n))
                             for point = (join-path (archives-folder) name)
                             unless (and (path-exists-p point) (or (mounted-p point) (folder-names point)))
                               return point)))
            (ensure-folder point)
            (let ((code (sb-ext:process-exit-code
                         ;; No "--": archivemount passes it on to FUSE, which refuses it.
                         ;; Both paths are whole, so neither starts with a dash.
                         (sb-ext:run-program program (list "-o" "readonly" archive point)
                                             :output nil :error nil :wait t))))
              (unless (and (eql code 0) (mounted-p point))
                (ignore-errors (sb-posix:rmdir point))
                (error "~a couldn't be opened (not an archive archivemount reads?)" (short-path archive))))
            (write-open-archives (append open (list (cons archive point))))
            point)))))

(defun close-archive (point)
  "Unmount the archive at POINT: T, or NIL when something still has a file
of it open (it stays, to close later)."
  (let ((open (open-archives)))
    (when (mounted-p point)
      (let ((code (sb-ext:process-exit-code
                   (sb-ext:run-program "fusermount" (list "-u" "--" point)
                                       :search t :output nil :error nil :wait t))))
        (unless (eql code 0) (return-from close-archive nil))))
    (ignore-errors (sb-posix:rmdir point))
    (write-open-archives (remove point open :key #'cdr :test #'string=))
    t))

(defun archive-of (path)
  "When PATH is in an archive opened read-only: the archive, and the mount
point."
  (loop for (archive . point) in (open-archives)
        when (or (string= path point) (path-inside-p path point))
          return (values archive point)))

(defun archive-step-problem (step)
  "A step that would write inside an opened archive: what to do instead."
  (flet ((inside (path) (and (stringp path) (archive-of path))))
    (let ((op (first step)) (a (second step)) (b (third step)))
      (cond ((and (member op '(:copy :move)) (inside b))
             (format nil "~a is open read-only: Extract Here to change it" (short-path (inside b))))
            ((and (member op '(:mkdir :tag)) (inside a))
             (format nil "~a is open read-only: Extract Here to change it" (short-path (inside a))))
            ((and (member op '(:move :rename :trash)) (inside a))
             (format nil "~a is open read-only: copy files out of it instead" (short-path (inside a))))))))

(pushnew 'archive-step-problem *step-checks*)
