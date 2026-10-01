;;;; files.lisp — paths, and what's in a folder.

(in-package #:esploro)

;;; --- Paths ---------------------------------------------------------------

(defun split-path (path)
  (loop with start = 0
        for slash = (position #\/ path :start start)
        for part = (subseq path start slash)
        unless (string= part "") collect part
        while slash
        do (setf start (1+ slash))))

(defun normalize-path (path)
  "PATH as an absolute path without . or .. or doubled or trailing slashes,
or NIL when PATH isn't absolute."
  (when (and (stringp path) (plusp (length path)) (char= (char path 0) #\/))
    (let ((parts '()))
      (dolist (part (split-path path))
        (cond ((string= part "."))
              ((string= part "..") (pop parts))
              (t (push part parts))))
      (format nil "/~{~a~^/~}" (reverse parts)))))

(defun path-parent (path)
  (let ((slash (position #\/ path :from-end t)))
    (if (or (null slash) (zerop slash)) "/" (subseq path 0 slash))))

(defun path-name (path)
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun join-path (folder &rest names)
  (let ((path folder))
    (dolist (name names path)
      (setf path (if (string= path "/")
                     (concatenate 'string "/" name)
                     (concatenate 'string path "/" name))))))

(defun path-inside-p (path folder)
  "True when PATH is somewhere inside FOLDER (not FOLDER itself)."
  (let ((n (length folder)))
    (and (> (length path) n)
         (string= folder path :end2 n)
         (or (string= folder "/") (char= (char path n) #\/)))))

(defun home-folder ()
  (or (normalize-path (sb-posix:getenv "HOME")) "/"))

;;; --- Files ---------------------------------------------------------------

(defun file-stat (path &key (follow t))
  (handler-case (if follow (sb-posix:stat path) (sb-posix:lstat path))
    (sb-posix:syscall-error () nil)))

(defun stat-type (stat)
  (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt))

(defun path-exists-p (path)
  "True when PATH is there, a dangling link included."
  (and (file-stat path :follow nil) t))

(defun directory-p (path)
  (let ((stat (file-stat path)))
    (and stat (= (stat-type stat) sb-posix:s-ifdir))))

;;; A kind is what presentations and commands go by. Each is a :file too,
;;; except :folder; :lisp is a :text as well.
(defparameter *kinds-by-type*
  '((:image "png" "jpg" "jpeg" "gif" "webp" "svg" "bmp" "tif" "tiff" "avif" "heic" "xpm")
    (:video "mp4" "mkv" "webm" "mov" "avi" "m4v")
    (:audio "mp3" "flac" "ogg" "opus" "wav" "m4a")
    (:pdf "pdf" "djvu" "epub")
    (:lisp "lisp" "lsp" "asd" "el" "scm" "ss" "rkt" "clj" "fnl")
    (:text "txt" "md" "org" "rst" "tex" "csv" "json" "toml" "yaml" "yml" "ini" "conf"
     "sh" "bash" "py" "rb" "c" "h" "cc" "cpp" "rs" "go" "js" "ts" "html" "css" "xml"
     "zig" "lua" "hs" "ml" "java" "sql" "mmd")
    (:archive "zip" "tar" "gz" "tgz" "xz" "zst" "bz2" "7z" "rar")))

(defparameter *kind-parents* '((:lisp . :text)))

(defun type-kind (name)
  (let* ((dot (position #\. name :from-end t))
         (type (and dot (plusp dot) (string-downcase (subseq name (1+ dot))))))
    (or (and type
             (car (find-if (lambda (kind) (member type (cdr kind) :test #'string=))
                           *kinds-by-type*)))
        :file)))

(defun path-kind (path)
  "The kind of what's at PATH: :folder, :image, :video, :audio, :pdf, :lisp,
:text, :archive or :file; NIL when nothing is there."
  (cond ((directory-p path) :folder)
        ((path-exists-p path) (type-kind (path-name path)))))

(defun kind-is (kind wanted)
  "True when something of KIND is a WANTED: T is anything, :file anything
but a folder, and :text takes in :lisp."
  (or (eq wanted t)
      (eq kind wanted)
      (and (eq wanted :file) (not (eq kind :folder)))
      (let ((parent (cdr (assoc kind *kind-parents*))))
        (and parent (kind-is parent wanted)))))

;;; --- Folders -------------------------------------------------------------

(defstruct entry
  path name kind size mtime link-p hidden-p)

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~a ~(~a~)" (entry-path entry) (entry-kind entry))))

(defun folder-names (folder)
  "The names in FOLDER, without . and .."
  (let ((dir (sb-posix:opendir folder))
        (names '()))
    (unwind-protect
         (loop for dirent = (sb-posix:readdir dir)
               until (sb-alien:null-alien dirent)
               do (let ((name (sb-posix:dirent-name dirent)))
                    (unless (member name '("." "..") :test #'string=)
                      (push name names))))
      (sb-posix:closedir dir))
    names))

(defun make-entry-for (path)
  (let ((lstat (file-stat path :follow nil)))
    (when lstat
      (let* ((link-p (= (stat-type lstat) sb-posix:s-iflnk))
             ;; A link shows as what it points to; a dangling one as a file.
             (stat (if link-p (or (file-stat path) lstat) lstat))
             (folder-p (= (stat-type stat) sb-posix:s-ifdir))
             (name (path-name path)))
        (make-entry :path path :name name
                    :kind (if folder-p :folder (type-kind name))
                    :size (if folder-p nil (sb-posix:stat-size stat))
                    :mtime (sb-posix:stat-mtime stat)
                    :link-p link-p
                    :hidden-p (char= (char name 0) #\.))))))

(defun list-folder (folder &key hidden)
  "FOLDER's entries, folders first, then by name. Hidden ones (a dot in
front) only when HIDDEN."
  (let ((entries (loop for name in (folder-names folder)
                       for entry = (make-entry-for (join-path folder name))
                       when (and entry (or hidden (not (entry-hidden-p entry))))
                         collect entry)))
    (sort entries (lambda (a b)
                    (let ((fa (eq (entry-kind a) :folder))
                          (fb (eq (entry-kind b) :folder)))
                      (if (eq fa fb)
                          (string-lessp (entry-name a) (entry-name b))
                          fa))))))
