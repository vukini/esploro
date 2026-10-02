;;;; preview.lisp — what a file looks like, without opening it.
;;;;
;;;; The window shows a preview of the one file selected. Pictures, PDFs
;;;; and videos become a PNG thumbnail (ImageMagick, pdftoppm,
;;;; ffmpegthumbnailer, whichever is there), kept in ~/.cache/esploro so
;;;; each is made once; text shows its first lines; a folder what's in it.

(in-package #:esploro)

(defparameter *thumbnail-size* 640
  "The longest side of a thumbnail, in pixels.")

(defun cache-folder ()
  (join-path (env-folder "XDG_CACHE_HOME" ".cache") "esploro"))

(defun thumbnail-path (path)
  "Where PATH's thumbnail is kept: named by the path, its size and time, so a
changed file gets a new one."
  (let ((stat (file-stat path)))
    (join-path (cache-folder) "thumbs"
               (format nil "~(~{~2,'0x~}~).png"
                       (coerce (sb-md5:md5sum-string
                                (format nil "~a ~d ~d ~d" path *thumbnail-size*
                                        (and stat (sb-posix:stat-size stat))
                                        (and stat (sb-posix:stat-mtime stat)))
                                :external-format :utf-8)
                               'list)))))

(defun tool-runs (program &rest args)
  "Run PROGRAM (for at most 15 s); true when it worked."
  (ignore-errors
   (eql 0 (sb-ext:process-exit-code
           (sb-ext:run-program "timeout" (list* "15" program args)
                               :search t :input nil :output nil :error nil :wait t)))))

(defun program-p (name)
  (some (lambda (dir) (path-exists-p (join-path dir name)))
        (remove "" (split-on #\: (or (sb-posix:getenv "PATH") "")) :test #'string=)))

(defun make-thumbnail (path out)
  (let ((size (princ-to-string *thumbnail-size*))
        (box (format nil "~dx~d>" *thumbnail-size* *thumbnail-size*)))
    (case (path-kind path)
      (:image (cond ((program-p "magick")
                     (tool-runs "magick" (concatenate 'string path "[0]") "-auto-orient" "-thumbnail" box out))
                    ((program-p "convert")
                     (tool-runs "convert" (concatenate 'string path "[0]") "-auto-orient" "-thumbnail" box out))))
      (:pdf (and (program-p "pdftoppm")
                 ;; pdftoppm adds the .png itself.
                 (tool-runs "pdftoppm" "-png" "-singlefile" "-f" "1" "-l" "1" "-scale-to" size
                            path (subseq out 0 (- (length out) 4)))))
      (:video (and (program-p "ffmpegthumbnailer")
                   (tool-runs "ffmpegthumbnailer" "-i" path "-o" out "-s" size))))))

(defun thumbnail (path)
  "A PNG of PATH (a picture, PDF or video), made once and kept; NIL when
there's no way to make one."
  (let ((out (thumbnail-path path)))
    (cond ((path-exists-p out) out)
          (t (ensure-folder (path-parent out))
             (and (make-thumbnail path out) (path-exists-p out) out)))))

(defun text-head (path &key (lines 60) (bytes 16384))
  "PATH's first LINES lines (from its first BYTES), or NIL when it isn't text."
  (ignore-errors
   (with-open-file (in (native path) :element-type '(unsigned-byte 8))
     (let* ((buffer (make-array bytes :element-type '(unsigned-byte 8)))
            (n (read-sequence buffer in)))
       (unless (find 0 buffer :end n)
         (let ((text (sb-ext:octets-to-string buffer :end n :external-format '(:utf-8 :replacement #\?))))
           (with-input-from-string (s text)
             (loop for line = (read-line s nil) while line
                   repeat lines collect (substitute #\Space #\Tab line)))))))))

(defun file-description (path)
  "What file(1) says PATH is."
  (command-output "file" "-b" path))
