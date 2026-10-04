;;;; recent.lisp — the files you opened lately.
;;;;
;;;; Esploro notes each file it opens (state folder's recent.lisp: (:opened
;;;; PATH TIME)), and reads what GTK programs note in the standard place,
;;;; ~/.local/share/recently-used.xbel. Recent is both, newest first, files
;;;; that are still there.

(in-package #:esploro)

(defparameter *recent-kept* 500 "Esploro's own notes kept, the newest.")

(defun recent-file () (join-path (state-folder) "recent.lisp"))

(defun recent-notes ()
  "Esploro's own: ((PATH . UNIVERSAL-TIME) ...), as written."
  (let ((file (recent-file)))
    (when (path-exists-p file)
      (loop for form in (ignore-errors (read-plan-file file))
            when (and (consp form) (eq (first form) :opened) (stringp (second form)) (integerp (third form)))
              collect (cons (second form) (third form))))))

(defun note-opened (path)
  "PATH was opened now: at the end of Esploro's notes (the oldest go when
there are too many)."
  (ignore-errors
   (let ((file (recent-file)))
     (ensure-folder (path-parent file))
     (let ((notes (recent-notes)))
       (if (> (length notes) (+ *recent-kept* 100))
           (write-forms file (mapcar (lambda (n) (list :opened (car n) (cdr n)))
                                     (append (last notes *recent-kept*) (list (cons path (get-universal-time))))))
           (with-open-file (out (native file) :direction :output :if-exists :append
                                              :if-does-not-exist :create :external-format :utf-8)
             (with-standard-io-syntax
               (let ((*print-case* :downcase))
                 (prin1 (list :opened path (get-universal-time)) out) (terpri out)))))))))

(defun iso-time (text)
  "2026-09-08T04:58:27.152976Z as a universal time; NIL when it isn't one."
  (ignore-errors
   (encode-universal-time (parse-integer text :start 17 :end 19) (parse-integer text :start 14 :end 16)
                          (parse-integer text :start 11 :end 13) (parse-integer text :start 8 :end 10)
                          (parse-integer text :start 5 :end 7) (parse-integer text :start 0 :end 4) 0)))

(defun xml-attribute (tag name)
  "NAME's value in the XML start TAG (a string), &amp; and the like undone."
  (let* ((key (concatenate 'string " " name "=\""))
         (start (search key tag)))
    (when start
      (let* ((from (+ start (length key)))
             (end (position #\" tag :start from))
             (value (subseq tag from end)))
        (loop for (entity . char) in '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'"))
              do (loop for i = (search entity value)
                       while i do (setf value (concatenate 'string (subseq value 0 i) char
                                                           (subseq value (+ i (length entity)))))))
        value))))

(defun xbel-recent (&optional (file (join-path (env-folder "XDG_DATA_HOME" ".local/share") "recently-used.xbel")))
  "What GTK programs opened (FILE, recently-used.xbel): ((PATH . TIME) ...)."
  (when (path-exists-p file)
    (let ((text (ignore-errors (file-text file))))
      (when text
        (loop with start = 0
              for open = (search "<bookmark " text :start2 start)
              while open
              do (setf start (1+ open))
              when (let* ((end (position #\> text :start open))
                          (tag (subseq text open end))
                          (href (xml-attribute tag "href"))
                          (time (or (iso-time (or (xml-attribute tag "visited") ""))
                                    (iso-time (or (xml-attribute tag "modified") "")))))
                     (and href time (> (length href) 7) (string= "file://" href :end2 7)
                          (cons (percent-decode (subseq href 7)) time)))
                collect it)))))

(defun recent-files (&key (limit 100))
  "The files opened lately, newest first, those still there: Esploro's
notes and GTK's, each file once (at its latest)."
  (let ((latest (make-hash-table :test 'equal)))
    (dolist (n (append (recent-notes) (xbel-recent)))
      (when (> (cdr n) (gethash (car n) latest 0))
        (setf (gethash (car n) latest) (cdr n))))
    (let ((all '()))
      (maphash (lambda (path time)
                 (when (and (path-exists-p path) (not (directory-p path)))
                   (push (cons path time) all)))
               latest)
      (mapcar #'car (subseq (sort all #'> :key #'cdr) 0 (min limit (length all)))))))
