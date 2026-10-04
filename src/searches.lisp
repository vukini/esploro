;;;; searches.lisp — folders that are questions: the files that match.
;;;;
;;;; A query is plain data: (:name "text") a name holding text, any case;
;;;; (:glob "*.pdf"); (:kind :pdf); (:newer-than DAYS), (:older-than
;;;; DAYS); (:larger-than BYTES), (:smaller-than BYTES); and (:and ...),
;;;; (:or ...), (:not Q). People type words instead, read by PARSE-QUERY:
;;;; "report kind:pdf newer:7 larger:1M -draft". A search is a query and
;;;; the folder it looks below; named ones are kept in
;;;; ~/.config/esploro/searches.lisp, and show down the side in Esploro.

(in-package #:esploro)

(defparameter *search-limit* 1000 "At most this many files found.")
(defparameter *search-visit-limit* 300000 "At most this many looked at.")
(defparameter *search-skipped* '("node_modules" "__pycache__" "target")
  "Folders not looked in (nor are hidden ones, a dot in front).")

;;; --- Words to a query, and back ---------------------------------------------

(defun parse-size (text)
  "Bytes in TEXT: 500, 10k, 2M, 1G (any case); NIL when it isn't one."
  (let* ((n (length text))
         (unit (and (plusp n) (char-downcase (char text (1- n)))))
         (scale (case unit (#\k 1024) (#\m (* 1024 1024)) (#\g (* 1024 1024 1024)) (t nil)))
         (digits (if scale (subseq text 0 (1- n)) text)))
    (and (plusp (length digits)) (every #'digit-char-p digits)
         (* (parse-integer digits) (or scale 1)))))

(defun glob-p (word) (or (find #\* word) (find #\? word)))

(defun parse-word (word)
  (let* ((colon (position #\: word))
         (key (and colon (string-downcase (subseq word 0 colon))))
         (value (and colon (subseq word (1+ colon)))))
    (flet ((number-or-fail (v)
             (or (and (plusp (length v)) (every #'digit-char-p v) (parse-integer v))
                 (error "~a: a number of days" word)))
           (size-or-fail (v) (or (parse-size v) (error "~a: a size, like 10M" word))))
      (cond ((and (> (length word) 1) (char= (char word 0) #\-))
             (list :not (parse-word (subseq word 1))))
            ((equal key "kind")
             (let ((kind (find value (cons "folder" (cons "file" (mapcar (lambda (k) (string-downcase (car k))) *kinds-by-type*)))
                               :test #'string-equal)))
               (unless kind (error "~a: kinds are folder, file, ~{~(~a~)~^, ~}" word (mapcar #'car *kinds-by-type*)))
               (list :kind (intern (string-upcase kind) :keyword))))
            ((equal key "newer") (list :newer-than (number-or-fail value)))
            ((equal key "older") (list :older-than (number-or-fail value)))
            ((equal key "larger") (list :larger-than (size-or-fail value)))
            ((equal key "smaller") (list :smaller-than (size-or-fail value)))
            ((equal key "tag")
             (if (parse-tags value) (list :tag (first (parse-tags value)))
                 (error "~a: tag:NAME, a tag of yours" word)))
            ((equal key "has")
             (if (plusp (length value)) (list :has value) (error "~a: has:WORD, a word inside the files" word)))
            ((glob-p word) (list :glob word))
            (t (list :name word))))))

(defun parse-query (text)
  "TEXT as a query: an s-expression when it starts with (, else words, all of
which must hold. Signals an error saying what's wrong."
  (let ((text (string-trim " " text)))
    (cond ((string= text "") (error "nothing to look for"))
          ((char= (char text 0) #\()
           (let ((forms (read-plan text)))
             (unless (= (length forms) 1) (error "one query, in parentheses"))
             (check-query (first forms))))
          (t (let ((words (loop with start = 0
                                for space = (position #\Space text :start start)
                                for word = (subseq text start space)
                                unless (string= word "") collect word
                                while space do (setf start (1+ space)))))
               (if (rest words)
                   ;; What's inside a file is asked last: the rest is quick.
                   (let ((parsed (mapcar #'parse-word words)))
                     (cons :and (append (remove :has parsed :key #'first)
                                        (remove :has parsed :key #'first :test-not #'eq))))
                   (parse-word (first words))))))))

(defun check-query (query)
  "QUERY, when it's one; else an error saying which part isn't."
  (flet ((bad () (error "not a query: ~s" query)))
    (unless (consp query) (bad))
    (case (first query)
      ((:and :or) (mapc #'check-query (rest query)))
      (:not (unless (= (length query) 2) (bad)) (check-query (second query)))
      ((:name :glob :has :tag) (unless (and (= (length query) 2) (stringp (second query)) (plusp (length (second query)))) (bad)))
      (:kind (unless (and (= (length query) 2) (keywordp (second query))) (bad)))
      ((:newer-than :older-than :larger-than :smaller-than)
       (unless (and (= (length query) 2) (realp (second query)) (>= (second query) 0)) (bad)))
      (t (bad)))
    query))

(defun size-words (bytes)
  (loop for (unit . scale) in '(("G" . 1073741824) ("M" . 1048576) ("k" . 1024))
        when (and (>= bytes scale) (zerop (mod bytes scale)))
          do (return (format nil "~d~a" (/ bytes scale) unit))
        finally (return (format nil "~d" bytes))))

(defun query-words (query)
  "QUERY as the words that make it, when there are such words; else its
s-expression."
  (labels ((word (q)
             (case (first q)
               (:name (second q))
               (:glob (second q))
               (:kind (format nil "kind:~(~a~)" (second q)))
               (:newer-than (format nil "newer:~d" (second q)))
               (:older-than (format nil "older:~d" (second q)))
               (:larger-than (format nil "larger:~a" (size-words (second q))))
               (:smaller-than (format nil "smaller:~a" (size-words (second q))))
               (:has (format nil "has:~a" (second q)))
               (:tag (format nil "tag:~a" (second q)))
               (:not (let ((w (word (second q)))) (and w (concatenate 'string "-" w))))))
           (simple-p (w) (and w (not (find #\Space w)) (not (find #\( w)))))
    (let ((words (mapcar #'word (if (eq (first query) :and) (rest query) (list query)))))
      (if (every #'simple-p words)
          (format nil "~{~a~^ ~}" words)
          (with-standard-io-syntax (let ((*print-case* :downcase)) (prin1-to-string query)))))))

;;; --- Matching ------------------------------------------------------------------

(defun glob-match-p (pattern name &optional (p 0) (n 0))
  "Whether NAME fits PATTERN: * any run of characters, ? any one; any case."
  (cond ((= p (length pattern)) (= n (length name)))
        ((char= (char pattern p) #\*)
         (loop for i from n to (length name) thereis (glob-match-p pattern name (1+ p) i)))
        ((= n (length name)) nil)
        ((or (char= (char pattern p) #\?) (char-equal (char pattern p) (char name n)))
         (glob-match-p pattern name (1+ p) (1+ n)))))

(defun query-match-p (query name kind stat now &optional has tags)
  "Whether QUERY holds for the file NAME of KIND. STAT, called only when a
time or a size is asked, gives the file's lstat; HAS, only when what's
inside is asked, whether the file holds a word; TAGS, only when a tag is
asked, the file's tags."
  (labels ((m (q)
             (ecase (first q)
               (:and (every #'m (rest q)))
               (:or (some #'m (rest q)))
               (:not (not (m (second q))))
               (:name (search (second q) name :test #'char-equal))
               (:glob (glob-match-p (second q) name))
               (:kind (kind-is kind (second q)))
               (:newer-than (let ((st (funcall stat))) (and st (> (sb-posix:stat-mtime st) (- now (* 86400 (second q)))))))
               (:older-than (let ((st (funcall stat))) (and st (< (sb-posix:stat-mtime st) (- now (* 86400 (second q)))))))
               (:larger-than (let ((st (funcall stat))) (and st (not (eq kind :folder)) (> (sb-posix:stat-size st) (second q)))))
               (:smaller-than (let ((st (funcall stat))) (and st (not (eq kind :folder)) (< (sb-posix:stat-size st) (second q)))))
               (:has (and has (not (eq kind :folder)) (funcall has (second q))))
               (:tag (and tags (member (second q) (funcall tags) :test #'string-equal) t)))))
    (m query)))

(defun unix-now () (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun fd-program ()
  (find-if (lambda (p) (path-exists-p p)) '("/usr/bin/fd" "/usr/local/bin/fd" "/usr/bin/fdfind")))

(defun fd-paths (fd root type)
  "What fd finds below ROOT of TYPE (\"d\" folders, \"f\" the rest): not
hidden, not what .gitignore leaves out, nor *SEARCH-SKIPPED*."
  (let* ((out (with-output-to-string (s)
                (sb-ext:run-program fd (append (list "--color" "never" "--print0" "--type" type)
                                               (when (string= type "f") (list "--type" "l"))
                                               (loop for x in *search-skipped* append (list "--exclude" x))
                                               (list "." root))
                                    :output s :error nil :input nil)))
         (paths '()) (start 0))
    (loop for end = (position (code-char 0) out :start start)
          while end
          do (push (subseq out start end) paths) (setf start (1+ end)))
    paths))

(defun each-candidate (root fn)
  "FN on each (PATH NAME FOLDER-P) below ROOT, until it returns :stop: from
fd when it's there (quick, and keeps to .gitignore), else a walk."
  (let ((fd (fd-program)))
    (if fd
        (block found
          (dolist (type '("d" "f"))
            (dolist (path (fd-paths fd root type))
              (let ((path (string-right-trim "/" path)))
                (when (eq (funcall fn path (path-name path) (string= type "d")) :stop)
                  (return-from found))))))
        (labels ((walk (folder)
                   (dolist (name (ignore-errors (folder-names folder)))
                     (unless (char= (char name 0) #\.)
                       (let* ((path (join-path folder name))
                              (stat (file-stat path :follow nil))
                              (folder-p (and stat (= (stat-type stat) sb-posix:s-ifdir))))
                         (when stat
                           (when (eq (funcall fn path name folder-p) :stop) (return-from each-candidate))
                           (when (and folder-p (not (member name *search-skipped* :test #'string=)))
                             (walk path))))))))
          (walk root)))))

(defun query-has-words (query)
  "The words QUERY looks for inside files."
  (case (first query)
    ((:and :or) (remove-duplicates (mapcan #'query-has-words (rest query)) :test #'string-equal))
    (:not (query-has-words (second query)))
    (:has (list (second query)))))

(defun rg-files (word root)
  "The files below ROOT holding WORD (any case), as ripgrep finds them (not
hidden, not what .gitignore leaves out): a table of their paths."
  (let ((table (make-hash-table :test 'equal))
        (out (with-output-to-string (s)
               (sb-ext:run-program "rg" (list "-l" "-0" "-i" "-F" "--max-filesize" "20M" "-e" word "--" root)
                                   :search t :output s :error nil :input nil))))
    (loop with start = 0
          for end = (position (code-char 0) out :start start)
          while end
          do (setf (gethash (subseq out start end) table) t)
             (setf start (1+ end)))
    table))

(defun pdf-text (path)
  "PATH's text (a PDF), made with pdftotext once and kept; NIL when it can't be."
  (let* ((stat (file-stat path))
         (out (join-path (cache-folder) "text"
                         (format nil "~(~{~2,'0x~}~).txt"
                                 (coerce (sb-md5:md5sum-string
                                          (format nil "~a ~d ~d" path (and stat (sb-posix:stat-size stat))
                                                  (and stat (sb-posix:stat-mtime stat)))
                                          :external-format :utf-8)
                                         'list)))))
    (unless (path-exists-p out)
      (ensure-folder (path-parent out))
      (tool-runs "pdftotext" "-q" "-enc" "UTF-8" path out))
    (and (path-exists-p out)
         (ignore-errors
          (with-open-file (in (native out) :external-format '(:utf-8 :replacement #\?))
            (let ((text (make-string (min (file-length in) 20000000))))
              (subseq text 0 (read-sequence text in))))))))

(defun text-file-has-p (path word)
  "Whether PATH (text) holds WORD, any case, in its first 20 MB: without ripgrep."
  (ignore-errors
   (with-open-file (in (native path) :external-format '(:utf-8 :replacement #\?))
     (let ((text (make-string (min (file-length in) 20000000))))
       (search word (subseq text 0 (read-sequence text in)) :test #'char-equal)))))

(defun has-test (query root)
  "For QUERY's words inside files: a function of a path, giving a function of
a word, or NIL when QUERY asks nothing inside."
  (let ((words (query-has-words query)))
    (when words
      (let ((tables (and (program-p "rg")
                         (mapcar (lambda (w) (cons w (rg-files w root))) words)))
            (pdfs (make-hash-table :test 'equal)))
        (lambda (path)
          (lambda (word)
            (case (type-kind (path-name path))
              (:pdf (let ((text (or (gethash path pdfs)
                                    (setf (gethash path pdfs) (or (and (program-p "pdftotext") (pdf-text path)) "")))))
                      (and (search word text :test #'char-equal) t)))
              (t (if tables
                     (and (gethash path (cdr (assoc word tables :test #'string-equal))) t)
                     (and (kind-is (type-kind (path-name path)) :text) (text-file-has-p path word) t))))))))))

(defun run-query (query root &key (limit *search-limit*) (visit-limit *search-visit-limit*))
  "The paths below ROOT that QUERY matches, newest first, and whether there
were more than were looked at or kept (T then)."
  (let ((found '()) (count 0) (visited 0) (more nil) (now (unix-now))
        (has (has-test query root)))
    (each-candidate
     root
     (lambda (path name folder-p)
       (if (or (>= count limit) (>= visited visit-limit))
           (progn (setf more t) :stop)
           (let ((stat :unknown))
             (incf visited)
             (when (query-match-p query name (if folder-p :folder (type-kind name))
                                  (lambda () (if (eq stat :unknown) (setf stat (file-stat path :follow nil)) stat))
                                  now
                                  (and has (funcall has path))
                                  (lambda () (file-tags path)))
               (incf count)
               (push path found))
             nil))))
    (values (mapcar #'cdr (sort (mapcar (lambda (p) (cons (let ((st (file-stat p :follow nil))) (if st (sb-posix:stat-mtime st) 0)) p))
                                        found)
                                #'> :key #'car))
            more)))

;;; --- Kept by name -------------------------------------------------------------

(defun searches-file ()
  (join-path (env-folder "XDG_CONFIG_HOME" ".config") "esploro" "searches.lisp"))

(defun read-searches ()
  "Your named searches, as (NAME QUERY ROOT)."
  (let ((file (searches-file)))
    (when (path-exists-p file)
      (loop for form in (ignore-errors (read-plan-file file))
            when (and (consp form) (eq (first form) :search) (stringp (second form))
                      (ignore-errors (check-query (third form)))
                      (stringp (fourth form)) (normalize-path (fourth form)))
              collect (list (second form) (third form) (fourth form))))))

(defun write-searches (searches)
  (write-forms (searches-file)
               (mapcar (lambda (s) (cons :search s)) searches)
               :comment ";; Esploro's searches: folders that are questions (Searches, down the side).
;; (:search \"NAME\" QUERY \"/looks/below\"); a query is (:name \"text\"), (:glob \"*.pdf\"),
;; (:kind :pdf), (:newer-than DAYS), (:older-than DAYS), (:larger-than BYTES),
;; (:smaller-than BYTES), or (:and ...), (:or ...), (:not ...) of them.")
  searches)

(defun save-search (name query root)
  (write-searches (append (remove name (read-searches) :key #'first :test #'string=)
                          (list (list name query root)))))

(defun forget-search (name)
  (write-searches (remove name (read-searches) :key #'first :test #'string=)))
