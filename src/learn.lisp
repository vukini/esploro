;;;; learn.lisp — your edits to an agent's plan, as rules for the next one.
;;;;
;;;; When you edit a plan before applying it, the difference is a
;;;; correction: the agent sent a file to one folder and you to another, or
;;;; it would have moved or trashed a file and you left it be. Each becomes a
;;;; rule in words, offered to you to keep in ~/.config/esploro/sorting.md,
;;;; which every agent proposing changes reads (vikix mcp gives it to them).
;;;; Nothing is written there unless you say so; every correction is also
;;;; kept, plain data, in the state folder's corrections.lisp, for later.

(in-package #:esploro)

(defun step-source (step)
  (and (member (first step) '(:move :copy :trash :rename)) (second step)))

(defun step-folder (step)
  "Where STEP sends its file: a folder, :trash, or NIL (a rename, a mkdir)."
  (case (first step)
    ((:move :copy) (path-parent (third step)))
    (:trash :trash)))

(defun plan-corrections (proposed applied)
  "What APPLIED (your plan) does differently from PROPOSED (the agent's), file
by file: (:elsewhere FILE AGENT-FOLDER YOUR-FOLDER), (:left FILE
AGENT-FOLDER) when you took the step out. A folder may be :trash."
  (loop for step in proposed
        for file = (step-source step)
        for agent = (step-folder step)
        for yours = (find file applied :key #'step-source :test #'equal)
        when (and file agent)
          append (cond ((null yours) (list (list :left file agent)))
                       ((not (equal (step-folder yours) agent))
                        (and (step-folder yours)
                             (list (list :elsewhere file agent (step-folder yours))))))))

(defun folder-words (folder)
  (if (eq folder :trash) "the Trash" (short-path folder)))

(defun correction-rule (correction)
  "CORRECTION as a rule in words, for sorting.md: a start, to make general."
  (destructuring-bind (kind file agent &optional yours) correction
    (let ((name (path-name file)))
      (ecase kind
        (:elsewhere
         (if (eq yours :trash)
             (format nil "\"~a\" goes in the Trash, not in ~a." name (folder-words agent))
             (format nil "\"~a\" goes in ~a, not in ~a." name (folder-words yours) (folder-words agent))))
        (:left
         (if (eq agent :trash)
             (format nil "Don't put \"~a\" in the Trash; leave it in ~a." name (short-path (path-parent file)))
             (format nil "Leave \"~a\" in ~a; don't move it to ~a." name (short-path (path-parent file))
                     (folder-words agent))))))))

(defun corrections-file () (join-path (state-folder) "corrections.lisp"))

(defun keep-corrections (corrections)
  "CORRECTIONS added to the state folder's corrections.lisp, dated."
  (when corrections
    (let ((file (corrections-file))
          (stamp (timestamp (get-universal-time) "-" " ")))
      (ensure-folder (path-parent file))
      (with-open-file (out (native file) :direction :output :if-exists :append
                                         :if-does-not-exist :create :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-case* :downcase) (*print-readably* nil))
            (dolist (c corrections)
              (prin1 (list* :correction stamp c) out) (terpri out))))))))

;;; --- sorting.md ------------------------------------------------------------------

(defparameter *learnt-heading* "## Learnt from my corrections")

(defun sorting-file ()
  (join-path (env-folder "XDG_CONFIG_HOME" ".config") "esploro" "sorting.md"))

(defun file-text (path)
  (with-open-file (in (native path) :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun add-sorting-rule (rule)
  "RULE (one line of words) at the end of sorting.md's learnt rules, under a
heading made for them the first time; the file made, when there's none."
  (let* ((file (sorting-file))
         (rule (string-trim '(#\Space #\Tab #\Newline) (substitute #\Space #\Newline rule)))
         (text (string-right-trim
                '(#\Newline)
                (if (path-exists-p file) (file-text file)
                    (format nil "# Where my files go~%~%Rules for any agent that proposes moving, sorting or trashing my files~%(through Esploro). They're mine: change them, add to them.~%"))))
         (heading (search *learnt-heading* text))
         ;; The learnt section ends where the next heading starts, or the file does.
         (end (and heading (or (search (format nil "~%#") text :start2 (+ heading (length *learnt-heading*)))
                               (length text))))
         (line (format nil "- ~a" rule))
         (new (if heading
                  (let ((before (string-right-trim '(#\Newline) (subseq text 0 end))))
                    (format nil "~a~%~a~@[~%~%~a~]~%" before line
                            (and (< end (length text)) (string-left-trim '(#\Newline) (subseq text end)))))
                  (format nil "~a~%~%~a~%~%~a~%" text *learnt-heading* line))))
    (ensure-folder (path-parent file))
    (with-open-file (out (native file) :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string new out))
    file))
