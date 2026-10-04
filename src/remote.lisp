;;;; remote.lisp — a server's folders over SSH, like local ones.
;;;;
;;;; A server ("user@host:folder", or "host" for your home there) is mounted
;;;; with sshfs in the cache folder's esploro/remote/ and shown there: every
;;;; part of Esploro works on it as on a local folder (plans, the Trash,
;;;; undo). A passphrase or password is asked with a small dialog (zenity),
;;;; through this command itself as ssh's SSH_ASKPASS. What's connected is
;;;; kept in the state folder's remotes.lisp: (:remote TARGET MOUNTPOINT).
;;;; ESPLORO_SSHFS_OPTIONS adds sshfs -o options (the tests' own server).

(in-package #:esploro)

(defun remotes-folder ()
  (join-path (env-folder "XDG_CACHE_HOME" ".cache") "esploro" "remote"))

(defun remotes-file () (join-path (state-folder) "remotes.lisp"))

(defun open-remotes ()
  "The servers connected now: ((TARGET . MOUNTPOINT) ...)."
  (let ((file (remotes-file)))
    (when (path-exists-p file)
      (loop for form in (ignore-errors (read-plan-file file))
            when (and (consp form) (eq (first form) :remote) (stringp (second form)) (stringp (third form))
                      (mounted-p (third form)))
              collect (cons (second form) (third form))))))

(defun write-open-remotes (open)
  (write-forms (remotes-file) (mapcar (lambda (o) (list :remote (car o) (cdr o))) open)
               :comment ";; Esploro's servers connected: (:remote TARGET MOUNTPOINT)."))

(defun parse-remote (text)
  "TEXT (user@host:folder, host:folder, host, or ssh://user@host:port/folder)
as (VALUES SSHFS-TARGET PORT NAME); an error when it isn't one."
  (let ((text (string-trim " " text)) (port nil))
    (when (and (> (length text) 6) (string= "ssh://" text :end2 6))
      (let* ((rest (subseq text 6))
             (slash (position #\/ rest))
             (hostpart (subseq rest 0 slash))
             (folder (if slash (subseq rest slash) ""))
             (colon (position #\: hostpart :from-end t)))
        (when colon
          (setf port (parse-integer hostpart :start (1+ colon) :junk-allowed t)
                hostpart (subseq hostpart 0 colon)))
        (setf text (format nil "~a:~a" hostpart folder))))
    (let* ((colon (position #\: text))
           (host (subseq text 0 colon))
           (folder (if colon (subseq text (1+ colon)) "")))
      (when (or (string= host "") (find #\Space host) (find #\/ host) (char= (char host 0) #\-))
        (error "~a: a server is user@host, host, or user@host:folder" text))
      (values (format nil "~a:~a" host folder) port
              (if (string= folder "") host (format nil "~a ~a" host (path-name (string-right-trim "/" folder))))))))

(defun askpass-environment ()
  "ssh's environment for asking a passphrase with a dialog: this command."
  (let ((self (sb-ext:native-namestring sb-ext:*runtime-pathname*)))
    (append (when (and self (path-exists-p self))
              (list (concatenate 'string "SSH_ASKPASS=" self)
                    "SSH_ASKPASS_REQUIRE=force" "ESPLORO_ASKPASS=1"))
            (sb-ext:posix-environ))))

(defun askpass (prompt)
  "As ssh's SSH_ASKPASS: PROMPT in a dialog; what's typed on standard output.
A yes/no question (a new server's key) is a question; the rest a password."
  (if (search "yes/no" prompt)
      ;; zenity --question answers by its exit code alone.
      (format t "~a~%" (if (eql 0 (sb-ext:process-exit-code
                                   (sb-ext:run-program "zenity" (list "--question" "--title=Esploro: a server" "--text" prompt)
                                                       :search t :output nil :error nil :wait t)))
                           "yes" "no"))
      (write-string (with-output-to-string (s)
                      (sb-ext:run-program "zenity" (list "--password" "--title" prompt)
                                          :search t :output s :error nil :input nil :wait t))))
  (finish-output))

(defun open-remote (text)
  "Connect to the server TEXT says (user@host:folder): its mount point, the
one already there when it's connected. An error saying why when it can't."
  (multiple-value-bind (target port name) (parse-remote text)
    (let ((open (open-remotes)))
      (or (cdr (assoc target open :test #'string=))
          (progn
            (unless (path-exists-p "/usr/bin/sshfs") (error "connecting needs sshfs (vikix esploro setup brings it)"))
            (ensure-folder (remotes-folder))
            (let* ((point (loop for n from 1
                                for point = (join-path (remotes-folder) (if (= n 1) name (format nil "~a ~d" name n)))
                                unless (and (path-exists-p point) (or (mounted-p point) (folder-names point)))
                                  return point))
                   (options (format nil "reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,idmap=user~@[,Port=~d~]~@[,~a~]"
                                    port (let ((extra (sb-posix:getenv "ESPLORO_SSHFS_OPTIONS")))
                                           (and extra (plusp (length extra)) extra))))
                   (err (make-string-output-stream)))
              (ensure-folder point)
              (let ((code (sb-ext:process-exit-code
                           (sb-ext:run-program "timeout" (list "90" "/usr/bin/sshfs" "-o" options target point)
                                               :search t :output nil :error err :input nil :wait t
                                               :environment (askpass-environment)))))
                (unless (and (eql code 0) (mounted-p point))
                  (ignore-errors (sb-posix:rmdir point))
                  (let ((why (string-trim '(#\Newline #\Space) (get-output-stream-string err))))
                    (error "couldn't connect to ~a~@[ (~a)~]" (string-right-trim ":" target) (and (plusp (length why)) why)))))
              (write-open-remotes (append open (list (cons target point))))
              point))))))

(defun close-remote (point)
  "Disconnect the server at POINT: T, or NIL when something still has a file
of it open."
  (let ((open (open-remotes)))
    (when (mounted-p point)
      (let ((code (sb-ext:process-exit-code
                   (sb-ext:run-program (if (path-exists-p "/usr/bin/fusermount3") "fusermount3" "fusermount")
                                       (list "-u" "--" point) :search t :output nil :error nil :wait t))))
        (unless (eql code 0) (return-from close-remote nil))))
    (ignore-errors (sb-posix:rmdir point))
    (write-open-remotes (remove point open :key #'cdr :test #'string=))
    t))

(defun known-servers ()
  "Servers you've used: ~/.ssh/config's Host names (not patterns) and
known_hosts' (those not hashed), each once."
  (let ((names '()))
    (let ((config (join-path (home-folder) ".ssh" "config")))
      (when (path-exists-p config)
        (dolist (line (split-on #\Newline (or (ignore-errors (file-text config)) "")))
          (let ((line (string-trim '(#\Space #\Tab) line)))
            (when (and (> (length line) 5) (string-equal "host " line :end2 5))
              (dolist (h (split-on #\Space (subseq line 5)))
                (unless (or (string= h "") (find #\* h) (find #\? h)) (pushnew h names :test #'string=))))))))
    (let ((known (join-path (home-folder) ".ssh" "known_hosts")))
      (when (path-exists-p known)
        (dolist (line (split-on #\Newline (or (ignore-errors (file-text known)) "")))
          (let ((first (subseq line 0 (or (position #\Space line) (length line)))))
            (unless (or (string= first "") (char= (char first 0) #\|) (char= (char first 0) #\#)
                        (search "github.com" first) (search "gitlab.com" first))
              (dolist (h (split-on #\, first))
                (let ((h (string-trim "[]" (subseq h 0 (or (position #\] h) (length h))))))
                  (unless (string= h "") (pushnew h names :test #'string=)))))))))
    (nreverse names)))
