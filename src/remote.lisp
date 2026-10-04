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

(defun ssh-why (target port)
  "Why ssh can't get into TARGET's host, asked without a prompt (BatchMode):
words to say, or NIL."
  (let* ((host (subseq target 0 (position #\: target)))
         (out (with-output-to-string (s)
                (sb-ext:run-program "timeout" (append (list "20" "ssh" "-v" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10")
                                                      (and port (list "-p" (princ-to-string port)))
                                                      ;; As the connection had them (the tests' own server).
                                                      (loop for o in (split-on #\, (or (sb-posix:getenv "ESPLORO_SSHFS_OPTIONS") ""))
                                                            unless (string= o "") append (list "-o" o))
                                                      (list host "true"))
                                    :search t :output nil :error s :input nil :wait t))))
    (cond ((search "Server accepts key" out) nil)  ; the key would do: the passphrase wasn't given
          ((search "Permission denied" out)
           (let ((user (let ((at (search "Authenticating to" out)))
                         (and at (let* ((q (position #\' out :start at)) (q2 (and q (position #\' out :start (1+ q)))))
                                   (and q q2 (subseq out (1+ q) q2)))))))
             (format nil "the server refused your key~@[ for ~a~]~:[~;: name the user, as user@~a~]"
                     user (not (find #\@ host)) host)))
          ((search "Could not resolve hostname" out) "there's no server of that name")
          ((or (search "Connection refused" out) (search "Connection reset" out) (search "Connection closed" out))
           ;; A server that blocks an address after failed logins (fail2ban,
           ;; sshguard) refuses it, or cuts it off before ssh can say anything.
           "the server refuses connections from here (it may have blocked this address after failed tries: wait a while)")
          ((search "timed out" out) "the server doesn't answer")
          ((search "Host key verification failed" out) "its key isn't the one known for it (~/.ssh/known_hosts)"))))

(defun servers-file () (join-path (state-folder) "servers.lisp"))

(defun remember-server (text)
  "TEXT connected: offered first next time."
  (let* ((file (servers-file))
         (old (and (path-exists-p file) (ignore-errors (read-plan-file file))))
         (old (remove-if-not #'stringp old)))
    (write-forms file (cons text (remove text old :test #'string=))
                 :comment ";; Servers Esploro connected to, newest first.")))

(defun remembered-servers ()
  (let ((file (servers-file)))
    (and (path-exists-p file) (remove-if-not #'stringp (ignore-errors (read-plan-file file))))))

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
                  (let ((why (or (ssh-why target port)
                                 (let ((said (string-trim '(#\Newline #\Space) (get-output-stream-string err))))
                                   (and (plusp (length said)) said)))))
                    (error "couldn't connect to ~a~@[: ~a~]" (string-right-trim ":" target) why))))
              (write-open-remotes (append open (list (cons target point))))
              (remember-server (string-trim " " text))
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
  "Servers you've used: those Esploro connected to (with their user), then
~/.ssh/config's Host names (not patterns) and known_hosts' (those not
hashed) that aren't among them, each once."
  (let ((names (reverse (remembered-servers))))
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
    ;; A host already remembered with its user isn't offered again without.
    (let ((all (nreverse names)))
      (remove-if (lambda (h) (and (not (find #\@ h))
                                  (some (lambda (r) (and (search (concatenate 'string "@" h) r) (string/= r h))) all)))
                 all))))
