;;;; tests/run.lisp — Esploro's core, without a window: sbcl --script tests/run.lisp
;;;;
;;;; Everything happens in a folder of its own, with HOME, the Trash and
;;;; the journal inside it, and nothing asks the real desktop: the Swank
;;;; port is 9 (nothing listens there) unless a test starts its own
;;;; stand-in, and Emacs's socket doesn't exist.

(require :asdf)
(require :sb-posix)

(defvar *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(asdf:load-asd (merge-pathnames "../esploro.asd" *here*))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "esploro/core" :verbose nil))

(defpackage #:esploro.tests (:use #:cl #:esploro))
(in-package #:esploro.tests)

(defvar *failures* 0)
(defvar *checks* 0)

(defmacro check (name form)
  `(progn
     (incf *checks*)
     (unless (handler-case ,form (error (e) (format t "  error: ~a~%" e) nil))
       (incf *failures*)
       (format t "FAIL: ~a~%" ,name))))

(defmacro signals (condition form)
  `(handler-case (progn ,form nil)
     (,condition () t)))

;;; A scratch world.
(defvar *top* (string-right-trim "/" (sb-posix:mkdtemp "/tmp/esploro-test-XXXXXX")))
(defvar *home* (esploro::join-path *top* "home"))
(sb-posix:mkdir *home* #o700)
(sb-posix:setenv "HOME" *home* 1)
(sb-posix:setenv "XDG_DATA_HOME" (esploro::join-path *home* ".local/share") 1)
(sb-posix:setenv "XDG_STATE_HOME" (esploro::join-path *home* ".local/state") 1)
(sb-posix:setenv "ESPLORO_SWANK_PORT" "9" 1)
(sb-posix:unsetenv "VIKIX_SWANK_PORT")
(sb-posix:setenv "EMACS_SOCKET_NAME" "/nonexistent/emacs-server" 1)

(defun p (&rest names) (apply #'join-path *home* names))
(defun make-file (path &optional (text "hello"))
  (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede)
    (write-string text out))
  path)
(defun file-text (path)
  (with-open-file (in (sb-ext:parse-native-namestring path))
    (read-line in nil "")))
(defun mkdir (path) (sb-posix:mkdir path #o755) path)
(defun there (path) (path-exists-p path))

;;; --- Paths -------------------------------------------------------------------

(check "normalize takes out . and .. and slashes" (equal (normalize-path "/a//b/./c/../d/") "/a/b/d"))
(check "normalize refuses a relative path" (null (normalize-path "a/b")))
(check "normalize of / is /" (equal (normalize-path "/") "/"))
(check "parent" (and (equal (path-parent "/a/b") "/a") (equal (path-parent "/a") "/")))
(check "name" (equal (path-name "/a/b.txt") "b.txt"))
(check "join" (and (equal (join-path "/" "a") "/a") (equal (join-path "/a" "b" "c") "/a/b/c")))
(check "inside" (and (path-inside-p "/a/b" "/a") (not (path-inside-p "/ab" "/a"))
                     (not (path-inside-p "/a" "/a")) (path-inside-p "/a" "/")))

;;; --- Folders -----------------------------------------------------------------

(let ((f (mkdir (p "list"))))
  (make-file (join-path f "b.png"))
  (make-file (join-path f "A.lisp"))
  (make-file (join-path f ".hidden"))
  (make-file (join-path f "odd [1] *.txt"))
  (mkdir (join-path f "zfolder"))
  (sb-posix:symlink (join-path f "zfolder") (join-path f "link"))
  (let ((entries (list-folder f)))
    (check "folders come first, then names A to z"
           (equal (mapcar #'entry-name entries) '("link" "zfolder" "A.lisp" "b.png" "odd [1] *.txt")))
    (check "hidden files only when asked"
           (and (not (find ".hidden" entries :key #'entry-name :test #'string=))
                (find ".hidden" (list-folder f :hidden t) :key #'entry-name :test #'string=)))
    (check "kinds" (equal (mapcar #'entry-kind entries) '(:folder :folder :lisp :image :text)))
    (check "a link to a folder is a folder, and says it's a link"
           (entry-link-p (first entries)))
    (check "sizes" (eql (entry-size (third entries)) 5))))
(check "lisp is text, anything but a folder is a file"
       (and (kind-is :lisp :text) (kind-is :image :file) (not (kind-is :folder :file)) (kind-is :folder t)))

;;; --- Checking plans ------------------------------------------------------------

(make-file (p "a.txt"))
(make-file (p "b.txt"))
(mkdir (p "docs"))

(check "a good plan has no problems"
       (null (check-plan `((:mkdir ,(p "new")) (:move ,(p "a.txt") ,(p "new" "a.txt"))
                           (:rename ,(p "new" "a.txt") "c.txt") (:copy ,(p "new") ,(p "new2"))
                           (:trash ,(p "b.txt"))))))
(check "steps are checked against what earlier steps leave: a moved file isn't there to move again"
       (search "isn't there" (first (check-plan `((:move ,(p "a.txt") ,(p "docs" "a.txt"))
                                                  (:move ,(p "a.txt") ,(p "a2.txt")))))))
(check "a copy of a folder holds its files, on paper too"
       (null (check-plan `((:copy ,(p "list") ,(p "list2")) (:trash ,(p "list2" "A.lisp"))))))
(check "only the five operations" (search "isn't something a plan can do" (first (check-plan '((:run "/bin/sh"))))))
(check "undo's own steps aren't for plans" (check-plan `((:rmdir ,(p "docs")))))
(check "relative paths are refused" (check-plan '((:trash "a.txt"))))
(check "paths with .. are refused" (check-plan `((:trash ,(concatenate 'string (p "docs") "/../a.txt")))))
(check "a name with a slash is refused" (check-plan `((:rename ,(p "a.txt") "x/y"))))
(check "a target that's there is refused" (search "already there" (first (check-plan `((:copy ,(p "a.txt") ,(p "b.txt")))))))
(check "a folder can't go into itself" (search "inside itself" (first (check-plan `((:move ,(p "docs") ,(p "docs" "in")))))))
(check "the home folder isn't trashed" (check-plan `((:trash ,*home*))))
(check "nor what holds it" (check-plan `((:trash ,*top*))))
(check "a plan that isn't a list" (check-plan "rm -rf /"))
(check "a step that isn't a list" (check-plan '("rm -rf /")))
(check "#. in a plan's text is refused, not run" (signals error (read-plan "#.(sb-ext:exit)")))
(check "a plan's text is read as data" (equal (read-plan "(:trash \"/x\") (:mkdir \"/y\")")
                                              '((:trash "/x") (:mkdir "/y"))))
(check "the plan is refused whole: nothing changed"
       (and (signals plan-refused (apply-plan `((:mkdir ,(p "made")) (:trash ,(p "missing")))))
            (not (there (p "made")))))

;;; --- Applying and undoing ---------------------------------------------------------

(let ((done (apply-plan `((:mkdir ,(p "new"))
                          (:move ,(p "a.txt") ,(p "new" "a.txt"))
                          (:rename ,(p "new" "a.txt") "c.txt")
                          (:copy ,(p "new") ,(p "new2"))
                          (:copy ,(p "list" "odd [1] *.txt") ,(p "odd copy [1].txt"))
                          (:trash ,(p "b.txt"))))))
  (check "all six steps done" (= (length done) 6))
  (check "moved and renamed" (and (not (there (p "a.txt"))) (there (p "new" "c.txt"))))
  (check "a folder copied whole" (equal (file-text (p "new2" "c.txt")) "hello"))
  (check "odd names work" (there (p "odd copy [1].txt")))
  (check "trashed into the freedesktop Trash"
         (and (not (there (p "b.txt"))) (there (join-path (trash-folder) "files" "b.txt"))))
  (check "with its .trashinfo, the path percent-encoded"
         (with-open-file (in (join-path (trash-folder) "info" "b.txt.trashinfo"))
           (and (equal (read-line in) "[Trash Info]")
                (equal (read-line in) (format nil "Path=~a" (p "b.txt"))))))
  (check "the journal has it" (= (length (journal-entries)) 1)))

(make-file (p "b.txt") "second")
(apply-plan `((:trash ,(p "b.txt"))))
(check "a second b.txt in the Trash gets a name of its own"
       (equal (file-text (join-path (trash-folder) "files" "b.txt.2")) "second"))

(check "undo brings the second b.txt back" (and (undo-last) (equal (file-text (p "b.txt")) "second")))
(check "undo refuses to put a file back where another now is"
       (signals plan-refused (undo-last)))
(sb-posix:rename (p "b.txt") (p "b-second.txt"))
(check "undo, again: the first plan put back"
       (and (undo-last)
            (equal (file-text (p "a.txt")) "hello")
            (not (there (p "new")))
            (not (there (p "new2")))
            (not (there (p "odd copy [1].txt")))
            (equal (file-text (p "b.txt")) "hello")
            (not (there (join-path (trash-folder) "files" "b.txt")))
            (not (there (join-path (trash-folder) "info" "b.txt.trashinfo")))))
(check "the copies undone went to the Trash, not gone"
       (there (join-path (trash-folder) "files" "new2")))
(check "nothing left to undo" (null (undo-last)))

;;; A step that fails while running: the restarts.
(mkdir (p "locked"))
(make-file (p "locked" "f.txt"))
(sb-posix:chmod (p "locked") #o555)
(let ((plan `((:mkdir ,(p "made1")) (:move ,(p "locked" "f.txt") ,(p "f.txt")) (:mkdir ,(p "made2")))))
  (check "a failing step offers to put back what's done"
         (and (handler-bind ((step-failed (lambda (c) (declare (ignore c)) (invoke-restart 'undo-done))))
                (null (apply-plan plan)))
              (not (there (p "made1"))) (not (there (p "made2")))))
  (check "or to skip it and go on"
         (and (handler-bind ((step-failed (lambda (c) (declare (ignore c)) (invoke-restart 'skip-step))))
                (= 2 (length (apply-plan plan))))
              (there (p "made1")) (there (p "made2")) (there (p "locked" "f.txt"))))
  (check "what was done is undone, the skipped step left out"
         (and (undo-last) (not (there (p "made1"))) (not (there (p "made2")))))
  (check "or to stop, keeping what's done"
         (and (handler-bind ((step-failed (lambda (c) (declare (ignore c)) (invoke-restart 'stop-here))))
                (= 1 (length (apply-plan plan))))
              (there (p "made1")) (not (there (p "made2"))))))
(sb-posix:chmod (p "locked") #o755)

;;; --- Commands ------------------------------------------------------------------------

(define-file-command test-shout ((path :text))
  "Shout."
  (declare (ignore path)))
(define-file-command test-plan-copy ((path t) :changes t)
  (list (list :copy path (concatenate 'string path ".bak"))))
(check "a command for text is offered for Lisp files"
       (member 'test-shout (commands-for (list (p "list" "A.lisp"))) :key #'file-command-name))
(check "but not for images"
       (not (member 'test-shout (commands-for (list (p "list" "b.png"))) :key #'file-command-name)))
(check "nor when one of several isn't text"
       (not (member 'test-shout (commands-for (list (p "list" "A.lisp") (p "list" "b.png")))
                    :key #'file-command-name)))
(check "a command that changes files gives steps, touching nothing"
       (and (equal (run-file-command 'test-plan-copy (list (p "a.txt")))
                   `((:copy ,(p "a.txt") ,(p "a.txt.bak"))))
            (not (there (p "a.txt.bak")))))
(check "duplicate finds a free name"
       (progn (make-file (p "n.org")) (make-file (p "n copy.org"))
              (equal (run-file-command 'duplicate (list (p "n.org"))) `((:copy ,(p "n.org") ,(p "n copy 2.org"))))))
(check "labels for people" (equal (file-command-label (find-file-command 'open-in-emacs)) "Open in emacs"))
(check "redefining replaces" (progn (define-file-command test-shout ((path t)) nil)
                                    (= 1 (count 'test-shout esploro::*file-commands* :key #'file-command-name))))

;;; --- StumpWM, and where files are open --------------------------------------------------

(check "no StumpWM: unreachable, said plainly" (signals stumpwm-unreachable (stumpwm-eval "(+ 1 2)")))

;;; A stand-in Swank: checks the password, answers one :emacs-rex.
(defun stand-in-swank (answer)
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
    (sb-bsd-sockets:socket-bind server #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen server 1)
    (let ((port (nth-value 1 (sb-bsd-sockets:socket-name server)))
          (got '()))
      (values port
              (sb-thread:make-thread
               (lambda ()
                 (let* ((client (sb-bsd-sockets:socket-accept server))
                        (s (sb-bsd-sockets:socket-make-stream client :input t :output t
                                                                     :element-type '(unsigned-byte 8))))
                   (push (esploro::swank-receive s) got)
                   (push (esploro::swank-receive s) got)
                   (esploro::swank-send s "(:indentation-update ((\"x\" . 1)))")
                   (esploro::swank-send s (format nil "(:return (:ok (~a \"nil\")) 1)" (esploro::lisp-string answer)))
                   (finish-output s)
                   (sb-bsd-sockets:socket-close client)
                   (sb-bsd-sockets:socket-close server)
                   (reverse got))))))))

(make-file (p ".slime-secret") (format nil "sesame~%"))
(multiple-value-bind (port thread) (stand-in-swank "(:OK (1 \"two\" (3)))")
  (sb-posix:setenv "ESPLORO_SWANK_PORT" (princ-to-string port) 1)
  (check "StumpWM's answer, read as data" (equal (stumpwm-eval "(list 1 \"two\" '(3))") '(1 "two" (3))))
  (let ((got (sb-thread:join-thread thread :default nil)))
    (check "the password first, as it is" (equal (first got) "sesame"))
    (check "the form, in the STUMPWM package" (and (search "(list 1 \\\"two\\\" '(3))" (second got))
                                                   (search "\"STUMPWM\"" (second got))))))
(multiple-value-bind (port thread) (stand-in-swank "(:ERROR \"unbound variable\")")
  (sb-posix:setenv "ESPLORO_SWANK_PORT" (princ-to-string port) 1)
  (check "an error in StumpWM comes back as unreachable" (signals stumpwm-unreachable (stumpwm-eval "x")))
  (sb-thread:join-thread thread :default nil))
(sb-posix:setenv "ESPLORO_SWANK_PORT" "9" 1)

(let* ((held (make-file (p "held open.txt")))
       (shown (make-file (p "shown.png")))
       (work (mkdir (p "work")))
       ;; Through the environment, so only the last sh has a path in its
       ;; arguments; fd 3 and the folder outlive the exec.
       (shell (sb-ext:run-program "/bin/sh"
                                  (list "-c" "cd \"$W\" && exec 3<\"$H\" && exec sh -c 'sleep 30; true' \"$S\"")
                                  :environment (list* (format nil "W=~a" work) (format nil "H=~a" held)
                                                      (format nil "S=~a" shown) (sb-ext:posix-environ))
                                  :wait nil))
       (pid (sb-ext:process-pid shell))
       (window (esploro::make-window :id 42 :class "Test" :title "t" :group "1" :pid pid)))
  (sleep 0.3)
  (unwind-protect
       (let ((map (scan-where :windows (list window))))
         (check "a file a window's process holds open" (equal (file-where held map) (list (cons window :file))))
         (check "a file it was started on" (equal (file-where shown map) (list (cons window :argument))))
         (check "and from the window, its files"
                (equal (window-files 42 map) (sort (list (cons held :file) (cons shown :argument) (cons work :folder))
                                                   #'string< :key #'car)))
         (check "no window, nothing" (null (file-where (p "a.txt") map))))
    (sb-ext:process-kill shell 15)
    (sb-ext:process-wait shell)))

;;; --- The end -------------------------------------------------------------------------

(sb-ext:run-program "chmod" (list "-R" "u+w" *top*) :search t)
(sb-ext:run-program "rm" (list "-rf" *top*) :search t)
(format t "~d checks, ~d failed~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
