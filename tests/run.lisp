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
  (asdf:load-system "esploro" :verbose nil))

(defpackage #:esploro.tests (:use #:cl #:esploro))
(in-package #:esploro.tests)

;; Nothing a test does may reach the desktop's screen (a viewer opened on
;; a file, a frame): no display at all.
(sb-posix:unsetenv "DISPLAY")
(sb-posix:unsetenv "WAYLAND_DISPLAY")

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
(check "a dozen plans in one second: undo takes back the newest, each in turn"
       (progn (dotimes (i 12) (apply-plan (list (list :mkdir (p (format nil "many~d" i))))))
              (and (loop for i from 11 downto 0
                         always (and (undo-last) (not (there (p (format nil "many~d" i))))
                                     (or (zerop i) (there (p (format nil "many~d" (1- i)))))))
                   (null (undo-last)))))

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
(check "labels for people" (equal (file-command-label (find-file-command 'open-in-emacs)) "Open in Emacs"))
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

;;; Esploro itself, and what started it, are no window's files: run under a
;;; window (as the window asks it from Emacs), it once saw its own argument
;;; and went to that window instead of opening the file.
(let* ((parent (esploro::process-parent (sb-posix:getpid)))
       (window (esploro::make-window :id 7 :class "Shell" :pid parent))
       (map (scan-where :windows (list window)))
       (script (esploro::normalize-path (namestring *load-truename*))))
  (check "the running esploro's own arguments aren't another window's"
         (null (file-where script map))))

;;; --- Previews -------------------------------------------------------------------------

(make-file (p "long.txt") (format nil "one~%two~%three~%four~%"))
(check "a text's first lines" (equal (text-head (p "long.txt") :lines 2) '("one" "two")))
(check "not text: no lines"
       (progn (with-open-file (out (p "bin.dat") :direction :output :element-type '(unsigned-byte 8))
                (write-sequence #(1 0 2 0) out))
              (null (text-head (p "bin.dat")))))
(check "a thumbnail is named by the file, its size and time"
       (let ((before (esploro::thumbnail-path (p "long.txt"))))
         (make-file (p "long.txt") "changed, and longer than before")
         (string/= before (esploro::thumbnail-path (p "long.txt")))))
(check "nothing to make a thumbnail of a text" (null (thumbnail (p "long.txt"))))

;;; --- The Trash, to look in, restore from and empty ------------------------------------

(check "a path percent-encoded comes back as it was"
       (let ((odd "/tmp/a b/ünï%code [x].txt"))
         (string= (esploro::percent-decode (esploro::percent-encode odd)) odd)))
(make-file (p "to-bin one.txt") "one")
(make-file (p "to-bin two.txt") "two")
(apply-plan (list (list :trash (p "to-bin one.txt")) (list :trash (p "to-bin two.txt"))))
(let ((entries (trash-entries)))
  (check "the Trash lists what went in, with where it came from"
         (equal (sort (mapcar #'second (remove-if-not (lambda (e) (search "to-bin" (first e))) entries))
                      #'string<)
                (list (p "to-bin one.txt") (p "to-bin two.txt"))))
  (check "each with when it went"
         (every (lambda (e) (plusp (length (third e)))) entries)))
(check "restoring puts it back where it was"
       (progn (restore-from-trash (list "to-bin one.txt"))
              (and (path-exists-p (p "to-bin one.txt"))
                   (not (find "to-bin one.txt" (trash-entries) :key #'first :test #'string=)))))
(check "and undo puts it in the Trash again"
       (progn (undo-last)
              (and (not (path-exists-p (p "to-bin one.txt")))
                   (find "to-bin one.txt" (trash-entries) :key #'first :test #'string=))))
(check "restoring over a file that's there now is refused, with nothing changed"
       (progn (make-file (p "to-bin two.txt") "a new one")
              (and (signals plan-refused (restore-from-trash (list "to-bin two.txt")))
                   (find "to-bin two.txt" (trash-entries) :key #'first :test #'string=))))
(check "emptying the Trash deletes what's in it"
       (and (plusp (empty-trash))
            (null (trash-entries))
            (null (esploro::folder-names (esploro::join-path (trash-folder) "files")))))

;;; --- The command: what the window reads --------------------------------------------

(defun check-plan-of (text)
  "What the core finds wrong with the plan TEXT: its problems, or NIL."
  (esploro::check-plan (esploro::read-plan text)))

(defun run-cli (args &optional (input ""))
  "ARGS through the command, INPUT on its standard input: (CODE . what it printed)."
  (let* ((code nil)
         (out (with-output-to-string (*standard-output*)
                (with-input-from-string (*standard-input* input)
                  (setf code (esploro::main-1 args))))))
    (cons code (let ((*package* (find-package '#:esploro.read))) (read-from-string out nil)))))

(make-file (p "cli.txt") "cli")
(check "apply reads a plan on its standard input, and says how many steps were done"
       (equal (run-cli (list "apply") (format nil "(:copy ~s ~s)" (p "cli.txt") (p "cli copy.txt")))
              (list 0 :done 1)))
(check "a refused plan says why, with nothing changed"
       (let ((r (run-cli (list "apply") (format nil "(:copy ~s ~s)" (p "cli.txt") (p "cli copy.txt")))))
         (and (eql (car r) 1) (eq (second r) :refused) (consp (third r)))))
(check "undo says what it undid"
       (let ((r (run-cli (list "undo"))))
         (and (eql (car r) 0) (eq (second r) :undone) (not (path-exists-p (p "cli copy.txt"))))))
(check "a step no plan may do is refused"
       (eq (second (run-cli (list "apply") (format nil "(:delete-forever ~s)" (p "cli.txt")))) :refused))
(check "where answers a list for the folder"
       (listp (cdr (run-cli (list "where" *home*)))))
(check "trash-list answers a list"
       (listp (cdr (run-cli (list "trash-list")))))

;;; preview: a thumbnail's path, or :none when there's no way to make one.
(check "preview of text has no thumbnail"
       (equal (run-cli (list "preview" (p "long.txt"))) (list 0 :none)))
(when (esploro::program-p "magick")
  (sb-ext:run-program "magick" (list "-size" "800x500" "xc:steelblue" (p "pic.png"))
                      :search t :output nil :error nil)
  (let ((r (run-cli (list "preview" (p "pic.png")))))
    (check "preview of a picture makes a thumbnail, kept in the cache"
           (and (eq (second r) :thumbnail) (path-exists-p (third r))
                (search "/esploro/thumbs/" (third r))))
    (check "the same again is the same file, not made twice"
           (equal (third (run-cli (list "preview" (p "pic.png")))) (third r)))))

;;; --- The workspace's project, and what's behind a window ----------------------------------

(sb-posix:mkdir (p "proj") #o755)
(sb-posix:mkdir (p "proj/.git") #o755)
(sb-posix:mkdir (p "proj/src") #o755)
(make-file (p "proj/src/a.c") "int main;")
(sb-posix:mkdir (p "notes") #o755)
(make-file (p "notes/log.md") "# Log")
(make-file (p "notes/today.md"))
(check "a file's project is the folder with .git above it" (equal (esploro::project-root (p "proj/src/a.c")) (p "proj")))
(check "or the one with a log.md (vikix project's mark)" (equal (esploro::project-root (p "notes/today.md")) (p "notes")))
(check "a file in no project has none" (null (esploro::project-root (p "long.txt"))))
(make-file (p "run.sh") "#!/bin/sh")
(sb-posix:chmod (p "run.sh") #o755)
(check "a program among the arguments isn't a document" (esploro::program-file-p (p "run.sh")))
(check "a document is" (not (esploro::program-file-p (p "proj/src/a.c"))))
(check "a terminal's log is its own, not a file it's about"
       (and (esploro::scratch-path-p "/tmp/Alacritty-1.log")
            (esploro::scratch-path-p (p ".cache/x"))
            ;; (The tests' home is under /tmp: a document elsewhere.)
            (not (esploro::scratch-path-p "/srv/notes/today.md"))))
(let* ((w1 (esploro::make-window :id 1 :class "Alacritty" :group "4"))
       (w2 (esploro::make-window :id 2 :class "zathura" :group "4"))
       (w3 (esploro::make-window :id 3 :class "Firefox" :group "5"))
       (map (make-hash-table :test 'equal)))
  (setf (gethash (p "proj/src") map) (list (cons w1 :folder))
        (gethash (p "proj/src/a.c") map) (list (cons w2 :argument))
        (gethash (p "notes/today.md") map) (list (cons w3 :argument)))
  (check "the workspace's folder is the project its windows are in"
         (equal (esploro::workspace-folder :windows (list w1 w2 w3) :group "4" :map map) (p "proj")))
  (check "an empty workspace says nothing"
         (null (esploro::workspace-folder :windows (list w1 w2 w3) :group "9" :map map))))

;;; propose: a plan with problems goes back to whoever proposed it, untouched.
(with-open-file (out (p "bad-plan.lisp") :direction :output :if-exists :supersede)
  (format out "(:copy ~s ~s)~%" (p "no-such-file") (p "x")))
(let ((r (run-cli (list "propose" (p "bad-plan.lisp") "tidy up"))))
  (check "a proposed plan with a problem is refused, with it" (and (eql (car r) 1) (eq (second r) :refused))))
(with-open-file (out (p "good-plan.lisp") :direction :output :if-exists :supersede)
  (format out "(:mkdir ~s)~%" (p "proposed-folder")))
(let ((r (run-cli (list "propose" (p "good-plan.lisp")))))
  ;; No Emacs here (its socket doesn't exist): the review can't be shown,
  ;; and nothing is done either way.
  (check "a sound plan goes to Emacs for review, and changes nothing by itself"
         (and (eq (second r) :error) (not (path-exists-p (p "proposed-folder"))))))

;;; --- Commands, and their other doors ---------------------------------------------------

(check "names keep their capitals in a label"
       (equal (file-command-label (find-file-command "show-in-esploro")) "Show in Esploro"))
(esploro::ensure-folder (p "cmd"))
(make-file (p "cmd/note.txt") "a note")
(check "commands suit their kinds: no shrinking a text"
       (let ((names (mapcar (lambda (c) (symbol-name (file-command-name c))) (commands-for (list (p "cmd/note.txt"))))))
         (and (member "COMPRESS" names :test #'string=) (not (member "SHRINK" names :test #'string=)))))
(when (esploro::program-p "zip")
  (check "compress makes a .zip beside it, never over one"
         (progn (run-cli (list "run" "compress" (p "cmd/note.txt")))
                (run-cli (list "run" "compress" (p "cmd/note.txt")))
                (and (path-exists-p (p "cmd/note.txt.zip")) (path-exists-p (p "cmd/note.txt 2.zip")))))
  (check "compress says what it made"
         (let ((r (run-cli (list "run" "compress" (p "cmd/note.txt")))))
           (and (eql (car r) 0) (equal (getf (cddr (cdr r)) :made) (list (p "cmd/note.txt 3.zip"))))))
  (when (or (esploro::program-p "bsdtar") (esploro::program-p "unzip"))
    (check "extract-here unpacks into a new folder beside it"
           (progn (run-cli (list "run" "extract-here" (p "cmd/note.txt.zip")))
                  ;; note.txt is the file itself: the folder is numbered.
                  (path-exists-p (p "cmd/note.txt 2/note.txt"))))))
(make-file (p "cmd/broken.zip") "not a zip at all")
(check "a command that makes nothing says so, naming the file"
       (let ((r (run-cli (list "run" "extract-here" (p "cmd/broken.zip")))))
         (and (eql (car r) 1) (eq (second r) :error) (search "broken.zip" (third r))
              (not (path-exists-p (p "cmd/broken"))))))
(check "the terminal: Esploro's own choice first, then TERMINAL, read when it's wanted"
       (let ((was (sb-posix:getenv "TERMINAL")))
         (sb-posix:setenv "TERMINAL" "xterm" 1)
         (prog1 (and (equal (esploro::terminal-command) "xterm")
                     (progn (sb-posix:setenv "ESPLORO_TERMINAL" "foot" 1)
                            (equal (esploro::terminal-command) "foot"))
                     (let ((esploro::*terminal* "kitty -1")) (equal (esploro::terminal-command) "kitty -1")))
           (sb-posix:unsetenv "ESPLORO_TERMINAL")
           (if was (sb-posix:setenv "TERMINAL" was 1) (sb-posix:unsetenv "TERMINAL")))))
(check "a command that isn't for that kind is refused"
       (eq (second (run-cli (list "run" "shrink" (p "cmd/note.txt")))) :error))
(check "a command that changes files only proposes (no Emacs here: nothing happens)"
       (progn (run-cli (list "run" "trash" (p "cmd/note.txt")))
              (path-exists-p (p "cmd/note.txt"))))
(esploro::ensure-folder (p ".config/esploro"))
(with-open-file (out (p ".config/esploro/commands.lisp") :direction :output :if-exists :supersede)
  (write-string "(define-file-command shout ((path :text)) \"Says it loud.\" (declare (ignore path)) t)" out))
(check "your own commands are offered too"
       (member "shout" (cdr (run-cli (list "commands" (p "cmd/note.txt")))) :key #'first :test #'string=))

;;; --- Recipes: a change done again ----------------------------------------------------

(check "moves into one folder are a recipe"
       (equal (esploro::plan-recipe '((:move "/a/x" "/b/x") (:move "/a/y" "/b/y"))) '(:move-into "/b")))
(check "a folder made, then moves into it, is one too"
       (equal (esploro::plan-recipe '((:mkdir "/b/new") (:move "/a/x" "/b/new/x"))) '(:move-into "/b/new")))
(check "the Trash is one" (equal (esploro::plan-recipe '((:trash "/a/x"))) '(:trash)))
(check "renames aren't" (null (esploro::plan-recipe '((:rename "/a/x" "y")))))
(check "moves into two folders aren't" (null (esploro::plan-recipe '((:move "/a/x" "/b/x") (:move "/a/y" "/c/y")))))
(esploro::ensure-folder (p "rec/in"))
(esploro::ensure-folder (p "rec/archive"))
(make-file (p "rec/in/one.txt")) (make-file (p "rec/in/two.txt")) (make-file (p "rec/archive/two.txt"))
(check "a recipe's plan: a name taken in the folder gets a free one"
       (equal (esploro::recipe-steps (list :move-into (p "rec/archive")) (list (p "rec/in/one.txt") (p "rec/in/two.txt")))
              (list (list :move (p "rec/in/one.txt") (p "rec/archive/one.txt"))
                    (list :move (p "rec/in/two.txt") (p "rec/archive/two 2.txt")))))
(run-cli (list "apply") (format nil "(:move ~s ~s)" (p "rec/in/one.txt") (p "rec/archive/one.txt")))
(check "the last change, as a recipe" (equal (cdr (run-cli (list "recipe" "last")))
                                             (list :recipe (list :move-into (p "rec/archive")) "move into ~/rec/archive")))
(check "it can be kept by name" (eq (second (run-cli (list "recipe" "save" "Archive it"))) :saved))
(check "and listed" (equal (cdr (run-cli (list "recipe" "list"))) (list (list "Archive it" "move into ~/rec/archive"))))
(check "and run on other files, as a plan (undone like any)"
       (and (equal (run-cli (list "recipe" "run" "Archive it" (p "rec/in/two.txt"))) (list 0 :done 1))
            (path-exists-p (p "rec/archive/two 2.txt"))
            (progn (undo-last) (path-exists-p (p "rec/in/two.txt")))))
(check "and forgotten" (progn (run-cli (list "recipe" "forget" "Archive it")) (null (esploro::read-recipes))))

;;; --- Renames by a pattern ------------------------------------------------------------

(check "a pattern's wildcards take parts of a name, * as little as will do, in any case"
       (equal (esploro::pattern-parts "img_*_*.JPG" "IMG_2024_07_01.jpg") '("2024" "07_01")))
(check "? takes one" (equal (esploro::pattern-parts "?-*" "a-b") '("a" "b")))
(check "a name that doesn't fit" (equal (multiple-value-list (esploro::pattern-parts "*.pdf" "a.txt")) '(nil nil)))
(check "an empty part fits" (equal (multiple-value-list (esploro::pattern-parts "a*b" "ab")) '(("") t)))
(check "a template gives the parts back, numbers and #"
       (equal (esploro::expand-template "#2 #1 ## #n" '("a" "b") 7 3) "b a # 007"))
(signals error (esploro::expand-template "#3" '("a") 1 1))
(signals error (esploro::expand-template "#x" '() 1 1))
(esploro::ensure-folder (p "ren"))
(dolist (n '("IMG_1.jpg" "IMG_2.jpg" "IMG_3.jpg" "notes.txt" "a b.txt" "1.txt" "2.txt"))
  (make-file (p "ren" n) n))
(flet ((names (steps) (mapcar (lambda (s) (list (path-name (second s)) (third s))) steps))
       (ren (&rest names) (mapcar (lambda (n) (p "ren" n)) names)))
  (multiple-value-bind (steps problems why)
      (esploro::rename-by-plan "IMG_*.jpg" "Holiday #n (#1).jpg" (ren "IMG_1.jpg" "IMG_2.jpg" "notes.txt"))
    (check "renames by a pattern: each file before and after, the rest left be"
           (and (null problems)
                (equal (names steps) '(("IMG_1.jpg" "Holiday 1 (1).jpg") ("IMG_2.jpg" "Holiday 2 (2).jpg")))
                (search "notes.txt" why))))
  (check "text without wildcards is replaced wherever a name holds it"
         (equal (names (esploro::rename-by-plan " " "_" (ren "a b.txt" "notes.txt"))) '(("a b.txt" "a_b.txt"))))
  (check "numbers are as wide as the count needs"
         (equal (mapcar #'second (names (esploro::rename-by-plan "*" "#n-#1"
                                                                  (loop repeat 10 collect (p "ren" "IMG_1.jpg")))))
                '("01-IMG_1.jpg" "02-IMG_1.jpg" "03-IMG_1.jpg" "04-IMG_1.jpg" "05-IMG_1.jpg"
                  "06-IMG_1.jpg" "07-IMG_1.jpg" "08-IMG_1.jpg" "09-IMG_1.jpg" "10-IMG_1.jpg")))
  (multiple-value-bind (steps problems) (esploro::rename-by-plan "*.txt" "same.txt" (ren "notes.txt" "a b.txt"))
    (check "two files to one name: refused, no steps, saying which"
           (and (null steps) (equal problems '("notes.txt, a b.txt would all be named same.txt")))))
  (multiple-value-bind (steps problems) (esploro::rename-by-plan "notes" "1" (ren "notes.txt"))
    (check "a name already taken: refused" (and (null steps) (search "already there" (first problems)))))
  (check "a name another file is leaving: renamed after it"
         (equal (names (esploro::rename-by-plan "?.txt" "#1#1.txt" (ren "1.txt" "2.txt")))
                '(("1.txt" "11.txt") ("2.txt" "22.txt"))))
  (check "a chain: the name freed first"
         (progn (make-file (p "ren/11.txt"))
                (prog1 (equal (names (esploro::rename-by-plan "1" "11" (ren "1.txt" "11.txt")))
                              '(("11.txt" "1111.txt") ("1.txt" "11.txt")))
                  (sb-posix:unlink (p "ren/11.txt")))))
  (check "names going round each other: refused"
         (search "each other's" (first (nth-value 1 (esploro::rename-by-plan "?.txt" "#n.txt" (ren "2.txt" "1.txt"))))))
  (check "a name with a / in it: refused"
         (search "isn't empty, has no /" (first (nth-value 1 (esploro::rename-by-plan "a b" "a/b" (ren "a b.txt"))))))
  (check "a name that stays isn't a step" (null (esploro::rename-by-plan "notes" "notes" (ren "notes.txt"))))
  (check "esploro rename-by --plan: the plan kept in a file, for the window to show"
         (destructuring-bind (code what file why n recipe)
             (run-cli (list* "rename-by" "--plan" "IMG_*" "Pic #1" (ren "IMG_1.jpg" "notes.txt")))
           (and (eql code 0) (eq what :plan) (= n 1) (search "Pic" why)
                (equal recipe '(:rename-by "IMG_*" "Pic #1"))
                (equal (read-plan-file file) (list (list :rename (p "ren/IMG_1.jpg") "Pic 1.jpg")))
                (path-exists-p (p "ren/IMG_1.jpg")))))
  (check "refused, it says why"
         (equal (run-cli (list* "rename-by" "--plan" "*.txt" "x" (ren "1.txt" "2.txt")))
                (list 1 :refused (list "1.txt, 2.txt would all be named x"))))
  (check "a template that can't be made, said at once"
         (eq (second (run-cli (list* "rename-by" "--plan" "zzz" "#q" (ren "1.txt")))) :error))
  (check "nothing fits: nothing to do" (eq (second (run-cli (list* "rename-by" "--plan" "zzz" "y" (ren "1.txt")))) :none))
  (check "kept as a recipe by name"
         (equal (run-cli (list "recipe" "add" "Pics" "(:rename-by \"IMG_*\" \"Pic #1\")"))
                (list 0 :saved "Pics" "rename IMG_* to Pic #1")))
  (check "listed as one to review" (equal (cdr (run-cli (list "recipe" "list"))) '(("Pics" "rename IMG_* to Pic #1" :review))))
  (check "run, it's a plan to review, not done at once"
         (and (eq (second (run-cli (list* "recipe" "run" "--plan" "Pics" (ren "IMG_3.jpg")))) :plan)
              (path-exists-p (p "ren/IMG_3.jpg"))))
  (check "what isn't a recipe isn't kept"
         (and (eq (second (run-cli (list "recipe" "add" "Bad" "(:delete \"/\")"))) :error)
              (eq (second (run-cli (list "recipe" "add" "Bad" "(:rename-by \"\" \"x\")"))) :error)))
  (run-cli (list "recipe" "forget" "Pics")))

;;; --- Sorting by kind into several folders ------------------------------------------------

(esploro::ensure-folder (p "srt/Images"))
(esploro::ensure-folder (p "srt/deep/Images"))
(esploro::ensure-folder (p "srt/sub"))
(dolist (n '("a.jpg" "b.png" "c.pdf" "d.docx" "e.lisp" "f.zip" "Images/a.jpg" "Images/z.png" "deep/g.jpg"
             "deep/Images/h.jpg"))
  (make-file (p "srt" n) n))
(flet ((srt (&rest names) (mapcar (lambda (n) (p "srt" n)) names))
       (short (steps) (mapcar (lambda (s) (cons (first s) (mapcar (lambda (x) (esploro::short-path x (p "srt"))) (rest s))))
                             steps)))
  (multiple-value-bind (steps problems why)
      (esploro::sort-by-kind-plan esploro::*kind-folders*
                                  (srt "a.jpg" "b.png" "c.pdf" "d.docx" "e.lisp" "f.zip" "sub" "Images/z.png"
                                       "deep/g.jpg" "deep/Images/h.jpg"))
    (check "sorted by kind into several folders, each made once, beside the files"
           (and (null problems)
                (equal (short steps)
                       '((:move "a.jpg" "Images/a 2.jpg") (:move "b.png" "Images/b.png")
                         (:mkdir "Documents") (:move "c.pdf" "Documents/c.pdf")
                         (:mkdir "Text") (:move "e.lisp" "Text/e.lisp")
                         (:mkdir "Archives") (:move "f.zip" "Archives/f.zip")
                         (:move "deep/g.jpg" "deep/Images/g.jpg")))))
    (check "the review says where they go, what stays and why, and which got another name"
           (and (search "2 into ~/srt/Images" why) (search "d.docx (of no kind named)" why)
                (search "sub (folders)" why) (search "z.png, h.jpg (already in their folder)" why)
                (search "a.jpg as a 2.jpg" why))))
  (check "two files of one name into one folder: the second gets a free name"
         (equal (short (esploro::sort-by-kind-plan '((:image "/tmp-nowhere-esploro")) (srt "a.jpg" "Images/a.jpg")))
                '((:mkdir "/tmp-nowhere-esploro") (:move "a.jpg" "/tmp-nowhere-esploro/a.jpg")
                  (:move "Images/a.jpg" "/tmp-nowhere-esploro/a 2.jpg"))))
  (check "a folder whole, its missing folders above made first; the first kind that fits wins"
         (equal (short (esploro::sort-by-kind-plan '((:lisp "Code/Lisp") (:text "Text") (:file "Other"))
                                                   (srt "e.lisp" "d.docx")))
                '((:mkdir "Code") (:mkdir "Code/Lisp") (:move "e.lisp" "Code/Lisp/e.lisp")
                  (:mkdir "Other") (:move "d.docx" "Other/d.docx"))))
  (make-file (p "srt/Text") "a file")
  (multiple-value-bind (steps problems) (esploro::sort-by-kind-plan esploro::*kind-folders* (srt "e.lisp" "c.pdf"))
    (check "a folder that's a file: refused, no steps"
           (and (null steps) (equal problems (list (format nil "e.lisp can't go into ~a: that's a file, not a folder"
                                                           (esploro::short-path (p "srt/Text"))))))))
  (sb-posix:unlink (p "srt/Text"))
  (check "esploro sort-by-kind --plan: kept for the window, nothing moved"
         (destructuring-bind (code what file why n recipe) (run-cli (list* "sort-by-kind" "--plan" (srt "c.pdf" "f.zip")))
           (declare (ignore why))
           (and (eql code 0) (eq what :plan) (= n 4) (eq (first recipe) :sort-by-kind)
                (= 4 (length (read-plan-file file))) (path-exists-p (p "srt/c.pdf")))))
  (check "nothing to sort: nothing to do" (eq (second (run-cli (list "sort-by-kind" "--plan" (p "srt/d.docx")))) :none))
  (check "your own kinds and folders, kept as a recipe"
         (equal (run-cli (list "recipe" "add" "Media" "(:sort-by-kind (:image \"Pics\") (:video \"/media/v\"))"))
                (list 0 :saved "Media" "sort by kind into Pics, /media/v")))
  (check "a sorting with a kind it doesn't know, or a folder going up, isn't kept"
         (and (eq (second (run-cli (list "recipe" "add" "X" "(:sort-by-kind (:spreadsheet \"S\"))"))) :error)
              (eq (second (run-cli (list "recipe" "add" "X" "(:sort-by-kind (:image \"../up\"))"))) :error)
              (eq (second (run-cli (list "recipe" "add" "X" "(:sort-by-kind)"))) :error)))
  (check "run, it's a plan to review"
         (let ((r (run-cli (list* "recipe" "run" "--plan" "Media" (srt "b.png")))))
           (and (eq (second r) :plan)
                (equal (short (read-plan-file (third r))) '((:mkdir "Pics") (:move "b.png" "Pics/b.png"))))))
  (check "applied, undo puts it all back"
         (let ((steps (esploro::sort-by-kind-plan esploro::*kind-folders* (srt "c.pdf" "f.zip"))))
           (apply-plan steps)
           (and (path-exists-p (p "srt/Documents/c.pdf"))
                (progn (undo-last) (and (path-exists-p (p "srt/c.pdf")) (not (path-exists-p (p "srt/Documents"))))))))
  (run-cli (list "recipe" "forget" "Media")))

;;; --- Searches: folders that are questions ---------------------------------------------

(check "words are a query" (equal (esploro::parse-query "report kind:pdf newer:7 larger:1M -draft")
                                  '(:and (:name "report") (:kind :pdf) (:newer-than 7) (:larger-than 1048576)
                                    (:not (:name "draft")))))
(check "and back to words" (equal (esploro::query-words (esploro::parse-query "report kind:pdf larger:1M -draft"))
                                  "report kind:pdf larger:1M -draft"))
(check "one word is itself" (equal (esploro::parse-query "*.md") '(:glob "*.md")))
(check "a query can be an s-expression" (equal (esploro::parse-query "(:or (:glob \"*.md\") (:kind :pdf))")
                                               '(:or (:glob "*.md") (:kind :pdf))))
(signals error (esploro::parse-query "kind:nope"))
(signals error (esploro::parse-query "(:delete \"/\")"))
(check "globs" (and (esploro::glob-match-p "*.MD" "notes.md") (esploro::glob-match-p "a?c" "abc")
                    (not (esploro::glob-match-p "*.md" "notes.mdx"))))
(esploro::ensure-folder (p "srch/deep/er"))
(esploro::ensure-folder (p "srch/.hidden"))
(make-file (p "srch/report.pdf")) (make-file (p "srch/deep/er/old report.pdf"))
(make-file (p "srch/deep/notes.md")) (make-file (p "srch/.hidden/report.pdf"))
(make-file (p "srch/big.txt") (make-string 3000 :initial-element #\x))
(sb-posix:utime (p "srch/deep/er/old report.pdf") 0 0)
(flet ((found (text) (sort (mapcar (lambda (f) (esploro::short-path f (p "srch")))
                                   (fourth (cdr (run-cli (list "query" text (p "srch"))))))
                           #'string<)))
  (check "found below, not in hidden folders" (equal (found "report") '("deep/er/old report.pdf" "report.pdf")))
  (check "by kind and time" (equal (found "kind:pdf newer:30") '("report.pdf")))
  (check "by size" (equal (found "larger:2k") '("big.txt")))
  (check "not" (equal (found "kind:file -report -big") '("deep/notes.md")))
  (check "folders too" (equal (found "kind:folder") '("deep" "deep/er"))))
(check "a bad query says why" (eq (second (run-cli (list "query" "newer:soon" (p "srch")))) :error))
(check "a search kept by name" (eq (second (run-cli (list "search" "save" "Reports" "report kind:pdf" (p "srch")))) :saved))
(check "is listed" (equal (cdr (run-cli (list "search" "list"))) (list (list "Reports" "report kind:pdf" (p "srch")))))
(check "and run" (= 2 (length (fourth (cdr (run-cli (list "search" "run" "Reports")))))))
(check "and forgotten" (progn (run-cli (list "search" "forget" "Reports")) (null (esploro::read-searches))))

;;; --- Closing a project: what's open in it ------------------------------------------------

(let* ((term (esploro::make-window :id 5 :class "Alacritty" :group "2"))
       (emacs (esploro::make-window :id 6 :class "Emacs" :group "2"))
       (pdf (esploro::make-window :id 7 :class "zathura" :group "3"))
       (map (make-hash-table :test 'equal)))
  (setf (gethash "/p/proj" map) (list (cons term :folder))
        (gethash "/p/proj/doc/a.pdf" map) (list (cons pdf :argument))
        (gethash "/p/proj/notes.md" map) (list (cons emacs :modified-buffer))
        (gethash "/p/other/b.pdf" map) (list (cons pdf :argument)))
  (check "a project's windows: what has something in it, Emacs's buffers left to Emacs"
         (equal (esploro::project-windows "/p/proj" map)
                '((5 "Alacritty" "2" nil ("/p/proj")) (7 "zathura" "3" nil ("/p/proj/doc/a.pdf"))))))
(esploro::ensure-folder (p "proj/.git"))
(esploro::ensure-folder (p "proj/src"))
(check "esploro project: the project, from inside it"
       (equal (subseq (run-cli (list "project" (p "proj/src"))) 0 3) (list 0 :project (p "proj"))))
(check "and says when there's none" (eq (second (run-cli (list "project" (p "srch")))) :none))

;;; --- Learning from your edits to a plan -------------------------------------------------

(let ((proposed '((:move "/h/D/a.zip" "/h/Archives/a.zip") (:trash "/h/D/x.iso") (:move "/h/D/p.png" "/h/Images/p.png")
                  (:move "/h/D/n.txt" "/h/Notes/n.txt")))
      (applied '((:move "/h/D/a.zip" "/h/Work/a.zip") (:move "/h/D/p.png" "/h/Images/p.png") (:trash "/h/D/n.txt"))))
  (check "what you changed in an agent's plan, file by file"
         (equal (esploro::plan-corrections proposed applied)
                '((:elsewhere "/h/D/a.zip" "/h/Archives" "/h/Work") (:left "/h/D/x.iso" :trash)
                  (:elsewhere "/h/D/n.txt" "/h/Notes" :trash))))
  (check "as rules in words"
         (equal (mapcar #'esploro::correction-rule (esploro::plan-corrections proposed applied))
                '("\"a.zip\" goes in /h/Work, not in /h/Archives." "Don't put \"x.iso\" in the Trash; leave it in /h/D."
                  "\"n.txt\" goes in the Trash, not in /h/Notes.")))
  (check "the same plan teaches nothing" (null (esploro::plan-corrections proposed proposed))))
(esploro::ensure-folder (p ".config/esploro"))
(with-open-file (out (p ".config/esploro/sorting.md") :direction :output :if-exists :supersede)
  (format out "# Where my files go~%~%## Sorting~%~%- By what it's for.~%~%## Learnt from my corrections~%~%- One.~%~%## Always~%~%- Ask.~%"))
(run-cli (list "learn" "--add" "Two."))
(check "a rule goes at the end of the learnt ones, before the next heading"
       (equal (esploro::file-text (p ".config/esploro/sorting.md"))
              (format nil "# Where my files go~%~%## Sorting~%~%- By what it's for.~%~%## Learnt from my corrections~%~%- One.~%- Two.~%~%## Always~%~%- Ask.~%")))
(delete-file (p ".config/esploro/sorting.md"))
(run-cli (list "learn" "--add" "First."))
(check "with no rules file, one is made"
       (search (format nil "## Learnt from my corrections~%~%- First.~%") (esploro::file-text (p ".config/esploro/sorting.md"))))

;;; --- Habits: what you keep doing, offered back ----------------------------------------------

(check "words worth noticing in a name"
       (equal (sort (esploro::name-words "2025-03 Invoice_LME-v2.pdf") #'string<) '("invoice" "lme")))
(esploro::ensure-folder (p "hab/Downloads"))
(esploro::ensure-folder (p "hab/Work/Invoices"))
(dolist (n '("invoice 1.pdf" "Invoice-2.pdf" "march invoice.pdf" "photo.png"))
  (make-file (p "hab/Downloads" n)))
(flet ((move (&rest names)
         (run-cli (list "apply")
                  (format nil "~{~a~%~}"
                          (mapcar (lambda (n) (format nil "(:move ~s ~s)" (p "hab/Downloads" n) (p "hab/Work/Invoices" n)))
                                  names)))))
  (move "invoice 1.pdf")
  (check "one plan is no habit" (null (cdr (run-cli (list "habits")))))
  (move "Invoice-2.pdf" "march invoice.pdf"))
(let ((habits (cdr (run-cli (list "habits")))))
  (check "three like files, in two plans, into one folder: a habit" (= 1 (length habits)))
  (check "said, as a rule, a recipe and a search"
         (equal (rest (first habits))
                (list (format nil "You've moved 3 PDFs and e-books named \"invoice\" from ~a into ~a."
                              (esploro::short-path (p "hab/Downloads")) (esploro::short-path (p "hab/Work/Invoices")))
                      (format nil "PDFs and e-books named \"invoice\" from ~a go in ~a."
                              (esploro::short-path (p "hab/Downloads")) (esploro::short-path (p "hab/Work/Invoices")))
                      (list :move-into (p "hab/Work/Invoices"))
                      "invoice kind:pdf"
                      (p "hab/Downloads"))))
  (check "--new tells of it once" (and (= 1 (length (cdr (run-cli (list "habits" "--new")))))
                                       (null (cdr (run-cli (list "habits" "--new"))))
                                       (= 1 (length (cdr (run-cli (list "habits")))))))
  (run-cli (list "habits" "--dismiss" (first (first habits))))
  (check "waved away, it isn't offered again" (null (cdr (run-cli (list "habits"))))))
(undo-last) (undo-last)

;;; --- Long copies: progress, and stopping part way --------------------------------------------

(esploro::ensure-folder (p "long/bin"))
(make-file (p "long/src.bin") "all of it")
;; A cp that writes part of the copy, then takes its time.
(with-open-file (out (p "long/bin/cp") :direction :output)
  (format out "#!/bin/sh~%for last; do :; done~%echo part > \"$last\"~%sleep 30~%"))
(sb-posix:chmod (p "long/bin/cp") #o755)
(let ((path (sb-posix:getenv "PATH")))
  (sb-posix:setenv "PATH" (format nil "~a:~a" (p "long/bin") path) 1)
  (unwind-protect
       (check "a copy stopped part way takes back what it copied, and leaves the original"
              (and (eq :stopped (handler-case (sb-ext:with-timeout 1
                                                (esploro::copy-tree-step '(:copy "a" "b") (p "long/src.bin") (p "long/dst.bin")))
                                  (sb-ext:timeout () :stopped)))
                   (not (path-exists-p (p "long/dst.bin")))
                   (path-exists-p (p "long/src.bin"))))
    (sb-posix:setenv "PATH" path 1)))
(check "a copy's progress: the bytes in a folder, all of it"
       (= (esploro::tree-size (p "long")) (+ 9 (esploro::tree-size (p "long/bin")))))

;;; --- Archives, opened read-only like folders ------------------------------------------------

(when (and (esploro::archive-program) (probe-file "/dev/fuse"))
  (esploro::ensure-folder (p "arc/in/sub"))
  (make-file (p "arc/in/one.txt") "one")
  (make-file (p "arc/in/sub/two.txt") "two")
  (sb-ext:run-program "tar" (list "czf" (p "arc/x.tgz") "-C" (p "arc") "in") :search t)
  (let* ((answer (run-cli (list "archive" "open" (p "arc/x.tgz"))))
         (point (third answer)))
    (unwind-protect
         (progn
           (check "an archive opens read-only, like a folder"
                  (and (eq (second answer) :archive) (path-exists-p (esploro::join-path point "in" "sub" "two.txt"))))
           (check "opened again, the same place" (equal (third (run-cli (list "archive" "open" (p "arc/x.tgz")))) point))
           (check "nothing is written inside it"
                  (search "open read-only" (first (check-plan-of (format nil "(:copy ~s ~s)" (p "arc/in/one.txt")
                                                                         (esploro::join-path point "in" "new.txt"))))))
           (check "nor taken out of it"
                  (search "copy files out" (first (check-plan-of (format nil "(:trash ~s)" (esploro::join-path point "in" "one.txt"))))))
           (check "but copied out, yes"
                  (and (equal (run-cli (list "apply") (format nil "(:copy ~s ~s)" (esploro::join-path point "in" "one.txt") (p "arc/out.txt")))
                              '(0 :done 1))
                       (path-exists-p (p "arc/out.txt")))))
      (check "and closed, its place goes"
             (and (eq (second (run-cli (list "archive" "close" point))) :closed)
                  (not (path-exists-p point))
                  (null (esploro::open-archives)))))))

;;; --- Opening on a workspace ------------------------------------------------------------------

(let ((windows (list (esploro::make-window :id 1 :class "Emacs" :title "Esploro" :group "3")
                     (esploro::make-window :id 2 :class "Emacs" :title "notes.org" :group "3")
                     (esploro::make-window :id 3 :class "Emacs" :title "x" :group "4"))))
  (check "an Emacs frame on the workspace, not Esploro's" (eql 2 (esploro::emacs-frame-on "3" windows)))
  (check "none there, none" (null (esploro::emacs-frame-on "5" windows))))
(check "without StumpWM, open-on says there's no such workspace"
       (eq :error (second (run-cli (list "open-on" "3" (p "rec/in/two.txt"))))))

;;; --- Searching inside files ----------------------------------------------------------------

(check "has:WORD looks inside, asked last"
       (equal (esploro::parse-query "has:tender kind:text") '(:and (:kind :text) (:has "tender"))))
(check "and says so back" (equal (esploro::query-words '(:and (:kind :text) (:has "tender"))) "kind:text has:tender"))
(signals error (esploro::parse-query "has:"))
(esploro::ensure-folder (p "inside/sub"))
(make-file (p "inside/a.txt") "the LME tender, due Friday")
(make-file (p "inside/sub/b.md") "nothing here")
(make-file (p "inside/c.txt") "Tender documents")
(flet ((found (text) (sort (mapcar (lambda (f) (esploro::short-path f (p "inside")))
                                   (fourth (cdr (run-cli (list "query" text (p "inside"))))))
                           #'string<)))
  (check "files holding a word, any case" (equal (found "has:tender") '("a.txt" "c.txt")))
  (check "with the rest of a query" (equal (found "has:tender has:lme") '("a.txt")))
  (check "and not" (equal (found "kind:file -has:tender") '("sub/b.md")))
  ;; Without ripgrep, read by Esploro itself.
  (let ((path (sb-posix:getenv "PATH")))
    (sb-posix:setenv "PATH" "/nonexistent" 1)
    (unwind-protect (check "without ripgrep too" (equal (found "has:tender") '("a.txt" "c.txt")))
      (sb-posix:setenv "PATH" path 1))))

;;; --- Emacs waiting: never asked back ----------------------------------------------------------

(sb-posix:setenv "ESPLORO_NO_EMACS" "1" 1)
(let ((start (get-internal-real-time)))
  (check "Emacs waiting for the core isn't asked anything back (no emacsclient, no wait)"
         (and (null (esploro::emacs-ask "t"))
              (< (- (get-internal-real-time) start) (/ internal-time-units-per-second 10)))))
(sb-posix:unsetenv "ESPLORO_NO_EMACS")

;;; --- Recent: the files opened lately ---------------------------------------------------------

(esploro::ensure-folder (p "rec2"))
(make-file (p "rec2/old.pdf")) (make-file (p "rec2/new.txt")) (make-file (p "rec2/r&d plan.md"))
(esploro::ensure-folder (p ".local/share"))
(with-open-file (out (p ".local/share/recently-used.xbel") :direction :output :if-exists :supersede)
  (format out "<?xml version=\"1.0\"?>~%<xbel version=\"1.0\">~%")
  (format out "<bookmark href=\"file://~a\" added=\"2026-01-01T10:00:00Z\" modified=\"2026-01-01T10:00:00Z\" visited=\"2026-01-02T10:00:00.5Z\">~%</bookmark>~%"
          (p "rec2/old.pdf"))
  (format out "<bookmark href=\"file://~a\" visited=\"2026-03-01T10:00:00Z\"></bookmark>~%"
          (esploro::percent-encode (p "rec2/r&d plan.md")))
  (format out "<bookmark href=\"file:///gone/away.txt\" visited=\"2026-05-01T10:00:00Z\"></bookmark>~%</xbel>~%"))
(check "GTK's recent files, read" (= 3 (length (esploro::xbel-recent))))
(check "a name with spaces and & comes back as it is"
       (find (p "rec2/r&d plan.md") (esploro::xbel-recent) :key #'car :test #'string=))
(esploro::note-opened (p "rec2/new.txt"))
(check "newest first, Esploro's and GTK's together, gone files left out"
       (equal (subseq (esploro::recent-files) 0 3) (list (p "rec2/new.txt") (p "rec2/r&d plan.md") (p "rec2/old.pdf"))))
;; A text file: the core answers that Emacs shows it, and starts nothing.
(check "opening through Esploro notes it"
       (progn (run-cli (list "open" (p "rec2/r&d plan.md")))
              (equal (first (esploro::recent-files)) (p "rec2/r&d plan.md"))))

;;; --- Space: what's taking it -----------------------------------------------------------------

(esploro::ensure-folder (p "space/big"))
(esploro::ensure-folder (p "space/.hidden"))
(make-file (p "space/big/a.bin") (make-string 200000 :initial-element #\x))
(make-file (p "space/.hidden/b.bin") (make-string 50000 :initial-element #\x))
(make-file (p "space/small.txt") "x")
(let ((sizes (esploro::folder-sizes (p "space"))))
  (check "a folder's entries by the space they take, biggest first, hidden ones too"
         (equal (mapcar #'car (second sizes)) '("big" ".hidden" "small.txt")))
  (check "and the folder's total" (>= (first sizes) (reduce #'+ (mapcar #'cdr (second sizes))))))
(check "esploro sizes says so" (eq :sizes (second (run-cli (list "sizes" (p "space"))))))

;;; --- Duplicates: the same, byte for byte -----------------------------------------------------

(esploro::ensure-folder (p "dup/a"))
(esploro::ensure-folder (p "dup/b"))
(esploro::ensure-folder (p "dup/proj/.git"))
(let ((same (make-string 4000 :initial-element #\y)))
  (make-file (p "dup/a/report.pdf") same)
  (make-file (p "dup/b/report copy.pdf") same)
  (make-file (p "dup/proj/LICENSE") same))           ; another repository's own
(make-file (p "dup/b/other.pdf") (concatenate 'string (make-string 3999 :initial-element #\y) "z")) ; same size, not the same
(make-file (p "dup/a/tiny") "x") (make-file (p "dup/b/tiny") "x")
(sb-posix:utime (p "dup/a/report.pdf") 1000 1000)      ; the oldest: kept
(let ((groups (esploro::find-duplicates (p "dup"))))
  (check "copies found byte for byte; the oldest kept; small ones and another repository's left out"
         (equal groups (list (list 4000 (p "dup/a/report.pdf") (p "dup/b/report copy.pdf"))))))
(let ((answer (run-cli (list "duplicates" "--plan" (p "dup")))))
  (check "the copies, as a plan to review" (and (eq (second answer) :plan) (= 1 (fifth answer))))
  (check "whose steps put only the copy in the Trash"
         (equal (esploro::read-plan-file (third answer)) (list (list :trash (p "dup/b/report copy.pdf"))))))

;;; --- The end -------------------------------------------------------------------------

(sb-ext:run-program "chmod" (list "-R" "u+w" *top*) :search t)
(sb-ext:run-program "rm" (list "-rf" *top*) :search t)
(format t "~d checks, ~d failed~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
