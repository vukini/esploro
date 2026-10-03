;;;; where.lisp — which window has a file open.
;;;;
;;;; StumpWM knows the windows, and each window's process (_NET_WM_PID).
;;;; /proc knows what that process and its children have open, what they
;;;; were started on, and (for a shell in a terminal) which folder they're
;;;; in. Emacs keeps files in buffers rather than open, so it is asked
;;;; itself. Together: for each file, the windows that have it, and how.

(in-package #:esploro)

(defstruct window
  id class title group pid)

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type t)
    (format stream "~a ~s on ~a" (window-class window) (window-title window) (window-group window))))

(defun stumpwm-windows ()
  "Every window StumpWM manages; NIL when StumpWM can't be asked."
  (handler-case
      (loop for (id class title group pid)
              in (stumpwm-eval "(mapcar (lambda (w)
                                          (list (xlib:window-id (window-xwin w))
                                                (window-class w) (window-title w)
                                                (group-name (window-group w))
                                                (first (ignore-errors (xlib:get-property (window-xwin w) :_net_wm_pid)))))
                                        (all-windows))")
            collect (make-window :id id :class class :title title :group group :pid pid))
    (stumpwm-unreachable () nil)))

(defun focus-window (id)
  "Bring window ID to the front, on its workspace. True when it was there."
  (handler-case
      (stumpwm-eval (format nil "(let ((w (find ~d (all-windows) :key (lambda (w) (xlib:window-id (window-xwin w))))))
                                   (when w (focus-all w) t))" id))
    (stumpwm-unreachable () nil)))

;;; --- Processes -----------------------------------------------------------------

(defun read-file-bytes (path)
  (ignore-errors
   (with-open-file (in path :element-type '(unsigned-byte 8))
     (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
       (loop for b = (read-byte in nil) while b do (vector-push-extend b bytes))
       bytes))))

(defun process-children ()
  "A table of each process's children, from /proc/*/stat."
  (let ((children (make-hash-table)))
    (dolist (name (ignore-errors (folder-names "/proc")) children)
      (let ((pid (parse-integer name :junk-allowed t)))
        (when (and pid (every #'digit-char-p name))
          (let* ((bytes (read-file-bytes (format nil "/proc/~d/stat" pid)))
                 (stat (and bytes (sb-ext:octets-to-string bytes :external-format :latin-1)))
                 ;; The command name, in parentheses, may hold spaces: the
                 ;; fields after it start at the last ).
                 (close (and stat (position #\) stat :from-end t)))
                 (fields (and close (split-on #\Space (subseq stat (+ close 2)))))
                 (ppid (and fields (parse-integer (second fields) :junk-allowed t))))
            (when ppid (push pid (gethash ppid children)))))))))

(defun process-parent (pid)
  "PID's parent, from /proc/PID/stat; NIL when it's gone."
  (let* ((bytes (read-file-bytes (format nil "/proc/~d/stat" pid)))
         (stat (and bytes (sb-ext:octets-to-string bytes :external-format :latin-1)))
         (close (and stat (position #\) stat :from-end t)))
         (fields (and close (split-on #\Space (subseq stat (+ close 2))))))
    (and fields (parse-integer (second fields) :junk-allowed t))))

(defun own-processes (children)
  "This esploro, what started it (a shell, timeout, Emacs's process for it:
they hold the same file among their arguments), and every other esploro
running now (the window asks where and open at once): none of them has a
file open in the sense the map means."
  (let ((own (list (sb-posix:getpid)))
        (exe (read-link (format nil "/proc/~d/exe" (sb-posix:getpid)))))
    (loop for pid = (process-parent (first own)) then (process-parent pid)
          while (and pid (> pid 1))
          do (push pid own))
    (when exe
      (loop for pid being the hash-keys of children using (hash-value kids)
            do (dolist (p (cons pid kids))
                 (when (equal (read-link (format nil "/proc/~d/exe" p)) exe)
                   (pushnew p own)))))
    own))

(defun process-tree (pid children)
  (cons pid (loop for child in (gethash pid children)
                  append (process-tree child children))))

(defun read-link (path)
  (ignore-errors (sb-posix:readlink path)))

(defun ordinary-path-p (path)
  (and path (plusp (length path)) (char= (char path 0) #\/)
       (notany (lambda (top) (or (string= path top) (path-inside-p path top)))
               '("/proc" "/sys" "/dev" "/run" "/tmp/.X11-unix"))
       (not (search " (deleted)" path))))

(defun process-open-files (pid)
  (let ((fd-folder (format nil "/proc/~d/fd" pid)))
    ;; Not 0, 1 and 2: those a program inherits (the session's log), they
    ;; aren't what it opened. Nor folders it holds open to watch them.
    (loop for fd in (ignore-errors (folder-names fd-folder))
          for target = (and (> (or (parse-integer fd :junk-allowed t) 0) 2)
                            (read-link (join-path fd-folder fd)))
          when (and (ordinary-path-p target) (not (directory-p target))) collect target)))

(defun process-arguments (pid cwd)
  "The arguments PID was started with that name a file or folder."
  (let ((bytes (read-file-bytes (format nil "/proc/~d/cmdline" pid))))
    (when bytes
      (let ((args (rest (split-on (code-char 0)
                                  (string-right-trim (string (code-char 0))
                                                     (sb-ext:octets-to-string bytes :external-format :utf-8))))))
        (loop for arg in args
              for path = (cond ((zerop (length arg)) nil)
                               ((char= (char arg 0) #\-) nil)
                               ((char= (char arg 0) #\/) (normalize-path arg))
                               (cwd (normalize-path (join-path cwd arg))))
              when (and path (ordinary-path-p path) (path-exists-p path)) collect path)))))

;;; --- Emacs ---------------------------------------------------------------------

(defun emacs-ask (elisp)
  "ELISP's value from the running Emacs (its server), read as data; NIL
when there's none. timeout keeps a busy Emacs from holding Esploro up.
Never when Emacs itself is waiting for this answer (ESPLORO_NO_EMACS=1,
which the window sets for a call it waits on): it couldn't answer, and the
two would wait for each other until the timeout."
  (unless (equal (sb-posix:getenv "ESPLORO_NO_EMACS") "1")
    (ignore-errors
     (let ((out (with-output-to-string (s)
                  (sb-ext:run-program "timeout" (list "2" "emacsclient" "-e" elisp)
                                      :search t :output s :error nil :wait t))))
       (and (plusp (length out)) (read-foreign out))))))

(defun emacs-buffers ()
  "Emacs's file buffers and its frames' windows: (VALUES ((FILE MODIFIED) ...) IDS)."
  (let ((answer (emacs-ask "(list (mapcar (lambda (b) (list (buffer-file-name b) (if (buffer-modified-p b) t nil)))
                                    (seq-filter (function buffer-file-name) (buffer-list)))
                            (delq nil (mapcar (lambda (f) (frame-parameter f 'outer-window-id)) (frame-list))))")))
    (values (first answer)
            (loop for id in (second answer)
                  for n = (and (stringp id) (parse-integer id :junk-allowed t))
                  when n collect n))))

(defparameter *selection-elisp*
  "(let ((b (seq-find (lambda (b) (with-current-buffer b (derived-mode-p 'dired-mode))) (buffer-list))))
     (when b
       (with-current-buffer b
         (let ((marked (dired-get-marked-files nil nil nil t)))
           (list (expand-file-name default-directory)
                 (seq-remove (lambda (f) (or (eq f t) (member (file-name-nondirectory (directory-file-name f)) '(\".\" \"..\"))))
                             (if (eq (car marked) t) (cdr marked) marked)))))))"
  "What Emacs answers for the selection: (FOLDER FILES) of the Esploro view,
or dired, used last; the file at point when none is marked.")

(defun emacs-selection ()
  "The files selected in the Esploro view (or dired) used last, and its folder:
(VALUES FILES FOLDER); NIL when Emacs has none."
  (let ((answer (emacs-ask *selection-elisp*)))
    (when (and (consp answer) (stringp (first answer)))
      (values (loop for f in (second answer)
                    for path = (and (stringp f) (normalize-path f))
                    when path collect path)
              (normalize-path (first answer))))))

(defun true-symbol-p (x)
  (and x (symbolp x) (string= (symbol-name x) "T")))

(defun where-text (places)
  "\"Emacs (unsaved), Alacritty on 2\": the windows that have a file."
  (format nil "~{~a~^, ~}"
          (remove-duplicates
           (loop for (window . how) in places
                 collect (format nil "~a~@[ on ~a~]~:[~; (unsaved)~]"
                                 (window-class window) (window-group window)
                                 (eq how :modified-buffer)))
           :test #'string=)))

;;; --- The map ---------------------------------------------------------------------

(defun scan-where (&key (windows (stumpwm-windows)))
  "Where each file is open: a table from a path to a list of (WINDOW . HOW),
HOW being :file (held open), :argument (started on it), :folder (a shell
or program working in it), :buffer or :modified-buffer (in Emacs)."
  (let* ((map (make-hash-table :test 'equal))
         (children (process-children))
         (self (sb-posix:getpid))
         (own (own-processes children)))
    (flet ((note (path window how)
             (let ((path (normalize-path path)))
               (when path
                 (pushnew (cons window how) (gethash path map) :test #'equal)))))
      (dolist (window windows)
        (let ((pid (window-pid window)))
          (when (and (integerp pid) (/= pid self))
            (dolist (p (set-difference (process-tree pid children) own))
              (let ((cwd (read-link (format nil "/proc/~d/cwd" p))))
                (dolist (file (process-open-files p)) (note file window :file))
                (dolist (arg (process-arguments p cwd)) (note arg window :argument))
                ;; Every program has a folder; a folder means something only
                ;; for the window's children (the shell in a terminal), and
                ;; not when it's just home.
                (when (and cwd (/= p pid) (ordinary-path-p cwd)
                           (string/= (normalize-path cwd) (home-folder)))
                  (note cwd window :folder)))))))
      (multiple-value-bind (buffers frame-ids) (emacs-buffers)
        ;; An Emacs with no frame (a daemon) still has the file: a window
        ;; with no id stands for it.
        (let ((emacs-windows (or (remove-if-not (lambda (w) (member (window-id w) frame-ids)) windows)
                                 (remove-if-not (lambda (w) (equal (window-class w) "Emacs")) windows)
                                 (and buffers (list (make-window :class "Emacs" :title "no frame open"))))))
          (dolist (buffer buffers)
            (dolist (window emacs-windows)
              (note (first buffer) window
                    (if (true-symbol-p (second buffer)) :modified-buffer :buffer)))))))
    map))

(defun file-where (path map)
  "The (WINDOW . HOW) that have PATH, from MAP (SCAN-WHERE)."
  (gethash path map))

(defun window-files (id map)
  "The files window ID has, from MAP: ((PATH . HOW) ...)."
  (let ((files '()))
    (maphash (lambda (path places)
               (dolist (place places)
                 (when (eql (window-id (car place)) id)
                   (push (cons path (cdr place)) files))))
             map)
    (sort files #'string< :key #'car)))

;;; --- The workspace: what's here, and its project -----------------------------------------

(defun scratch-path-p (path)
  "A program's own temporary, cache or state file (a terminal's log)."
  (let ((home (home-folder)))
    (or (path-inside-p path "/tmp") (path-inside-p path "/run") (path-inside-p path "/var")
        (some (lambda (d) (path-inside-p path (join-path home d))) '(".cache" ".local/state" ".local/share")))))

(defun program-file-p (path)
  "True for an executable file: a program among a process's arguments (an
MCP server, a script run), not a document it has."
  (and (not (directory-p path))
       (ignore-errors (zerop (sb-posix:access path sb-posix:x-ok)))))

(defun stumpwm-here ()
  "StumpWM's current workspace and its focused window's id: (VALUES GROUP ID),
NIL when StumpWM can't be asked."
  (handler-case
      (destructuring-bind (&optional group id)
          (stumpwm-eval "(list (group-name (current-group))
                               (and (current-window) (xlib:window-id (window-xwin (current-window)))))")
        (values group id))
    (stumpwm-unreachable () nil)))

(defun project-root (path)
  "The project PATH is in: the nearest folder above it (itself, for a
folder) with a .git (a repository) or a log.md (vikix project's mark); NIL
when there's none below home."
  (let ((home (home-folder)))
    (loop for dir = (if (directory-p path) path (path-parent path)) then (path-parent dir)
          while (and dir (path-inside-p dir home))
          when (or (path-exists-p (join-path dir ".git")) (path-exists-p (join-path dir "log.md")))
            return dir)))

(defun workspace-folder (&key (windows (stumpwm-windows)) group (map nil map-p))
  "The folder the workspace GROUP (the current one) is about: the project
most of its windows' files and folders are in (a terminal's folder, Emacs's
files, a viewer's document), or the folder they're in when there's no
project; NIL when its windows say nothing (an empty workspace)."
  (let* ((group (or group (stumpwm-here)))
         (here (remove-if-not (lambda (w) (and (equal (window-group w) group)
                                                (not (equal (window-title w) "Esploro"))))
                              windows))
         (map (if map-p map (scan-where :windows here)))
         (counts (make-hash-table :test 'equal)))
    (when here
      (maphash (lambda (path places)
                 (dolist (place places)
                   (when (and (member (car place) here)
                              (not (and (eq (cdr place) :argument) (program-file-p path))))
                     (let* ((folder (if (eq (cdr place) :folder) path (path-parent path)))
                            (key (or (project-root path) folder)))
                       ;; Home and / say nothing about what a workspace is for.
                       (when (and key (string/= key (home-folder)) (string/= key "/") (directory-p key))
                         (incf (gethash key counts 0)))))))
               map)
      (let ((best nil) (n 0))
        (maphash (lambda (k v) (when (or (> v n) (and (= v n) best (string< k best))) (setf best k n v)))
                 counts)
        best))))

(defun reveal-target (&key (windows (stumpwm-windows)) id)
  "The file behind the focused window (or window ID): what an Emacs frame
shows, else what the window's program was started on, holds open, or works
in; NIL when there's none."
  (let* ((id (or id (nth-value 1 (stumpwm-here))))
         (window (find id windows :key #'window-id)))
    (when window
      (or (and (equal (window-class window) "Emacs")
               (let ((file (emacs-ask (format nil "(let ((f (seq-find (lambda (f) (equal (frame-parameter f 'outer-window-id) ~s)) (frame-list))))
                                                      (and f (with-current-buffer (window-buffer (frame-selected-window f))
                                                               (let ((x (or buffer-file-name (and (derived-mode-p 'dired-mode) default-directory)))) (and x (expand-file-name x))))))"
                                              (princ-to-string id)))))
                 (and (stringp file) (normalize-path file))))
          (let* ((files (window-files id (scan-where :windows (list window))))
                 (by (lambda (how) (find-if (lambda (f) (and (eq (cdr f) how) (path-exists-p (car f))
                                                              (or (eq how :folder)
                                                                  (and (not (directory-p (car f)))
                                                                       (not (program-file-p (car f)))))))
                                            files))))
            ;; What it was started on (a document in a viewer, vim's file),
            ;; else where it works (a terminal's shell), else what it
            ;; holds open, not counting a program's own logs and caches.
            (car (or (funcall by :argument) (funcall by :folder)
                     (find-if (lambda (f) (and (eq (cdr f) :file) (not (scratch-path-p (car f))))) files))))))))

;;; --- Closing a project: what's open in it --------------------------------------------------

(defun project-windows (root &optional (map (scan-where)))
  "The windows that have something below ROOT (or ROOT itself) other than
Emacs's buffers, which Emacs lists itself: ((ID CLASS GROUP TITLE (PATH ...)) ...)."
  (let ((windows (make-hash-table :test 'equal)))
    (maphash (lambda (path places)
               (when (or (string= path root) (path-inside-p path root))
                 (dolist (place places)
                   (unless (member (cdr place) '(:buffer :modified-buffer))
                     (pushnew path (gethash (car place) windows) :test #'string=)))))
             map)
    (let ((rows '()))
      (maphash (lambda (window paths)
                 (when (window-id window)
                   (push (list (window-id window) (window-class window) (window-group window)
                               (window-title window) (sort paths #'string<))
                         rows)))
               windows)
      (sort rows (lambda (a b) (string< (format nil "~a ~a" (third a) (second a))
                                        (format nil "~a ~a" (third b) (second b))))))))

;;; --- Workspaces: what each is about, and opening a file on one --------------------------

(defun workspaces ()
  "StumpWM's workspaces (not its hidden ones): ((NUMBER NAME WINDOWS FOLDER
CURRENT) ...), FOLDER the project or folder its windows are about, if any."
  (let* ((groups (handler-case
                     (stumpwm-eval "(let ((here (current-group)))
                                      (mapcar (lambda (g) (list (group-number g) (group-name g)
                                                                (length (group-windows g)) (eq g here)))
                                              (sort (remove-if (lambda (g) (< (group-number g) 1))
                                                               (copy-list (screen-groups (current-screen))))
                                                    (function <) :key (function group-number))))")
                   (stumpwm-unreachable () nil)))
         (windows (and groups (stumpwm-windows)))
         (map (and windows (scan-where :windows windows))))
    (loop for (number name count current) in groups
          collect (list number name count
                        (and (plusp count) (workspace-folder :windows windows :group name :map map))
                        (and current (not (null current)) (string/= (format nil "~a" current) "NIL"))))))

(defun go-to-workspace (number)
  "Make workspace NUMBER the current one: T, or NIL when there's none."
  (handler-case
      (stumpwm-eval (format nil "(let ((g (find ~d (screen-groups (current-screen)) :key (function group-number))))
                                   (when g (switch-to-group g) t))" number))
    (stumpwm-unreachable () nil)))

(defun emacs-frame-on (group windows)
  "An Emacs frame (not Esploro's) on the workspace GROUP: its window id, or NIL."
  (let ((w (find-if (lambda (w) (and (equal (window-group w) group) (equal (window-class w) "Emacs")
                                     (not (equal (window-title w) "Esploro"))))
                    windows)))
    (and w (window-id w))))
