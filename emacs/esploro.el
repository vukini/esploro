;;; esploro.el --- Esploro's window: a file explorer in Emacs, on dired  -*- lexical-binding: t -*-

;; Author: Vid <vukini@gmail.com>
;; URL: https://github.com/vukini/esploro
;; Package-Requires: ((emacs "29.1"))
;; License: MIT

;;; Commentary:

;; Esploro's window is a frame of its own, showing one folder at a time in
;; dired, with what a file manager needs on top: a menu bar and a tool bar,
;; right-click menus, click to select and double-click to open, dragging
;; files out to other programs and dropping them in, places down the side
;; (home, your folders, drives, bookmarks, the Trash), back and forward,
;; sorting, a filter, finding below, and the Trash to look in.
;;
;; Every change to files (copy, move, paste, rename, a new folder, the
;; Trash, a drop) goes through the esploro command (the Common Lisp core,
;; github.com/vukini/esploro) as a plan: checked whole first, done in the
;; background, journaled, so `esploro-undo' puts it back.  Beside each
;; file, the windows that have it open ("open in Emacs on 2").
;;
;;   M-x esploro   (or: esploro FOLDER, from a shell or a key)
;;
;; In it, ? shows the keys.  Dired's own keys still work, but its
;; operations (C, R, D...) go around the journal; Esploro's are on the
;; menus, the tool bar and the keys below.

;;; Code:

(require 'dired)
(require 'dnd)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'svg)
(require 'color)

(defgroup esploro nil
  "A file explorer on dired, for Lisp desktops."
  :group 'files
  :prefix "esploro-")

(defcustom esploro-program "esploro"
  "The esploro command: the core that does every change to files."
  :type 'string)

(defcustom esploro-places
  '(("Home" . "~") ("Documents" . "~/Documents") ("Downloads" . "~/Downloads")
    ("Pictures" . "~/Pictures") ("Music" . "~/Music") ("Videos" . "~/Videos")
    ("Projects" . "~/src") ("Dropbox" . "~/Dropbox"))
  "Places down the side: (NAME . FOLDER), each shown when the folder is there."
  :type '(alist :key-type string :value-type directory))

(defcustom esploro-frame-parameters
  '((name . "Esploro") (width . 130) (height . 42) (menu-bar-lines . 1) (tool-bar-lines . 1))
  "Esploro's frame.
Its menu bar and tool bar are on even when they're off elsewhere."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom esploro-find-limit 5000
  "The most files finding below shows."
  :type 'integer)

(defface esploro-where '((t :inherit font-lock-comment-face))
  "\"open in Emacs on 2\" beside a file.")

;;; --- State: each view keeps its own --------------------------------------------------

;; A view is a buffer of Esploro's showing one folder: the one in an Esploro
;; frame, or a pane of one (F3 splits it in two). Each has its own folder,
;; history, sort and hidden files, kept across dired's resetting of the
;; buffer (permanent-local), so frames on different workspaces, and two
;; panes side by side, go their own ways.
(defvar-local esploro--view nil "Non-nil in a buffer that is an Esploro view.")
(defvar-local esploro--back '() "Folders to go back to, newest first.")
(defvar-local esploro--forward '() "Folders to go forward to, after going back.")
(defvar-local esploro--sort 'name "How the folder is sorted: name, size, time or kind.")
(defvar-local esploro--reverse nil "Non-nil: the sort the other way round.")
(defvar-local esploro--hidden nil "Non-nil: files starting with a dot are shown.")
(defvar-local esploro--filter nil "Only names holding this text are shown, until refreshed.")
(defvar-local esploro--search nil "In a view of a search's files: (WORDS FOLDER NAME).")
(defvar-local esploro--thumbnails nil "Non-nil: pictures, PDFs and videos show a thumbnail in the list.")
(defvar esploro--unsorted nil "Non-nil while a list is shown in the order it's given (Recent).")
(defvar-local esploro--recent nil "Non-nil in a view of the files opened lately.")
(defvar-local esploro--duplicates nil "In a view of duplicates: (FOLDER . GROUPS).")
(put 'esploro--duplicates 'permanent-local t)
(defvar-local esploro--space nil
  "Non-nil: this view shows what takes space, biggest first: t, the
folder's entries; `below', the biggest things anywhere below it.")
(defvar-local esploro--space-total nil "The folder's size as measured, or `measuring'.")
(put 'esploro--space 'permanent-local t)
(put 'esploro--recent 'permanent-local t)
(defvar-local esploro--dropbox nil
  "What Dropbox says of this folder: (STATUS ENTRY-STATES ONLINE-ONLY), or
nil; ONLINE-ONLY, at Dropbox's top, how many folders aren't on this machine.")
(defvar-local esploro--git nil
  "What git says of this folder's repository: (ROOT BRANCH-LINE . STATES), or nil.")
(dolist (v '(esploro--view esploro--back esploro--forward esploro--sort esploro--reverse esploro--hidden
             esploro--search esploro--thumbnails))
  (put v 'permanent-local t))

(defvar esploro--clipboard nil "(copy . FILES) or (cut . FILES), from Esploro's own copy or cut.")
(defvar esploro--dropped '() "Files dropped in, as (OP FILE . FOLDER), applied together.")
(defvar esploro--wait nil "Non-nil: wait for the core's answers (the tests).")
(defconst esploro-places-buffer-name "*Esploro places*")
(defvar esploro-file-menu)
(defvar esploro-folder-menu)

;;; --- The core ---------------------------------------------------------------------

(defun esploro--read-answer (text)
  "The s-expression the core printed, or (:error TEXT)."
  (condition-case nil
      (car (read-from-string text))
    (error (list :error (string-trim text)))))

(defvar esploro--progress nil
  "When non-nil, a function: the call it's bound around reports progress
to it, (BYTES-DONE BYTES-ALL NAME), as the core copies.")

(defvar esploro--running '() "The core's plans being applied now: processes.")
(defvar esploro--remotes '() "Servers connected here, as (TARGET . MOUNTPOINT).")
(defvar esploro--archives '()
  "Archives opened here, as (ARCHIVE . MOUNTPOINT); kept after closing, so
Back into one opens it again.")

(defun esploro--progress-filter (report)
  "A filter for the core's standard error: its (:progress ...) lines to REPORT."
  (let ((pending ""))
    (lambda (_process text)
      (setq pending (concat pending text))
      (while (string-match "\\`\\([^\n]*\\)\n" pending)
        (let ((line (match-string 1 pending)))
          (setq pending (substring pending (match-end 0)))
          (pcase (ignore-errors (car (read-from-string line)))
            (`(:progress ,done ,all ,name) (funcall report done all name))))))))

(defun esploro--call (args &optional input then sync)
  "Run the core with ARGS, INPUT on its standard input; THEN gets its answer.
In the background, so a long copy never stops Emacs; SYNC waits (tests)."
  (if (not (executable-find esploro-program))
      (message "Esploro: the esploro command isn't installed (vikix add esploro)")
    ;; From /: the core is given whole paths, and the folder Emacs happens
    ;; to be in may be gone.
    (let ((out (generate-new-buffer " *esploro-out*"))
          (here default-directory)
          (default-directory "/"))
      (with-current-buffer out (setq default-directory "/"))
      (if (or sync esploro--wait)
          ;; Emacs waits for this answer, so the core mustn't ask Emacs
          ;; anything (emacsclient would wait for Emacs, and Emacs for it).
          (let* ((process-environment (cons "ESPLORO_NO_EMACS=1" process-environment))
                 (answer (with-current-buffer out
                          (when input (insert input))
                          (apply #'call-process-region (point-min) (point-max)
                                 esploro-program t t nil args)
                          (esploro--read-answer (buffer-string)))))
            (kill-buffer out)
            ;; THEN as called from where it was asked: the binding of "/"
            ;; above may be the view's own folder, bound for the while.
            (when then (let ((default-directory here)) (funcall then answer)))
            answer)
        (let* ((report esploro--progress)
               (process-environment (if report (cons "ESPLORO_PROGRESS=1" process-environment)
                                      process-environment))
               (err (when report
                      (make-pipe-process :name "esploro-progress" :noquery t
                                         :filter (esploro--progress-filter report))))
               (process (make-process
                         :name "esploro" :buffer out :command (cons esploro-program args)
                         :connection-type 'pipe :noquery t
                         :stderr err
                         :sentinel (lambda (process _event)
                                     (unless (process-live-p process)
                                       (setq esploro--running (delq process esploro--running))
                                       (when-let* ((err (process-get process 'esploro-stderr)))
                                         (delete-process err))
                                       (let ((answer (with-current-buffer (process-buffer process)
                                                       (esploro--read-answer (buffer-string)))))
                                         (kill-buffer (process-buffer process))
                                         (when then (funcall then answer))))))))
          (when report
            (process-put process 'esploro-stderr err)
            (push process esploro--running))
          (when input (process-send-string process input))
          (process-send-eof process)
          process)))))

(defun esploro--say (answer what)
  "Tell what the core's ANSWER means, about WHAT was asked."
  (pcase answer
    (`(:done ,n) (message "Esploro: %s (%d %s)" what n (if (= n 1) "step" "steps")))
    (`(:undone ,steps) (message "Esploro: undone: %s" (string-join steps "; ")))
    (`(:nothing) (message "Esploro: nothing to undo"))
    (`(:refused ,problems) (message "Esploro: not done, nothing changed: %s" (string-join problems "; ")))
    (`(:failed ,step ,reason . ,_) (message "Esploro: stopped at %s: %s (what was done before stays)" step reason))
    (`(:emptied ,n) (message "Esploro: the Trash is empty (%d deleted)" n))
    (`(:cancelled ,n) (message "Esploro: stopped; %s (undo takes %s back)"
                               (if (= n 0) "nothing was done" (format "%d %s done stay%s" n (if (= n 1) "step" "steps") (if (= n 1) "s" "")))
                               (if (= n 1) "it" "them")))
    (`(:error ,text) (message "Esploro: %s" text))
    (_ (message "Esploro: %s" what))))

(defun esploro--plan-text (steps)
  "STEPS as the core reads them: one form a line."
  (let ((print-escape-newlines nil) (print-length nil) (print-level nil))
    (mapconcat #'prin1-to-string steps "\n")))

(defun esploro--apply (steps what &optional sync)
  "Apply STEPS through the core, then show the folder again.
WHAT says what it was, for the message after.  SYNC waits (tests)."
  (when steps
    (message "Esploro: %s..." what)
    (let ((esploro--progress (esploro--progress-reporter what)))
      (esploro--call (list "apply") (esploro--plan-text steps)
                     (lambda (answer)
                       (esploro--say answer what)
                       (esploro--refresh)
                       (when (and (eq (car-safe answer) :done)
                                  (seq-some (lambda (s) (memq (car s) '(:move :copy))) steps))
                         (esploro--habits-nudge)))
                     sync))))

(defun esploro--progress-reporter (_what)
  "Say how a long copy goes, as the core reports it: which file, how much,
and how to stop it."
  (lambda (done all name)
    (when (> all 0)
      (message "Esploro: %s, %s of %s (%d%%)   C-c C-k stops"
               name (file-size-human-readable done) (file-size-human-readable all)
               (min 100 (/ (* 100 done) all))))))

(defun esploro-cancel ()
  "Stop the copy under way: the step being done is taken back, the ones
done stay (undo takes them back)."
  (interactive)
  (let ((process (car esploro--running)))
    (if (not (process-live-p process))
        (message "Esploro: nothing is being copied")
      (message "Esploro: stopping...")
      (interrupt-process process))))

;;; --- Showing a folder -------------------------------------------------------------

(defun esploro--switches ()
  "ls's switches for the sort, the order and hidden files.
No owner or group (-g -G): size, time and name are what a file manager shows."
  (concat "-lhgG --group-directories-first --time-style=long-iso -"
          (if esploro--hidden "a" "")
          ;; Recent files: in the order given, the newest first.
          (if esploro--unsorted "U" (pcase esploro--sort ('size "S") ('time "t") ('kind "X") (_ "v")))
          (if esploro--reverse "r" "")))

(defun esploro--view-p (buffer)
  (and (buffer-live-p buffer) (buffer-local-value 'esploro--view buffer)))

(defun esploro--views ()
  "Every view buffer there is."
  (seq-filter #'esploro--view-p (buffer-list)))

(defun esploro--new-view (&optional like)
  "A new, empty view buffer; with LIKE (a view), its sort and hidden files."
  (let ((buffer (generate-new-buffer "Esploro")))
    (with-current-buffer buffer
      (setq esploro--view t)
      (when (esploro--view-p like)
        (setq esploro--sort (buffer-local-value 'esploro--sort like)
              esploro--reverse (buffer-local-value 'esploro--reverse like)
              esploro--hidden (buffer-local-value 'esploro--hidden like))))
    buffer))

(defun esploro--view-windows (&optional frame)
  "FRAME's windows showing views, left to right (places, down the side, aren't)."
  (seq-filter (lambda (w) (and (not (window-parameter w 'window-side))
                               (esploro--view-p (window-buffer w))))
              (window-list (or frame (selected-frame)) 'no-minibuf)))

(defun esploro--view (&optional frame)
  "The view a command acts on: this buffer, when it's one; else the pane of
FRAME (the selected one) used last; else its first; nil when it has none."
  (if (esploro--view-p (current-buffer))
      (current-buffer)
    (let* ((frame (or frame (selected-frame)))
           (last (frame-parameter frame 'esploro-last-window)))
      (cond ((and (window-live-p last) (eq (window-frame last) frame)
                  (esploro--view-p (window-buffer last)))
             (window-buffer last))
            ((car (esploro--view-windows frame))
             (window-buffer (car (esploro--view-windows frame))))))))

(defun esploro--note-window ()
  "Remember the pane used last, for places and the tool bar."
  (when (esploro--view-p (window-buffer))
    (set-frame-parameter nil 'esploro-last-window (selected-window))))

(defun esploro--show (what &optional file buffer)
  "Show WHAT in BUFFER (the current view, or a new one): a folder, or
(TITLE . FILES) for a list.  Point goes to FILE when given.  Returns BUFFER."
  (with-current-buffer (or buffer (esploro--view) (esploro--new-view))
    (let ((inhibit-read-only t)
          (dir (if (consp what) (car what) (file-name-as-directory (expand-file-name what)))))
      (setq esploro--filter nil esploro--search nil esploro--recent nil esploro--duplicates nil)
      (erase-buffer)
      ;; As dired-internal-noselect does, in a buffer of Esploro's own, so a
      ;; dired of the same folder elsewhere is left alone.
      (dired-mode (if (consp what) what dir) (esploro--switches))
      (setq default-directory (if (consp what) (file-name-as-directory (car what)) dir))
      (esploro-mode 1)
      (dired-readin)
      (goto-char (point-min))
      (or (and file (dired-goto-file (expand-file-name file)))
          (dired-initial-position dir))
      ;; Named after its folder, for the buffer list; unique, as panes may
      ;; show the same one.
      (rename-buffer (format "Esploro: %s"
                             (let* ((d (directory-file-name (if (consp what) (car what) dir)))
                                    (archive (esploro--archive-for-path d)))
                               (if archive
                                   (let ((rel (file-relative-name d (cdr (assoc archive esploro--archives)))))
                                     (concat (file-name-nondirectory archive) (if (equal rel ".") "" (concat "/" rel))))
                                 (abbreviate-file-name d))))
                     t))
    (current-buffer)))

(defun esploro--dir (&optional buffer)
  "The folder the view BUFFER (the current one) shows."
  (when-let* ((buffer (or buffer (esploro--view))))
    (with-current-buffer buffer (expand-file-name default-directory))))

(defun esploro--refresh ()
  "Show every view again: a change in one folder may show in another pane."
  (dolist (buffer (esploro--views))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (setq esploro--filter nil)
        (revert-buffer)))))

(defun esploro--frames ()
  (seq-filter (lambda (f) (and (frame-live-p f) (frame-parameter f 'esploro))) (frame-list)))

(defun esploro--frame ()
  "The Esploro frame to use: the selected one when it's Esploro's, else any."
  (if (frame-parameter nil 'esploro) (selected-frame) (car (esploro--frames))))

(defun esploro--frame-of-window-id (id)
  "The Esploro frame whose X window is ID (as StumpWM knows it)."
  (seq-find (lambda (f) (equal (frame-parameter f 'outer-window-id) (format "%s" id)))
            (esploro--frames)))

(defun esploro--make-frame ()
  (make-frame (append esploro-frame-parameters
                      `((esploro . t)
                        ,@(when-let* ((display (or (car (x-display-list)) (getenv "DISPLAY"))))
                            `((display . ,display)))))))

(defun esploro--main-window (frame)
  "FRAME's window for a folder: its pane used last, else any but the places."
  (let ((last (frame-parameter frame 'esploro-last-window)))
    (if (and (window-live-p last) (eq (window-frame last) frame)) last
      (or (car (esploro--view-windows frame))
          (seq-find (lambda (w) (not (window-parameter w 'window-side))) (window-list frame 'no-minibuf))
          (frame-first-window frame)))))

;;;###autoload
(defun esploro (&optional dir where file)
  "Show DIR in an Esploro frame, and go there, with point on FILE if given.
WHERE says which: nil, the frame you're in when it's Esploro's, else any;
`new' (with a prefix argument), a new one; a number, the frame whose X
window that is (StumpWM's choice: the one on your workspace), or a new
one when it's gone."
  (interactive (list default-directory (and current-prefix-arg 'new)))
  (let* ((dir (expand-file-name (or dir default-directory)))
         (frame (cond ((eq where 'new) (esploro--make-frame))
                      ((numberp where) (or (esploro--frame-of-window-id where) (esploro--make-frame)))
                      (t (or (esploro--frame) (esploro--make-frame))))))
    (select-frame-set-input-focus frame)
    (with-selected-frame frame
      (select-window (esploro--main-window frame))
      (esploro-go dir file)
      (esploro-places-show)
      (when (and (esploro--preview-wanted-p) (not (esploro--preview-window frame)))
        (esploro--preview-show frame)))
    frame))

(defun esploro-new-window ()
  "Another Esploro frame, at this folder."
  (interactive)
  (esploro (or (esploro--dir) default-directory) 'new))

(defun esploro-go (dir &optional file no-history)
  "Show DIR in this view (or the selected window's), remembering where it was
for Back; point on FILE when given."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (buffer (cond ((esploro--view-p (current-buffer)) (current-buffer))
                       ((esploro--view-p (window-buffer)) (window-buffer))
                       (t (esploro--new-view)))))
    ;; Back into an archive closed since: open it again.
    (unless (file-directory-p dir)
      (when-let* ((archive (esploro--archive-for-path dir)))
        (esploro--archive-mount archive)))
    (unless (file-directory-p dir) (user-error "%s isn't a folder" dir))
    (with-current-buffer buffer
      (let ((here (and (derived-mode-p 'dired-mode) (expand-file-name default-directory))))
        (unless (or no-history (null here) (equal here dir))
          (push here esploro--back)
          (setq esploro--forward '()))))
    (esploro--show dir file buffer)
    (unless (eq (window-buffer) buffer)
      (switch-to-buffer buffer nil t))
    (esploro--note-window)
    (when (get-buffer esploro-places-buffer-name) (esploro-places-refresh))
    (when (esploro--preview-window) (esploro--preview-update))
    (esploro--close-archives-left)
    dir))

;;; --- Moving around ------------------------------------------------------------------

(defmacro esploro--in-view (&rest body)
  "BODY in the view a command acts on (`esploro--view'), or say there's none."
  `(with-current-buffer (or (esploro--view) (user-error "No Esploro here (M-x esploro)"))
     ,@body))

(defun esploro-back ()
  "Back to the folder before, in this view."
  (interactive)
  (esploro--in-view
   (if (null esploro--back) (message "Esploro: nothing to go back to")
     (let ((here (esploro--dir)))
       (push here esploro--forward)
       ;; Point on the folder just left, when it's in the one gone back to.
       (esploro-go (pop esploro--back) (directory-file-name here) t)))))

(defun esploro-forward ()
  "Forward again, after going back, in this view."
  (interactive)
  (esploro--in-view
   (if (null esploro--forward) (message "Esploro: nothing to go forward to")
     (let ((here (esploro--dir)))
       (push here esploro--back)
       (esploro-go (pop esploro--forward) (directory-file-name here) t)))))

(defun esploro-up ()
  "The folder above, with point on the one just left."
  (interactive)
  (let* ((here (directory-file-name (esploro--dir)))
         (up (file-name-directory here))
         (archive (car (rassoc here esploro--archives))))
    (cond (archive (esploro-go (file-name-directory archive) archive))
          ((equal (file-name-as-directory here) up) (message "Esploro: this is the top"))
          (t (esploro-go up here)))))

(defun esploro-home ()
  "Your home folder."
  (interactive)
  (esploro-go "~"))

(defun esploro-go-to (dir)
  "Go to the folder DIR, typed, with completion."
  (interactive (list (read-directory-name "Go to: " (esploro--dir) nil t)))
  (esploro-go dir))

(defun esploro--file-at (&optional event)
  "The file at EVENT's position or at point; nil on . and .. or no file."
  (save-excursion
    (when event (goto-char (posn-point (event-start event))))
    (let ((file (dired-get-filename nil t)))
      (and file (not (member (file-name-nondirectory (directory-file-name file)) '("." "..")))
           file))))

(defun esploro--selection ()
  "The marked files, or the one at point."
  (let ((marked (dired-get-marked-files nil nil nil t)))
    ;; With nothing marked, dired gives the file at point, or (t FILE) for one mark.
    (seq-remove (lambda (f) (or (eq f t) (member (file-name-nondirectory (directory-file-name f)) '("." ".."))))
                (if (eq (car marked) t) (cdr marked) marked))))

;;; --- Opening ------------------------------------------------------------------------

(defun esploro--other-frame ()
  "An Emacs frame to show a file in: one on the screen now (on this
workspace) that isn't Esploro's.  A frame on another workspace is `icon'
to Emacs, not t: a file sent there would open out of sight."
  (seq-find (lambda (f) (and (eq (frame-visible-p f) t) (not (frame-parameter f 'esploro))
                             (display-graphic-p f)))
            (frame-list)))

(defun esploro--visit (file)
  "FILE in Emacs, in a frame other than Esploro's (a new one if there's none)."
  (let ((frame (esploro--other-frame)))
    (if (not frame)
        (find-file-other-frame file)
      (select-frame-set-input-focus frame)
      (find-file file))))

(defun esploro-open (&optional file)
  "Open FILE (the one at point): a folder here; a file that a window has goes to
that window; text in Emacs; anything else in its usual program."
  (interactive)
  (let ((file (or file (esploro--file-at) (user-error "No file here"))))
    (cond
     ((file-directory-p file) (esploro-go file))
     ((and (esploro--archive-p file) (executable-find esploro-program))
      (esploro-open-archive file))
     (t
      (if (executable-find esploro-program)
          (esploro--call (list "open" file) nil
                         (lambda (answer)
                           (pcase answer
                             (`(:emacs) (esploro--visit file))
                             (`(:window ,class) (message "Esploro: %s has it: went there" class))
                             (`(:error ,text) (message "Esploro: %s" text)))))
        (call-process "xdg-open" nil 0 nil file))))))

(defun esploro-open-with (program)
  "Open the selection with PROGRAM, a command you type."
  (interactive (list (read-shell-command "Open with: ")))
  (let ((files (or (esploro--selection) (user-error "Nothing selected"))))
    (apply #'call-process "setsid" nil 0 nil "-f" (append (split-string-and-unquote program) files))))

(defun esploro-terminal-here ()
  "A terminal in this folder."
  (interactive)
  (let ((default-directory (esploro--dir)))
    (call-process "setsid" nil 0 nil "-f" (or (getenv "TERMINAL") "alacritty"))))

;;; --- Changes, through the core -----------------------------------------------------

(defun esploro--free-name (path suffix)
  "PATH, or a free name beside it: \"notes copy.org\", \"notes copy 2.org\"..."
  (if (not (file-exists-p path)) path
    (let* ((dir (file-name-directory path))
           (name (file-name-nondirectory path))
           (dot (string-match-p "\\.[^.]+\\'" name))
           (dot (and dot (> dot 0) dot))
           (stem (substring name 0 dot))
           (type (if dot (substring name dot) "")))
      (seq-find (lambda (p) (not (file-exists-p p)))
                (cons (concat dir stem suffix type)
                      (mapcar (lambda (n) (format "%s%s%s %d%s" dir stem suffix n type))
                              (number-sequence 2 9999)))))))

(defun esploro--paste-steps (op files dir)
  "The steps putting FILES into DIR: OP copy or cut (a move)."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (delq nil
          (mapcar (lambda (file)
                    (let* ((file (directory-file-name (expand-file-name file)))
                           (same (equal (file-name-directory file) dir))
                           (target (concat dir (file-name-nondirectory file))))
                      (cond ((and same (eq op 'cut)) nil)   ; already here
                            ((string-prefix-p (file-name-as-directory file) dir) nil) ; into itself
                            (t (list (if (eq op 'cut) :move :copy) file
                                     (esploro--free-name target (if same " copy" "")))))))
                  files))))

(defun esploro--uri (file)
  (concat "file://" (url-hexify-string (expand-file-name file) url-path-allowed-chars)))

(defun esploro--uri-file (uri)
  "The file a file:// URI names; nil for any other."
  (when (string-match "\\`file://\\(?:localhost\\)?\\(/.*\\)\\'" uri)
    (decode-coding-string (url-unhex-string (match-string 1 uri)) 'utf-8)))

(defun esploro--parse-copied (text)
  "(copy . FILES) or (cut . FILES) from x-special/gnome-copied-files or a URI list."
  (when (and text (not (string-empty-p text)))
    (let* ((lines (split-string text "[\r\n]+" t "[ \t]+"))
           (op (cond ((equal (car lines) "cut") 'cut) ((equal (car lines) "copy") 'copy)))
           (files (delq nil (mapcar #'esploro--uri-file (if op (cdr lines) lines)))))
      (when files (cons (or op 'copy) files)))))

(defun esploro--publish (op files)
  "Offer OP and FILES on the clipboard, so other file managers can paste them."
  (when (and (executable-find "xclip") (display-graphic-p))
    (with-temp-buffer
      (insert (symbol-name op) "\n" (mapconcat #'esploro--uri files "\n"))
      ;; xclip stays to answer for the clipboard; 0: don't wait for it.
      (call-process-region (point-min) (point-max) "xclip" nil 0 nil
                           "-selection" "clipboard" "-t" "x-special/gnome-copied-files"))))

(defun esploro--clipboard ()
  "What's to paste, as (copy . FILES) or (cut . FILES).
From the clipboard (another program, or Esploro), else Esploro's own."
  (or (and (display-graphic-p)
           (or (esploro--parse-copied (ignore-errors (gui-get-selection 'CLIPBOARD 'x-special/gnome-copied-files)))
               (esploro--parse-copied (ignore-errors (gui-get-selection 'CLIPBOARD 'text/uri-list)))))
      esploro--clipboard))

(defun esploro-copy ()
  "Copy the selection, to paste in another folder."
  (interactive)
  (let ((files (or (esploro--selection) (user-error "Nothing selected"))))
    (setq esploro--clipboard (cons 'copy files))
    (esploro--publish 'copy files)
    (message "Esploro: %d copied: paste where they should go" (length files))))

(defun esploro-cut ()
  "Cut the selection: pasting moves it."
  (interactive)
  (let ((files (or (esploro--selection) (user-error "Nothing selected"))))
    (setq esploro--clipboard (cons 'cut files))
    (esploro--publish 'cut files)
    (message "Esploro: %d cut: paste where they should go" (length files))))

(defun esploro-paste ()
  "Paste what was copied or cut into this folder."
  (interactive)
  (pcase-let ((`(,op . ,files) (or (esploro--clipboard) (user-error "Nothing copied or cut"))))
    (let ((steps (esploro--paste-steps op files (esploro--dir))))
      (if (null steps) (message "Esploro: nothing to paste here")
        (when (eq op 'cut) (setq esploro--clipboard nil))
        (esploro--apply steps (if (eq op 'cut) "moved" "copied"))))))

(defun esploro-trash ()
  "Put the selection in the Trash (undo puts it back)."
  (interactive)
  ;; A folder and something in it both selected (Biggest Anywhere Below
  ;; lists both): the folder alone, which takes the other with it.
  (let ((files (esploro--outermost (or (esploro--selection) (user-error "Nothing selected")))))
    (esploro--apply (mapcar (lambda (f) (list :trash f)) files)
                    (format "%d to the Trash" (length files)))))

(defun esploro-rename (file name)
  "Give FILE a new NAME."
  (interactive (let ((file (or (esploro--file-at) (user-error "No file here"))))
                 (list file (read-string "New name: " (file-name-nondirectory (directory-file-name file))))))
  (unless (equal name (file-name-nondirectory (directory-file-name file)))
    (esploro--apply (list (list :rename (directory-file-name file) name)) "renamed")))

(defun esploro-new-folder (name)
  "A new folder called NAME here."
  (interactive (list (read-string "New folder: ")))
  (esploro--apply (list (list :mkdir (directory-file-name (expand-file-name name (esploro--dir)))))
                  "folder made"))

(defun esploro-duplicate ()
  "A copy of each selected file beside it."
  (interactive)
  (let ((files (or (esploro--selection) (user-error "Nothing selected"))))
    (esploro--apply (esploro--paste-steps 'copy files (esploro--dir)) "duplicated")))

(defun esploro-move-to (dir)
  "Move the selection into DIR."
  (interactive (list (read-directory-name "Move to: " nil nil t)))
  (esploro--apply (esploro--paste-steps 'cut (esploro--selection) dir) "moved"))

(defun esploro-copy-to (dir)
  "Copy the selection into DIR."
  (interactive (list (read-directory-name "Copy to: " nil nil t)))
  (esploro--apply (esploro--paste-steps 'copy (esploro--selection) dir) "copied"))

(defun esploro-undo ()
  "Undo the last change Esploro applied."
  (interactive)
  (esploro--call (list "undo") nil (lambda (answer) (esploro--say answer "undone") (esploro--refresh))))

;;; --- The Trash ----------------------------------------------------------------------

(defun esploro--trash-dir ()
  (expand-file-name "Trash/files/" (or (getenv "XDG_DATA_HOME") "~/.local/share")))

(defun esploro--in-trash-p ()
  (equal (esploro--dir) (expand-file-name (esploro--trash-dir))))

(defun esploro-show-trash ()
  "What's in the Trash: restore from it, or empty it."
  (interactive)
  (make-directory (esploro--trash-dir) t)
  (esploro-go (esploro--trash-dir)))

(defun esploro-restore ()
  "Put the selection back where it was before the Trash."
  (interactive)
  (unless (esploro--in-trash-p) (user-error "Not in the Trash"))
  (let ((names (mapcar (lambda (f) (file-name-nondirectory (directory-file-name f)))
                       (or (esploro--selection) (user-error "Nothing selected")))))
    (message "Esploro: restoring...")
    (esploro--call (cons "restore" names) nil
                   (lambda (answer) (esploro--say answer "restored") (esploro--refresh)))))

(defun esploro-empty-trash ()
  "Delete everything in the Trash, for good: this can't be undone.
It's measured first, so the question says what emptying it gives back."
  (interactive)
  (message "Esploro: measuring the Trash...")
  (esploro--call
   (list "sizes" "--trash") nil
   (lambda (measured)
     (pcase-let* ((`(,bytes ,count) (pcase measured (`(:trash ,b ,c) (list b c)) (_ (list nil nil))))
                  (gives (and bytes (> bytes 0) (file-size-human-readable bytes))))
       (cond
        ((and count (= count 0) (not gives)) (message "Esploro: the Trash is empty already"))
        ((yes-or-no-p (if gives
                          (format "Delete everything in the Trash for good (%d %s, %s)? This can't be undone. "
                                  count (if (= count 1) "thing" "things") gives)
                        "Delete everything in the Trash for good? This can't be undone. "))
         (esploro--call (list "empty-trash") nil
                        (lambda (answer)
                          (pcase answer
                            ((and `(:emptied ,n) (guard gives))
                             (message "Esploro: the Trash is empty (%d deleted, %s given back)" n gives))
                            (_ (esploro--say answer "emptied")))
                          (esploro--refresh))))
        (t (message "Esploro: the Trash is as it was")))))))

;;; --- Looking: sort, hidden, filter, find --------------------------------------------

(defun esploro-sort (how)
  "Sort by HOW: name, size (largest first), time (newest first) or kind.
The same again turns it round."
  (interactive (list (intern (completing-read "Sort by: " '("name" "size" "time" "kind") nil t))))
  (esploro--in-view
   (if (eq how esploro--sort)
       (setq esploro--reverse (not esploro--reverse))
     (setq esploro--sort how esploro--reverse nil))
   (dired-sort-other (esploro--switches))))

(defun esploro-sort-cycle ()
  "The next sort: name, time, size, kind."
  (interactive)
  (esploro--in-view
   (setq esploro--reverse nil)
   (esploro-sort (pcase esploro--sort ('name 'time) ('time 'size) ('size 'kind) (_ 'name)))
   (message "Esploro: sorted by %s" esploro--sort)))

(defun esploro-toggle-hidden ()
  "Show or hide files whose names start with a dot."
  (interactive)
  (esploro--in-view
   (setq esploro--hidden (not esploro--hidden))
   (dired-sort-other (esploro--switches))
   (message "Esploro: hidden files %s" (if esploro--hidden "shown" "hidden"))))

(defun esploro-filter (text)
  "Show only the names holding TEXT (any case), until refreshed (g, F5)."
  (interactive (list (read-string "Show names with: ")))
  (esploro--in-view
    (revert-buffer)
    (unless (string-empty-p text)
      (setq esploro--filter text)
      (let ((inhibit-read-only t) (case-fold-search t))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((name (dired-get-filename 'no-dir t)))
              (if (and name (not (member name '("." "..")))
                       (not (string-match-p (regexp-quote text) name)))
                  (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
                (forward-line 1))))))
      (force-mode-line-update))))

(defun esploro-find (pattern)
  "Find files below this folder whose names match PATTERN (fd)."
  (interactive (list (read-string "Find below: ")))
  (let* ((dir (esploro--dir))
         (files (let ((default-directory dir))
                  (seq-take (ignore-errors
                              (process-lines "fd" "--hidden" "--exclude" ".git" "--color" "never"
                                             "--strip-cwd-prefix" "--" pattern))
                            esploro-find-limit))))
    (if (null files) (message "Esploro: nothing below matches %s" pattern)
      (esploro--in-view
       (push dir esploro--back)
       (setq esploro--forward '())
       (esploro--show (cons dir files) nil (current-buffer)))
      (message "Esploro: %d found%s" (length files)
               (if (= (length files) esploro-find-limit) " (the first ones)" "")))))

(defun esploro-properties ()
  "Size, permissions and owner of the file at point."
  (interactive)
  (let* ((file (or (esploro--file-at) (esploro--dir)))
         (attrs (file-attributes file 'string))
         (size (if (file-directory-p file)
                   (car (split-string (or (ignore-errors (car (process-lines "du" "-sh" "--" file))) "?")))
                 (file-size-human-readable (file-attribute-size attrs)))))
    (message "%s: %s, %s, %s:%s, changed %s"
             (abbreviate-file-name file) size (file-attribute-modes attrs)
             (file-attribute-user-id attrs) (file-attribute-group-id attrs)
             (format-time-string "%Y-%m-%d %H:%M" (file-attribute-modification-time attrs)))))

;;; --- Where files are open ---------------------------------------------------------------

(defun esploro--annotate ()
  "Beside each file that a window has open, which one: asked in the background."
  (let ((buffer (current-buffer))
        (dir default-directory))
    (when (and (executable-find esploro-program) (not (consp dired-directory)))
      (esploro--call (list "where" dir) nil
                     (lambda (answer)
                       (when (and (buffer-live-p buffer) (listp answer) (not (keywordp (car answer))))
                         (with-current-buffer buffer
                           (remove-overlays (point-min) (point-max) 'esploro-where t)
                           (dolist (place answer)
                             (save-excursion
                               (when (and (consp place)
                                          (dired-goto-file (expand-file-name (car place) dir)))
                                 (let ((o (make-overlay (line-end-position) (line-end-position))))
                                   (overlay-put o 'esploro-where t)
                                   (overlay-put o 'after-string
                                                (propertize (concat "   open in " (cdr place))
                                                            'face 'esploro-where)))))))))))))

;;; --- The mouse ----------------------------------------------------------------------------

(defun esploro--whole-row-drag ()
  "Let a file be dragged out from anywhere on its row, as in other file
managers, not only from its name: dired puts its drag keymap on the name."
  (when (and dired-mouse-drag-files (boundp 'dired-mouse-drag-files-map))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((name (dired-move-to-filename)))
            (when (and name (not (member (dired-get-filename 'no-dir t) '("." ".."))))
              (put-text-property (line-beginning-position) name 'keymap dired-mouse-drag-files-map)))
          (forward-line 1))))))

(defun esploro-mouse-open (event)
  "Open what was double-clicked."
  (interactive "e")
  (mouse-set-point event)
  (when-let* ((file (esploro--file-at event)))
    (esploro-open file)))

(defun esploro-mouse-toggle (event)
  "Ctrl+click: mark the file clicked, or unmark it."
  (interactive "e")
  (mouse-set-point event)
  (when (esploro--file-at)
    (save-excursion
      (beginning-of-line)
      (if (eq (char-after) dired-marker-char) (dired-unmark 1) (dired-mark 1)))))

(defun esploro-mouse-extend (event)
  "Shift+click: mark every file from point to the one clicked."
  (interactive "e")
  (let ((from (line-number-at-pos)))
    (mouse-set-point event)
    (let ((to (line-number-at-pos)))
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (min from to)))
        (dotimes (_ (1+ (abs (- to from))))
          (when (esploro--file-at) (dired-mark 1) (forward-line -1))
          (forward-line 1))))))

(defun esploro-context-menu (event)
  "Right-click: what can be done with the file clicked, or with the folder."
  (interactive "e")
  (mouse-set-point event)
  (let ((file (esploro--file-at)))
    (when (and file (not (member file (dired-get-marked-files nil nil nil t))))
      (dired-unmark-all-marks))
    (popup-menu (if file esploro-file-menu esploro-folder-menu) event)))

;;; --- Dropping files in ------------------------------------------------------------------

(defun esploro--dnd-file (uri action)
  "A file dropped on Esploro, at URI: copied in, or moved when ACTION says so.
Through the core, so undo takes it back."
  (when-let* ((file (or (esploro--uri-file uri) (dnd-get-local-file-name uri t))))
    ;; Into the folder of the pane it was dropped on: Emacs calls this with
    ;; that pane's window selected, so this buffer is its view.
    (let ((op (if (eq action 'move) 'cut 'copy))
          (dir (file-name-as-directory (expand-file-name default-directory))))
      (setq esploro--dropped (append esploro--dropped (list (cons op (cons file dir)))))
      ;; A drop of many files calls this once each: apply them together.
      (run-at-time 0.2 nil #'esploro--apply-dropped))
    action))

(defun esploro--apply-dropped ()
  (when esploro--dropped
    (let* ((dropped esploro--dropped)
           (steps
            (seq-mapcat
             (lambda (dir)
               (let ((here (seq-filter (lambda (d) (equal (cddr d) dir)) dropped)))
                 (seq-mapcat (lambda (op)
                               (esploro--paste-steps
                                op
                                ;; Files dropped on the folder they're in (let go over
                                ;; their own pane) stay as they are: no copies.
                                (seq-remove (lambda (f) (equal (file-name-directory (directory-file-name f)) dir))
                                            (mapcar #'cadr (seq-filter (lambda (d) (eq (car d) op)) here)))
                                dir))
                             '(copy cut))))
             (seq-uniq (mapcar #'cddr dropped)))))
      (setq esploro--dropped '())
      (if steps (esploro--apply steps "dropped in")
        (message "Esploro: dropped on the folder it's in: nothing to do")))))

;;; --- Closing ---------------------------------------------------------------------------------

(defun esploro-close ()
  "Close Esploro's frame; Emacs, and its other frames, go on.
Quitting from the menu (File, Quit) or with C-x C-c in Esploro does this:
in Emacs run as a daemon, the frame Esploro made isn't a client's, and
Emacs's own quit would end all of Emacs."
  (interactive)
  (let* ((frame (esploro--frame))
         (views (and frame (mapcar #'window-buffer (esploro--view-windows frame)))))
    (cond ((null frame) (bury-buffer))
          ((or (daemonp)
               (seq-some (lambda (f) (and (not (eq f frame)) (frame-visible-p f)))
                         (frame-list)))
           (delete-frame frame))
          ;; Esploro's frame is the only one, outside a daemon: keep Emacs.
          (t (delete-other-windows (esploro--main-window frame))
             (switch-to-buffer (other-buffer (current-buffer)))
             (set-frame-parameter frame 'esploro nil)))
    (let ((preview (and frame (frame-parameter frame 'esploro-preview))))
      (when (buffer-live-p preview) (setq views (cons preview views))))
    ;; Its views (and its preview) go with it, unless another frame shows one.
    (dolist (buffer views)
      (unless (get-buffer-window buffer t) (kill-buffer buffer)))
    (esploro--close-archives-left)))

;;; --- Two panes ---------------------------------------------------------------------------------

(defun esploro--other-pane (&optional window)
  "The other pane of WINDOW's frame (the selected one's), if it has two."
  (let ((window (or window (esploro--main-window (selected-frame)))))
    (seq-find (lambda (w) (not (eq w window))) (esploro--view-windows (window-frame window)))))

(defun esploro-split ()
  "Two panes side by side, each with its own folder; again, back to one."
  (interactive)
  (let* ((here (if (esploro--view-p (window-buffer)) (selected-window)
                 (esploro--main-window (selected-frame))))
         (other (esploro--other-pane here)))
    (if other
        (let ((buffer (window-buffer other)))
          (delete-window other)
          (unless (get-buffer-window buffer t) (kill-buffer buffer))
          (select-window here))
      (let* ((buffer (window-buffer here))
             (dir (esploro--dir buffer))
             (new (split-window here nil 'right))
             (view (esploro--new-view buffer)))
        (set-window-buffer new view)
        (with-selected-window new
          (with-current-buffer view (esploro-go dir nil t)))
        (select-window here)
        (esploro--note-window)))))

(defun esploro--to-other-pane (op)
  (let ((other (or (esploro--other-pane (selected-window))
                   (user-error "One pane: F3 makes two"))))
    (esploro--apply (esploro--paste-steps op (or (esploro--selection) (user-error "Nothing selected"))
                                          (esploro--dir (window-buffer other)))
                    (if (eq op 'cut) "moved to the other pane" "copied to the other pane"))))

(defun esploro-copy-to-other-pane ()
  "Copy the selection into the other pane's folder."
  (interactive)
  (esploro--to-other-pane 'copy))

(defun esploro-move-to-other-pane ()
  "Move the selection into the other pane's folder."
  (interactive)
  (esploro--to-other-pane 'cut))

(defun esploro--two-panes-p ()
  (and (esploro--other-pane (selected-window)) t))

;;; --- Dragging out: the file list as the standard says --------------------------------------

(defun esploro--uri-list-crlf (converted)
  "CONVERTED (Emacs's text/uri-list for a drag) with CRLF line ends.
RFC 2483 asks for CRLF; Emacs sends LF (select.el), which GTK and Qt
forgive, but winit (Alacritty and other Rust programs) splits on CRLF, so
a dropped name kept its newline, named no file, and the drop was lost."
  (if (and (consp converted) (stringp (cdr converted)))
      (cons (car converted)
            (replace-regexp-in-string "\r?\n" "\r\n" (cdr converted) t t))
    converted))

(with-eval-after-load 'select
  (when (fboundp 'xselect-convert-to-text-uri-list)
    (advice-add 'xselect-convert-to-text-uri-list :filter-return #'esploro--uri-list-crlf)))

;;; --- Menus, the tool bar and the keys -------------------------------------------------------

(defvar-keymap esploro-mode-map
  :doc "Esploro's keys, on top of dired's."
  "RET" #'esploro-open
  "f" #'esploro-open
  "^" #'esploro-up
  "M-<up>" #'esploro-up
  "M-<left>" #'esploro-back
  "M-<right>" #'esploro-forward
  "M-<home>" #'esploro-home
  "C-l" #'esploro-go-to
  "M-w" #'esploro-copy
  "C-w" #'esploro-cut
  "C-y" #'esploro-paste
  "<delete>" #'esploro-trash
  "<f2>" #'esploro-rename
  "+" #'esploro-new-folder
  "C-/" #'esploro-undo
  "C-_" #'esploro-undo
  "s" #'esploro-sort-cycle
  "." #'esploro-toggle-hidden
  "/" #'esploro-filter
  "M-s f" #'esploro-search
  "M-s s" #'esploro-search
  "<f5>" #'revert-buffer
  "<f9>" #'esploro-places-toggle
  "<f3>" #'esploro-split
  "<f11>" #'esploro-preview-toggle
  "z" #'esploro-repeat
  "T" #'esploro-thumbnails-toggle
  "G" #'esploro-grid-toggle
  "M-o" #'esploro-open-on-workspace
  "C-c b" #'esploro-bookmark-folder
  "C-c r" #'esploro-recent
  "C-c u" #'esploro-changes
  "C-c k" #'esploro-connect
  "C-c s" #'esploro-space-toggle
  "C-c S" #'esploro-space-below-toggle
  "C-c C-k" #'esploro-cancel
  "<f6>" #'esploro-move-to-other-pane
  "C-c C-c" #'esploro-copy-to-other-pane
  "C-x 5 2" #'esploro-new-window
  "?" #'esploro-manual
  "<f1>" #'esploro-manual
  "<remap> <save-buffers-kill-terminal>" #'esploro-close
  "<remap> <save-buffers-kill-emacs>" #'esploro-close
  "<mouse-2>" #'esploro-mouse-open
  "C-<down-mouse-1>" #'ignore
  "C-<mouse-1>" #'esploro-mouse-toggle
  "S-<down-mouse-1>" #'ignore
  "S-<mouse-1>" #'esploro-mouse-extend
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'esploro-context-menu)

(defun esploro--marked-or-point-p ()
  (and (derived-mode-p 'dired-mode) (or (esploro--file-at) (dired-get-marked-files nil nil nil t))))

;; Esploro's frame has a file manager's menus: File, Edit, View, Go and
;; Help, the same in its panes and its places. Emacs's own (Options,
;; Buffers, Tools) and dired's (Operate, Mark, Regexp, Immediate, Subdir)
;; are hidden there; every other frame keeps them.

(defun esploro--value (symbol)
  "SYMBOL's value in the view a command acts on (nil with none): the menus
show the pane's sort and history even when the places are selected."
  (let ((view (esploro--view)))
    (and view (buffer-local-value symbol view))))

(defun esploro-select-all ()
  "Select every file here."
  (interactive)
  (esploro--in-view (dired-unmark-all-marks) (dired-toggle-marks)))

(defun esploro-select-none ()
  "Select nothing."
  (interactive)
  (esploro--in-view (dired-unmark-all-marks)))

(defun esploro-invert-selection ()
  "Select what isn't, and unselect what is."
  (interactive)
  (esploro--in-view (dired-toggle-marks)))

(defconst esploro--menu-bar
  `((esploro-file "File"
          ["New Window" esploro-new-window :keys "C-x 5 2"]
          ["New Folder..." esploro-new-folder :keys "+"]
          "---"
          ["Open" esploro-open :active (esploro--file-at)]
          ["Open With..." esploro-open-with :active (esploro--marked-or-point-p)]
          ("Open on Workspace" :filter esploro--workspaces-menu)
          ("Commands" :filter esploro--commands-menu)
          ["Properties" esploro-properties :active (esploro--file-at)]
          "---"
          ["Terminal Here" esploro-terminal-here]
          ["Bookmark This Folder..." esploro-bookmark-folder
           :visible (not (and (esploro--view) (esploro--bookmarked-p (esploro--dir (esploro--view)))))]
          ["Remove Bookmark" esploro-remove-bookmark
           :visible (and (esploro--view) (esploro--bookmarked-p (esploro--dir (esploro--view))))]
          ["Close Project..." esploro-close-project]
          ["Close Esploro" esploro-close :keys "C-x C-c"])
    (esploro-edit "Edit"
          ["Undo" esploro-undo :keys "C-/"]
          ["Changes..." esploro-changes :keys "C-c u"]
          ["Stop Copying" esploro-cancel :keys "C-c C-k" :visible esploro--running]
          ["Repeat Last Change" esploro-repeat :keys "z" :active (esploro--marked-or-point-p)]
          ("Recipes" :filter esploro--recipes-menu)
          ["Habits Noticed..." esploro-habits]
          ["Find Duplicates" esploro-find-duplicates]
          ["Trash the Copies..." esploro-trash-duplicates :visible (esploro--value 'esploro--duplicates)]
          "---"
          ["Copy" esploro-copy :keys "M-w" :active (esploro--marked-or-point-p)]
          ["Cut" esploro-cut :keys "C-w" :active (esploro--marked-or-point-p)]
          ["Paste" esploro-paste :keys "C-y"]
          ["Duplicate" esploro-duplicate :active (esploro--marked-or-point-p)]
          ["Copy To..." esploro-copy-to :active (esploro--marked-or-point-p)]
          ["Move To..." esploro-move-to :active (esploro--marked-or-point-p)]
          ["Copy to Other Pane" esploro-copy-to-other-pane :keys "C-c C-c"
           :active (and (esploro--two-panes-p) (esploro--marked-or-point-p))]
          ["Move to Other Pane" esploro-move-to-other-pane :keys "F6"
           :active (and (esploro--two-panes-p) (esploro--marked-or-point-p))]
          ["Rename..." esploro-rename :keys "F2" :active (esploro--file-at)]
          ["Rename by Pattern..." esploro-rename-by-pattern]
          ["Sort into Folders by Kind..." esploro-sort-by-kind]
          ["Move to Trash" esploro-trash :keys "Delete" :active (esploro--marked-or-point-p)]
          "---"
          ["Select All" esploro-select-all]
          ["Select None" esploro-select-none]
          ["Invert Selection" esploro-invert-selection])
    (esploro-view "View"
          ["Sort by Name" (esploro-sort 'name) :style radio :selected (eq (esploro--value 'esploro--sort) 'name)]
          ["Sort by Size" (esploro-sort 'size) :style radio :selected (eq (esploro--value 'esploro--sort) 'size)]
          ["Sort by Time" (esploro-sort 'time) :style radio :selected (eq (esploro--value 'esploro--sort) 'time)]
          ["Sort by Kind" (esploro-sort 'kind) :style radio :selected (eq (esploro--value 'esploro--sort) 'kind)]
          ["The Other Way Round" (esploro-sort (esploro--value 'esploro--sort))
           :style toggle :selected (esploro--value 'esploro--reverse)]
          "---"
          ["Hidden Files" esploro-toggle-hidden :style toggle :selected (esploro--value 'esploro--hidden)]
          ["Thumbnails" esploro-thumbnails-toggle :keys "T" :style toggle :selected (esploro--value 'esploro--thumbnails)]
          ["Grid" esploro-grid-toggle :keys "G" :style toggle :selected (esploro--value 'esploro--grid)]
          ["What's Taking Space" esploro-space-toggle :keys "C-c s" :style toggle :selected (esploro--value 'esploro--space)]
          ["Biggest Anywhere Below" esploro-space-below-toggle :keys "C-c S" :style toggle
           :selected (eq (esploro--value 'esploro--space) 'below)]
          ["Filter..." esploro-filter :keys "/"]
          ["Search Below..." esploro-search :keys "M-s s"]
          ["Refresh" esploro--refresh :keys "F5"]
          "---"
          ["Two Panes" esploro-split :keys "F3" :style toggle :selected (esploro--two-panes-p)]
          ["Preview" esploro-preview-toggle :keys "F11" :style toggle :selected (esploro--preview-window)]
          ["Places" esploro-places-toggle :keys "F9" :style toggle
           :selected (get-buffer-window esploro-places-buffer-name)])
    (esploro-go "Go"
        ["Back" esploro-back :keys "M-<left>" :active (esploro--value 'esploro--back)]
        ["Forward" esploro-forward :keys "M-<right>" :active (esploro--value 'esploro--forward)]
        ["Up" esploro-up :keys "M-<up>"]
        ["Home" esploro-home]
        ["Go to Folder..." esploro-go-to :keys "C-l"]
        ["Recent Files" esploro-recent :keys "C-c r"]
        "---"
        ["Connect to Server..." esploro-connect :keys "C-c k"]
        ["Disconnect..." esploro-disconnect :active esploro--remotes]
        ("Searches" :filter esploro--searches-menu)
        "---"
        ["The Trash" esploro-show-trash]
        ["Restore from the Trash" esploro-restore :active (esploro--in-trash-p)]
        ["Empty the Trash..." esploro-empty-trash])
    (esploro-help "Help"
               ["Esploro Manual" esploro-manual :keys "?"]
               ["Keys and Mouse" esploro-manual-keys]
               "---"
               ["Your Sorting Rules" esploro-sorting-rules])))

(defconst esploro--hidden-menus
  '(options buffer tools operate mark regexp immediate subdir)
  "Emacs's menus (Options, Buffers, Tools) and dired's, hidden in Esploro.")

;; Esploro's menus have keys of their own (esploro-file, not file): Emacs
;; merges the menus of one key from every keymap, so with Emacs's own
;; `file' its File menu had Emacs's File items too.  In Esploro's buffers a
;; keymap above all others (an emulation keymap) hides every menu that isn't
;; Esploro's: Emacs's, dired's, and other packages' (a Virtual Envs, say),
;; whatever they are, as they come.

(defvar-local esploro--menus-only nil
  "Non-nil in Esploro's buffers: only Esploro's menus in the menu bar.")

(defvar esploro-menu-hider-map (make-sparse-keymap)
  "Every menu-bar menu that isn't Esploro's, undefined: above all keymaps
in Esploro's buffers.")

(defvar esploro--menu-hider-alist `((esploro--menus-only . ,esploro-menu-hider-map)))
(add-to-list 'emulation-mode-map-alists 'esploro--menu-hider-alist)

(defun esploro--hide-other-menus ()
  "Before the menu bar is drawn, in an Esploro buffer: any menu not
Esploro's that has turned up since is hidden too."
  (when esploro--menus-only
    (dolist (map (current-active-maps))
      (let ((bar (and (not (eq map esploro-menu-hider-map)) (lookup-key map [menu-bar]))))
        (when (keymapp bar)
          (map-keymap
           (lambda (key _def)
             (when (and (symbolp key)
                        (not (eq key 'mouse-1))
                        (not (string-prefix-p "esploro-" (symbol-name key)))
                        ;; Not yet hidden (a number means no [menu-bar] at all yet).
                        (not (eq (lookup-key esploro-menu-hider-map (vector 'menu-bar key)) 'undefined)))
               (define-key esploro-menu-hider-map (vector 'menu-bar key) 'undefined)))
           bar))))))

(add-hook 'menu-bar-update-hook #'esploro--hide-other-menus)

;; Loaded again (an update reloads it), the buffers already open get it too.
(with-eval-after-load 'esploro
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (bound-and-true-p esploro-mode)
                (memq major-mode '(esploro-places-mode esploro-project-mode esploro-habits-mode esploro-review-mode
                           esploro-changes-mode)))
        (setq esploro--menus-only t))
      ;; Views open from before: what's done after a folder is shown, as
      ;; esploro-mode now sets it (new things, like git status, included).
      (when (bound-and-true-p esploro-mode)
        (dolist (f '(esploro--whole-row-drag esploro--annotate esploro--thumbnails-show
                     esploro--grid-after-readin esploro--git-show esploro--space-after-readin
                     esploro--dropbox-show))
          (add-hook 'dired-after-readin-hook f nil t))))))

(defun esploro--install-menu-bar (map)
  "Esploro's menus in MAP, in order, and Emacs's and dired's hidden."
  (dolist (key esploro--hidden-menus)
    (define-key map (vector 'menu-bar key) 'undefined))
  (pcase-dolist (`(,key ,name . ,items) esploro--menu-bar)
    (define-key-after map (vector 'menu-bar key)
      (cons name (easy-menu-create-menu name items)))))

(esploro--install-menu-bar esploro-mode-map)

(easy-menu-define esploro-file-menu nil
  "Right-click on a file."
  '("File"
    ["Open" esploro-open]
    ["Open With..." esploro-open-with]
    ("Open on Workspace" :filter esploro--workspaces-menu)
    ("Commands" :filter esploro--commands-menu)
    "---"
    ["Copy" esploro-copy]
    ["Cut" esploro-cut]
    ["Duplicate" esploro-duplicate]
    ["Rename..." esploro-rename]
    ["Move To..." esploro-move-to]
    ["Copy To..." esploro-copy-to]
    ["Copy to Other Pane" esploro-copy-to-other-pane :visible (esploro--two-panes-p)]
    ["Move to Other Pane" esploro-move-to-other-pane :visible (esploro--two-panes-p)]
    "---"
    ["Repeat Last Change" esploro-repeat :keys "z"]
    ("Recipes" :filter esploro--recipes-menu)
    "---"
    ["Move to Trash" esploro-trash :visible (not (esploro--in-trash-p))]
    ["Restore" esploro-restore :visible (esploro--in-trash-p)]
    ["Properties" esploro-properties]))

(easy-menu-define esploro-folder-menu nil
  "Right-click on the folder (no file under the mouse)."
  '("Folder"
    ["Paste" esploro-paste]
    ["New Folder..." esploro-new-folder]
    ["Select All" esploro-select-all]
    ["Undo" esploro-undo]
    "---"
    ["Search Below..." esploro-search]
    ["Keep This Search..." esploro-save-search :visible esploro--search]
    ["Terminal Here" esploro-terminal-here]
    ["Bookmark This Folder..." esploro-bookmark-folder :visible (not (esploro--bookmarked-p (esploro--dir)))]
    ["Remove Bookmark" esploro-remove-bookmark :visible (esploro--bookmarked-p (esploro--dir))]
    ["Close Project..." esploro-close-project]
    "---"
    ["Hidden Files" esploro-toggle-hidden :style toggle :selected esploro--hidden]
    ["Sort by Name" (esploro-sort 'name) :style radio :selected (eq esploro--sort 'name)]
    ["Sort by Size" (esploro-sort 'size) :style radio :selected (eq esploro--sort 'size)]
    ["Sort by Time" (esploro-sort 'time) :style radio :selected (eq esploro--sort 'time)]
    ["Sort by Kind" (esploro-sort 'kind) :style radio :selected (eq esploro--sort 'kind)]
    ["Two Panes" esploro-split :style toggle :selected (esploro--two-panes-p)]
    ["Refresh" esploro--refresh]
    "---"
    ["Empty the Trash..." esploro-empty-trash :visible (esploro--in-trash-p)]))

(defvar esploro-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (dolist (item '((esploro-back "left-arrow" "Back")
                    (esploro-forward "right-arrow" "Forward")
                    (esploro-up "up-arrow" "Up")
                    (esploro-home "home" "Home")
                    (esploro-places-toggle "index" "Places")
                    (esploro-split "next-page" "Two Panes")
                    (esploro-preview-toggle "show" "Preview")
                    nil
                    (esploro-new-folder "new" "New Folder")
                    (esploro-copy "copy" "Copy")
                    (esploro-cut "cut" "Cut")
                    (esploro-paste "paste" "Paste")
                    (esploro-trash "delete" "Trash")
                    (esploro-undo "undo" "Undo")
                    nil
                    (esploro-search "search" "Search")
                    (esploro--refresh "refresh" "Refresh")
                    (esploro-manual "help" "Help")))
      (if (null item)
          (define-key-after map (vector (gensym "sep")) menu-bar-separator)
        (tool-bar-local-item (nth 1 item) (nth 0 item) (nth 0 item) map
                             :label (nth 2 item) :help (nth 2 item))))
    map)
  "Esploro's tool bar.")

(defun esploro--header ()
  (concat " " (if esploro--duplicates
                  (format "Duplicates below %s: %d %s, the same byte for byte   Edit > Trash the Copies..."
                          (abbreviate-file-name (car esploro--duplicates)) (length (cdr esploro--duplicates))
                          (if (= 1 (length (cdr esploro--duplicates))) "group" "groups"))
                (if esploro--space
                  (esploro--space-header)
                (if esploro--recent "Recent: the files opened lately, newest first   F5 looks again"
                (if esploro--search
                  (pcase-let ((`(,words ,root ,name) esploro--search))
                    (format "%s%s below %s   F5 looks again" (if name (concat name ": ") "") words
                            (abbreviate-file-name root)))
                (or (esploro--archive-heading (esploro--dir))
                    (esploro--remote-heading (esploro--dir))
                    (abbreviate-file-name (esploro--dir)))))))
          ;; Space and Recent have an order of their own.
          (if (or esploro--space esploro--recent esploro--duplicates) ""
            (format "   sorted by %s%s" esploro--sort (if esploro--reverse ", the other way" "")))
          (if (and esploro--hidden (not esploro--space)) "   hidden shown" "")
          (if esploro--filter (format "   only \"%s\" (F5: all)" esploro--filter) "")
          (if (cadr esploro--git) (format "   git: %s" (esploro--git-branch-words (cadr esploro--git))) "")
          (if (car esploro--dropbox)
              (format "   Dropbox: %s%s" (downcase (car esploro--dropbox))
                      (if (nth 2 esploro--dropbox) (format ", %d folders online only" (nth 2 esploro--dropbox)) ""))
            "")
          (if (esploro--in-trash-p) "   the Trash: Restore and Empty on the menus" "")))

(define-minor-mode esploro-mode
  "Esploro, on a dired buffer: menus, the tool bar, the mouse, and changes
through the core, journaled so they can be undone."
  :lighter " Esploro"
  :keymap esploro-mode-map
  (when esploro-mode
    ;; A double click opens; one click only selects.
    (setq-local mouse-1-click-follows-link 'double)
    ;; Dragging a name out gives the file to the program it's dropped on.
    (setq-local dired-mouse-drag-files t)
    (setq-local dnd-protocol-alist (cons '("^file:" . esploro--dnd-file) dnd-protocol-alist))
    (setq-local tool-bar-map esploro-tool-bar-map)
    (setq-local esploro--menus-only t)
    (setq-local header-line-format '(:eval (esploro--header)))
    (add-hook 'post-command-hook #'esploro--note-window nil t)
    (add-hook 'post-command-hook #'esploro--preview-schedule nil t)
    (add-hook 'dired-after-readin-hook #'esploro--whole-row-drag nil t)
    (add-hook 'dired-after-readin-hook #'esploro--annotate nil t)
    (add-hook 'dired-after-readin-hook #'esploro--thumbnails-show nil t)
    (add-hook 'dired-after-readin-hook #'esploro--grid-after-readin nil t)
    (add-hook 'dired-after-readin-hook #'esploro--git-show nil t)
    (add-hook 'dired-after-readin-hook #'esploro--space-after-readin nil t)
    (add-hook 'dired-after-readin-hook #'esploro--dropbox-show nil t)))

;;; --- Places, down the side --------------------------------------------------------------

(defun esploro--bookmarks ()
  "GTK's bookmarks, as other file managers keep them: (NAME . FOLDER)."
  (let ((file (expand-file-name "gtk-3.0/bookmarks" (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (delq nil (mapcar (lambda (line)
                            (when-let* ((uri (car (split-string line " ")))
                                        (dir (esploro--uri-file uri)))
                              (cons (if (string-match " \\(.+\\)\\'" line) (match-string 1 line)
                                      (file-name-nondirectory (directory-file-name dir)))
                                    dir)))
                          (split-string (buffer-string) "\n" t)))))))

(defun esploro--bookmarks-file ()
  (expand-file-name "gtk-3.0/bookmarks" (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun esploro--bookmarked-p (dir)
  (rassoc (directory-file-name (expand-file-name dir))
          (mapcar (lambda (b) (cons (car b) (directory-file-name (cdr b)))) (esploro--bookmarks))))

(defun esploro-bookmark-folder (name)
  "Bookmark this folder as NAME: down the side under Bookmarks, and in
PCManFM's and the file dialogs' (GTK's bookmarks, which they share)."
  (interactive (list (esploro--in-view
                      (read-string "Bookmark this folder as: "
                                   (file-name-nondirectory (directory-file-name (esploro--dir)))))))
  (let* ((dir (directory-file-name (esploro--in-view (esploro--dir))))
         (file (esploro--bookmarks-file))
         (default (file-name-nondirectory dir)))
    (when (esploro--bookmarked-p dir) (user-error "%s is bookmarked already" (abbreviate-file-name dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (when (file-readable-p file) (insert-file-contents file))
      (goto-char (point-max))
      (unless (or (bobp) (eq (char-before) ?\n)) (insert "\n"))
      ;; GTK's form: the folder's URI, then its name when it isn't the folder's own.
      (insert (esploro--uri dir) (if (or (string-empty-p name) (equal name default)) "" (concat " " name)) "\n")
      (write-region nil nil file nil 'silent))
    (esploro-places-refresh)
    (message "Esploro: %s is under Bookmarks" (abbreviate-file-name dir))))

(defun esploro-remove-bookmark ()
  "Take this folder out of the bookmarks."
  (interactive)
  (let* ((dir (directory-file-name (esploro--in-view (esploro--dir))))
         (file (esploro--bookmarks-file)))
    (unless (esploro--bookmarked-p dir) (user-error "%s isn't bookmarked" (abbreviate-file-name dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
               (uri-dir (esploro--uri-file (car (split-string line " ")))))
          (if (and uri-dir (equal (directory-file-name uri-dir) dir))
              (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
            (forward-line 1))))
      (write-region nil nil file nil 'silent))
    (esploro-places-refresh)
    (message "Esploro: %s is no longer bookmarked" (abbreviate-file-name dir))))

(defun esploro--drives ()
  "Mounted drives (udiskie mounts them in /run/media/USER): (NAME . FOLDER)."
  (let ((media (format "/run/media/%s" user-login-name)))
    (when (file-directory-p media)
      (mapcar (lambda (d) (cons (file-name-nondirectory d) d))
              (directory-files media t "\\`[^.]")))))

(defun esploro--places ()
  "Everything down the side, in groups: ((GROUP (NAME . FOLDER)...)...)."
  (seq-filter #'cdr
              (list (cons "Places" (cons (cons "Recent" 'recent)
                                         (seq-filter (lambda (p) (file-directory-p (cdr p)))
                                                     (mapcar (lambda (p) (cons (car p) (expand-file-name (cdr p))))
                                                             esploro-places))))
                    (cons "Drives" (esploro--drives))
                    (cons "Bookmarks" (esploro--bookmarks))
                    (cons "Servers" (esploro--servers))
                    (cons "Searches" (esploro--searches))
                    (cons "" (list (cons "Trash" (esploro--trash-dir)))))))

(defvar-keymap esploro-places-mode-map
  :doc "Esploro's places."
  :parent special-mode-map
  "<remap> <save-buffers-kill-terminal>" #'esploro-close
  "<remap> <save-buffers-kill-emacs>" #'esploro-close)

(esploro--install-menu-bar esploro-places-mode-map)

(define-derived-mode esploro-places-mode special-mode "Places"
  "Esploro's places: click one, or RET on it."
  (setq-local esploro--menus-only t)
  (setq-local cursor-type nil)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (setq-local mode-line-format nil))

(defun esploro-places-refresh ()
  (when-let* ((buffer (get-buffer esploro-places-buffer-name)))
    ;; One places buffer for every Esploro frame: it shows where the pane
    ;; used last, in the frame you're in, is.  A place is a folder, or
    ;; (search . NAME).
    (let* ((view (when-let* ((frame (esploro--frame))) (esploro--view frame)))
           (search (and view (nth 2 (buffer-local-value 'esploro--search view))))
           (here (cond ((and view (buffer-local-value 'esploro--recent view)) 'recent)
                       (search (cons 'search search))
                       (t (and view (esploro--dir view))))))
     (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (group (esploro--places))
          (unless (string-empty-p (car group))
            (insert (propertize (car group) 'face 'bold) "\n"))
          (dolist (place (cdr group))
            (insert "  ")
            (insert-text-button (car place)
                                'action (cond ((stringp (cdr place)) (lambda (_) (esploro--from-places (cdr place))))
                                              ((eq (car-safe (cdr place)) 'remote) (lambda (_) (esploro-connect (cddr place))))
                                              ((eq (cdr place) 'recent) (lambda (_) (esploro--from-places-recent)))
                                              (t (lambda (_) (esploro-run-search (cddr place)))))
                                'follow-link t
                                'help-echo (cond ((stringp (cdr place)) (abbreviate-file-name (cdr place)))
                                                 ((eq (cdr place) 'recent) "The files opened lately")
                                                 ((eq (car-safe (cdr place)) 'remote) "Connect to it")
                                                 (t "A search"))
                                'face (if (equal (if (stringp (cdr place)) (file-name-as-directory (cdr place)) (cdr place))
                                                 here)
                                          'highlight 'default))
            (insert "\n"))
          (insert "\n"))
        (goto-char (point-min)))))))

(defun esploro--from-places (dir)
  "DIR in the pane used last of the frame whose places were clicked."
  (let ((frame (esploro--frame)))
    (when frame
      (with-selected-frame frame
        (select-window (esploro--main-window frame))
        (esploro-go dir)))))

(defun esploro-places-show ()
  "Places down the side of Esploro's frame."
  (interactive)
  (let ((buffer (get-buffer-create esploro-places-buffer-name)))
    (with-current-buffer buffer (unless (derived-mode-p 'esploro-places-mode) (esploro-places-mode)))
    (esploro-places-refresh)
    (display-buffer-in-side-window buffer '((side . left) (slot . 0) (window-width . 22)
                                            (preserve-size . (t . nil))
                                            (window-parameters (no-delete-other-windows . t)
                                                               (no-other-window . t))))))

(defun esploro-places-toggle ()
  "Show or hide the places down the side."
  (interactive)
  (let ((window (get-buffer-window esploro-places-buffer-name)))
    (if window (delete-window window) (esploro-places-show))))

;;; --- Help -----------------------------------------------------------------------------------

;;; --- The preview --------------------------------------------------------------------------

;; Beside the folder, the file under the cursor: a picture, a PDF's first
;; page or a frame of a video (the core makes the thumbnail once, in the
;; background, and keeps it in ~/.cache/esploro), a text's first lines in
;; their colours, what's in a folder, or what file(1) says, with the size,
;; the time and the windows that have it. Each Esploro window has its own;
;; F11 shows or hides it, and hidden stays hidden (a file in
;; ~/.local/state/esploro says so).

(defcustom esploro-preview-delay 0.15
  "Seconds after the cursor stops before the preview follows it."
  :type 'number)

(defcustom esploro-preview-text-bytes 16384
  "How much of a text file the preview shows."
  :type 'integer)

(defconst esploro--image-types
  '("png" "jpg" "jpeg" "gif" "webp" "svg" "bmp" "tif" "tiff" "heic" "avif" "xpm" "ico"))
(defconst esploro--video-types
  '("mp4" "mkv" "webm" "mov" "avi" "m4v" "mpg" "mpeg" "wmv" "flv" "ogv" "3gp"))

(defvar esploro--preview-timer nil)
(defvar-local esploro--preview-shown nil "What the preview shows: (FILES . MTIMES), so it isn't redone.")

(defun esploro--preview-off-file ()
  (expand-file-name "esploro/preview-off" (or (getenv "XDG_STATE_HOME") "~/.local/state")))

(defun esploro--preview-wanted-p ()
  (not (file-exists-p (esploro--preview-off-file))))

(defun esploro--preview-buffer (&optional frame)
  "FRAME's preview buffer (each Esploro window has its own), made when asked."
  (let* ((frame (or frame (selected-frame)))
         (buffer (frame-parameter frame 'esploro-preview)))
    (if (buffer-live-p buffer) buffer
      (let ((buffer (generate-new-buffer "*Esploro preview*")))
        (with-current-buffer buffer (esploro-preview-mode))
        (set-frame-parameter frame 'esploro-preview buffer)
        buffer))))

(defun esploro--preview-window (&optional frame)
  (let ((buffer (frame-parameter (or frame (selected-frame)) 'esploro-preview)))
    (and (buffer-live-p buffer) (get-buffer-window buffer (or frame (selected-frame))))))

(defun esploro--preview-show (&optional frame)
  "The preview down the right of FRAME, following its pane used last."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (display-buffer-in-side-window (esploro--preview-buffer frame)
                                     '((side . right) (slot . 1) (window-width . 0.35)
                                       (preserve-size . (t . nil))
                                       (window-parameters (no-delete-other-windows . t)
                                                          (no-other-window . t))))
      (with-current-buffer (esploro--preview-buffer frame) (setq esploro--preview-shown nil))
      (esploro--preview-update frame))))

(defun esploro-preview-toggle ()
  "Show or hide the preview; hidden stays hidden, in every Esploro window."
  (interactive)
  (let ((window (esploro--preview-window)))
    (make-directory (file-name-directory (esploro--preview-off-file)) t)
    (if window
        (progn (delete-window window)
               (with-temp-file (esploro--preview-off-file) (insert "The preview is off: F11 in Esploro turns it on.\n")))
      (when (file-exists-p (esploro--preview-off-file)) (delete-file (esploro--preview-off-file)))
      (esploro--preview-show))))

(defun esploro--preview-schedule ()
  "After a command in a view: the preview follows, once the cursor rests."
  (when (esploro--preview-window)
    (when (timerp esploro--preview-timer) (cancel-timer esploro--preview-timer))
    (setq esploro--preview-timer
          (run-with-idle-timer esploro-preview-delay nil #'esploro--preview-update (selected-frame)))))

(defun esploro--preview-target (frame)
  "What to preview in FRAME: its pane's marked files, or the file at point,
or the folder itself."
  (when-let* ((view (esploro--view frame)))
    (with-current-buffer view
      (let ((marked (seq-remove (lambda (f) (eq f t)) (dired-get-marked-files nil nil nil t))))
        (cond ((cdr marked) marked)
              ((esploro--file-at) (list (esploro--file-at)))
              (t (list (esploro--dir))))))))

(defun esploro--kind (file)
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (cond ((file-directory-p file) 'folder)
          ((member ext esploro--image-types) 'image)
          ((equal ext "pdf") 'pdf)
          ((member ext esploro--video-types) 'video)
          ((esploro--text-p file) 'text)
          (t 'other))))

(defun esploro--text-p (file)
  "True when FILE's start holds no NUL byte: text, to show as text."
  (and (file-readable-p file) (file-regular-p file)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (ignore-errors (insert-file-contents-literally file nil 0 4096))
         (not (search-forward "\0" nil t)))))

(defun esploro--preview-update (&optional frame)
  "Show in FRAME's preview what its pane has under the cursor."
  (let* ((frame (or frame (selected-frame)))
         (buffer (frame-parameter frame 'esploro-preview)))
    (when (and (frame-live-p frame) (buffer-live-p buffer) (esploro--preview-window frame))
      (let* ((files (esploro--preview-target frame))
             (key (cons files (mapcar (lambda (f) (file-attribute-modification-time (file-attributes f))) files))))
        (with-current-buffer buffer
          (unless (equal key esploro--preview-shown)
            (setq esploro--preview-shown key)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cond ((null files))
                    ((cdr files) (esploro--preview-many files))
                    (t (esploro--preview-one (car files) frame))))
            (goto-char (point-min))))))))

(defun esploro--preview-heading (file)
  (insert (propertize (file-name-nondirectory (directory-file-name file)) 'face 'bold) "\n"))

(defun esploro--preview-facts (file)
  "Size, time, and the windows that have FILE."
  (let* ((attrs (file-attributes file))
         (where (esploro--where-of file)))
    (insert "\n" (propertize
                  (format "%s%s\n" (if (file-directory-p file) ""
                                      (concat (file-size-human-readable (file-attribute-size attrs)) ", "))
                          (format-time-string "%Y-%m-%d %H:%M" (file-attribute-modification-time attrs)))
                  'face 'shadow))
    (when where (insert (propertize (concat "open in " where "\n") 'face 'esploro-where)))))

(defun esploro--where-of (file)
  "The windows that have FILE, as the pane's list says beside it."
  (when-let* ((view (esploro--view)))
    (with-current-buffer view
      (save-excursion
        (when (dired-goto-file file)
          (seq-some (lambda (o) (and (overlay-get o 'esploro-where)
                                     (string-trim (replace-regexp-in-string "\\`\\s-*open in " ""
                                                                            (overlay-get o 'after-string)))))
                    (overlays-in (line-beginning-position) (1+ (line-end-position)))))))))

(defun esploro--preview-one (file frame)
  (esploro--preview-heading file)
  (pcase (esploro--kind file)
    ('folder (esploro--preview-folder file))
    ('text (esploro--preview-text file))
    ((or 'image 'pdf 'video) (esploro--preview-picture file frame))
    (_ (insert (or (ignore-errors (car (process-lines "file" "-b" "--" file))) "a file") "\n")))
  (esploro--preview-facts file))

(defun esploro--preview-many (files)
  (let ((size (apply #'+ (mapcar (lambda (f) (or (and (file-regular-p f) (file-attribute-size (file-attributes f))) 0))
                                 files))))
    (insert (propertize (format "%d selected" (length files)) 'face 'bold)
            (format ", %s in files\n\n" (file-size-human-readable size)))
    (dolist (f (seq-take files 60))
      (insert (file-name-nondirectory (directory-file-name f)) (if (file-directory-p f) "/" "") "\n"))
    (when (> (length files) 60) (insert "...\n"))))

(defun esploro--preview-folder (dir)
  (let* ((names (seq-remove (lambda (n) (member n '("." ".."))) (ignore-errors (directory-files dir))))
         (shown (seq-remove (lambda (n) (string-prefix-p "." n)) names)))
    (insert (format "a folder of %d%s\n\n" (length shown)
                    (if (= (length shown) (length names)) ""
                      (format " (and %d hidden)" (- (length names) (length shown))))))
    (dolist (n (seq-take shown 40))
      (insert n (if (file-directory-p (expand-file-name n dir)) "/" "") "\n"))
    (when (> (length shown) 40) (insert "...\n"))))

(defun esploro--preview-text (file)
  "FILE's start, in the colours of its mode, fontified apart from this buffer."
  (let ((text (with-temp-buffer
                (ignore-errors (insert-file-contents file nil 0 esploro-preview-text-bytes))
                (let ((buffer-file-name file))
                  (ignore-errors (delay-mode-hooks (set-auto-mode t)))
                  (ignore-errors (font-lock-ensure)))
                (buffer-string))))
    (insert "\n" text)
    (unless (bolp) (insert "\n"))))

(defun esploro--preview-picture (file frame)
  "A picture, or the thumbnail the core makes of a picture, PDF or video."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (if (and (member ext '("png" "jpg" "jpeg" "gif" "webp" "svg"))
             (< (or (file-attribute-size (file-attributes file)) 0) 3000000)
             (image-supported-file-p file))
        (esploro--preview-insert-image file frame)
      (insert (propertize "making a preview...\n" 'face 'shadow 'esploro-pending t))
      (let ((buffer (current-buffer))
            (key esploro--preview-shown))
        (esploro--call (list "preview" file) nil
                       (lambda (answer)
                         (esploro--preview-thumbnail-came buffer key file frame answer)))))))

(defun esploro--preview-thumbnail-came (buffer key file frame answer)
  "The core's ANSWER for FILE's thumbnail: in place of \"making a preview\",
when BUFFER still shows what it did (KEY) when it was asked."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (equal key esploro--preview-shown)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (when-let* ((pending (text-property-search-forward 'esploro-pending t t)))
              (delete-region (prop-match-beginning pending) (prop-match-end pending))
              (goto-char (prop-match-beginning pending))
              (pcase answer
                (`(:thumbnail ,png) (esploro--preview-insert-image png frame))
                (_ (insert (or (ignore-errors (car (process-lines "file" "-b" "--" file)))
                               "no preview")
                           "\n"))))))))))

(defun esploro--preview-insert-image (file frame)
  (let* ((window (esploro--preview-window frame))
         (width (max 100 (- (if window (window-body-width window t) 400) 8))))
    (if (display-images-p)
        (insert-image (create-image file nil nil :max-width width :max-height (* 2 width)))
      ;; No pictures here (a terminal frame, or the tests in batch).
      (insert (propertize (concat "[picture: " file "]") 'esploro-image file)))
    (insert "\n")))

(define-derived-mode esploro-preview-mode special-mode "Preview"
  "Esploro's preview of the file under the cursor."
  (setq-local cursor-type nil)
  (setq-local mode-line-format nil)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (visual-line-mode 1))

;;; --- Show in folder: org.freedesktop.FileManager1 ---------------------------------------

;; Browsers' "Show in folder" (Firefox, Chromium), and other programs that
;; ask for the file manager over D-Bus, reach Esploro: it shows the folder,
;; with the file selected, on your workspace, as Super+e decides (the
;; esploro command asks StumpWM). Registered by `esploro-dbus-register',
;; which `esploro --dbus' runs: Vikix makes the bus start that the first
;; time it's asked.

(declare-function dbus-register-service "dbus" (bus service &rest flags))
(declare-function dbus-register-method "dbus" (bus service path interface method handler &optional dont-register-service))

(defconst esploro--dbus-name "org.freedesktop.FileManager1")
(defconst esploro--dbus-path "/org/freedesktop/FileManager1")

(defun esploro--dbus-show (uris)
  "Each of URIS (file:// ones), shown by the esploro command, in the
background: never wait inside a D-Bus call."
  (dolist (uri uris)
    (when-let* ((file (esploro--uri-file uri)))
      (when (executable-find esploro-program)
        (call-process esploro-program nil 0 nil (directory-file-name file)))))
  :ignore)

(defun esploro-dbus-register ()
  "Answer org.freedesktop.FileManager1 on the session bus: ShowFolders,
ShowItems and ShowItemProperties open Esploro there."
  (interactive)
  (require 'dbus)
  (dbus-register-service :session esploro--dbus-name :replace-existing)
  (dolist (method '("ShowFolders" "ShowItems" "ShowItemProperties"))
    (dbus-register-method :session esploro--dbus-name esploro--dbus-path esploro--dbus-name
                          method (lambda (uris _startup-id) (esploro--dbus-show uris))))
  (when (called-interactively-p 'any)
    (message "Esploro answers Show in folder (%s)" esploro--dbus-name))
  t)

;;; --- File commands: the core's, and yours ----------------------------------------------

;; The commands the core defines for each kind of file (define-file-command;
;; yours in ~/.config/esploro/commands.lisp), on the File menu and the
;; right-click menu under Commands. One that changes files proposes a plan,
;; which waits for you in the review panel.

(defconst esploro--commands-on-menus '("duplicate" "trash" "show-in-esploro" "terminal-here")
  "The core's file commands Esploro's own menus have already, by other names
or not: left out of Commands, so nothing is there twice.")

(defun esploro--commands-menu (_items)
  "The Commands submenu, made when it opens: what suits the selection,
but what Esploro's own menus already do."
  (let ((files (and (esploro--view) (with-current-buffer (esploro--view) (esploro--selection)))))
    (if (null files)
        (list ["Select a file first" ignore :active nil])
      (let ((answer (esploro--call (cons "commands" files) nil nil t)))
        (if (and (listp answer) (not (keywordp (car answer))) answer)
            (mapcar (lambda (c)
                      (vector (concat (nth 1 c) (if (nth 3 c) "..." ""))
                              (list 'esploro-run-command (nth 0 c))
                              :help (nth 2 c)))
                    (seq-remove (lambda (c) (member (nth 0 c) esploro--commands-on-menus)) answer))
          (list ["No commands for these" ignore :active nil]))))))

(defun esploro-run-command (name &optional files)
  "Run the file command NAME on FILES (the selection): at once, or, for one
that changes files, as a plan for your review."
  (interactive (list (read-string "Command: ")))
  (let ((files (or files (esploro--in-view (esploro--selection)) (user-error "Nothing selected"))))
    (esploro--call (append (list "run" name) files) nil
                   (lambda (answer)
                     (pcase answer
                       (`(:done ,_ :made ,made)
                        (message "Esploro: %s made %s" name
                                 (mapconcat #'file-name-nondirectory made ", "))
                        (esploro--refresh))
                       (`(:done ,_) (message "Esploro: %s, done" name) (esploro--refresh))
                       (`(:proposed ,n) (message "Esploro: %s proposes %d %s: review it" name n (if (= n 1) "step" "steps")))
                       (_ (esploro--say answer name)))))))

;;; --- Git: what's changed, in a repository -----------------------------------------------

;; In a folder inside a git repository, each file git has something to say
;; about says it after its name (modified, new, staged, conflict), a folder
;; with changes inside says so, and the top line has the branch and what's
;; waiting to be pushed or pulled.  git is asked in the background, after
;; each showing of the folder.

(defcustom esploro-git-status t
  "Non-nil: in a git repository, what git says of each file, after its name."
  :type 'boolean :group 'esploro)

(defface esploro-git-modified '((t :inherit warning :weight normal))
  "A file changed since the last commit.")
(defface esploro-git-new '((t :inherit success :weight normal))
  "A file git doesn't follow yet.")
(defface esploro-git-staged '((t :inherit font-lock-keyword-face))
  "A change staged for the next commit.")
(defface esploro-git-conflict '((t :inherit error))
  "A file a merge left in conflict.")

(defun esploro--git-state (xy)
  "Git's two letters (staged, then not) for a file, as Esploro says it."
  (let ((x (aref xy 0)) (y (aref xy 1)))
    (cond ((equal xy "??") 'new)
          ((or (eq x ?U) (eq y ?U) (member xy '("AA" "DD"))) 'conflict)
          ((memq y '(?M ?D ?T)) 'modified)
          ((eq x ?A) 'added)
          ((memq x '(?M ?R ?C ?D ?T)) 'staged))))

(defun esploro--git-parse (text)
  "git status --porcelain=v1 -b -z's TEXT: (BRANCH-LINE . ((PATH . STATE) ...))."
  (let ((entries (split-string text "\0" t)) (branch nil) (states '()))
    (while entries
      (let ((e (pop entries)))
        (cond ((string-prefix-p "## " e) (setq branch (substring e 3)))
              ((> (length e) 3)
               (let ((state (esploro--git-state (substring e 0 2))))
                 (when state (push (cons (substring e 3) state) states))
                 ;; A rename or copy: the old path follows; it's not a file here.
                 (when (memq (aref e 0) '(?R ?C)) (pop entries)))))))
    (cons branch states)))

(defun esploro--git-branch-words (line)
  "\"main...origin/main [ahead 2, behind 1]\" as \"main, 2 to push, 1 to pull\"."
  (when line
    (let* ((line (replace-regexp-in-string "\\`No commits yet on " "" line))
           (name (car (split-string line "\\.\\.\\.\\| ")))
           (ahead (and (string-match "ahead \\([0-9]+\\)" line) (match-string 1 line)))
           (behind (and (string-match "behind \\([0-9]+\\)" line) (match-string 1 line))))
      (concat name
              (if ahead (format ", %s to push" ahead) "")
              (if behind (format ", %s to pull" behind) "")))))

(defun esploro--git-show ()
  "Ask git, in the background, about this folder's repository, then mark the files."
  (remove-overlays (point-min) (point-max) 'esploro-git t)
  (setq esploro--git nil)
  (let* ((dir (and esploro-git-status (not (consp dired-directory)) (executable-find "git")
                   (expand-file-name default-directory)))
         (root (and dir (not (esploro--archive-for-path dir)) (locate-dominating-file dir ".git"))))
    (when root
      (let ((buffer (current-buffer))
            (out (generate-new-buffer " *esploro-git*"))
            (default-directory root))
        (make-process
         :name "esploro-git" :buffer out :noquery t :connection-type 'pipe
         :command (list "git" "-C" (expand-file-name root) "status" "--porcelain=v1" "-b" "-z" "--untracked-files=normal")
         :stderr (make-pipe-process :name "esploro-git-err" :noquery t :filter #'ignore)
         :sentinel (lambda (process _event)
                     (unless (process-live-p process)
                       (let ((text (with-current-buffer out (buffer-string))))
                         (kill-buffer out)
                         (when (and (buffer-live-p buffer) (zerop (process-exit-status process)))
                           (with-current-buffer buffer
                             (when (equal (expand-file-name default-directory) dir)
                               (setq esploro--git (cons (expand-file-name root) (esploro--git-parse text)))
                               (esploro--git-mark)
                               (force-mode-line-update))))))))))))

(defun esploro--git-mark ()
  "Each file's state after its name; a folder with changes inside says so."
  (remove-overlays (point-min) (point-max) 'esploro-git t)
  (pcase-let ((`(,root ,_branch . ,states) esploro--git))
    (when root
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((file (esploro--grid-file)))
            (when (and file (dired-move-to-end-of-filename t))
              (let* ((rel (file-relative-name file root))
                     (dirp (file-directory-p file))
                     (state (cdr (assoc (if dirp (file-name-as-directory rel) rel) states)))
                     (inside (and dirp (not state)
                                  (seq-some (lambda (s) (string-prefix-p (file-name-as-directory rel) (car s))) states))))
                (when (or state inside)
                  (let ((o (make-overlay (point) (point))))
                    (overlay-put o 'esploro-git t)
                    (overlay-put o 'after-string
                                 (if inside
                                     (propertize "  changes inside" 'face 'esploro-git-modified)
                                   (propertize (format "  %s" state)
                                               'face (pcase state
                                                       ('new 'esploro-git-new) ('conflict 'esploro-git-conflict)
                                                       ((or 'staged 'added) 'esploro-git-staged)
                                                       (_ 'esploro-git-modified))))))))))
          (forward-line 1))))))

;;; --- Dropbox: what's synced ---------------------------------------------------------------

;; In a folder inside Dropbox, each entry says what Dropbox has done with it
;; (synced, syncing, can't sync), and the top line says how Dropbox is
;; (up to date, syncing, not running); at Dropbox's top, how many folders are
;; kept online only (selective sync).  Dropbox's own command is asked in the
;; background, after each showing of the folder.

(defcustom esploro-dropbox-status t
  "Non-nil: in Dropbox's folder, what Dropbox has done with each file."
  :type 'boolean :group 'esploro)

(defface esploro-dropbox-synced '((t :inherit shadow))
  "A file Dropbox has up to date.")
(defface esploro-dropbox-syncing '((t :inherit warning :weight normal))
  "A file Dropbox is syncing now.")
(defface esploro-dropbox-unsyncable '((t :inherit error))
  "A file Dropbox can't sync.")

(defun esploro--dropbox-folder ()
  "Where Dropbox keeps its files on this machine (its info.json says), or nil."
  (let ((info (expand-file-name "~/.dropbox/info.json")))
    (when (and (executable-find "dropbox") (file-readable-p info))
      (ignore-errors
        (let* ((json (with-temp-buffer (insert-file-contents info) (json-parse-buffer :object-type 'alist)))
               (path (alist-get 'path (alist-get 'personal json))))
          (and (stringp path) (file-directory-p path) (file-name-as-directory path)))))))

(defun esploro--dropbox-parse (text)
  "dropbox filestatus's TEXT: ((NAME . STATE) ...), STATE a symbol."
  (delq nil (mapcar (lambda (line)
                      (when (string-match "\\`\\(.*?\\):[ \t]+\\([^:]+\\)\\'" line)
                        (cons (match-string 1 line)
                              (pcase (match-string 2 line)
                                ("up to date" 'synced) ("syncing" 'syncing)
                                ("unsyncable" 'unsyncable) (_ nil)))))
                    (split-string text "\n" t))))

(defun esploro--dropbox-run (args then)
  "Dropbox's command with ARGS, in the background; THEN gets its output."
  (let ((out (generate-new-buffer " *esploro-dropbox*")))
    (make-process :name "esploro-dropbox" :buffer out :noquery t :connection-type 'pipe
                  :command (cons "dropbox" args)
                  :stderr (make-pipe-process :name "esploro-dropbox-err" :noquery t :filter #'ignore)
                  :sentinel (lambda (process _event)
                              (unless (process-live-p process)
                                (let ((text (with-current-buffer out (buffer-string))))
                                  (kill-buffer out)
                                  (funcall then text)))))))

(defun esploro--dropbox-show ()
  "In Dropbox's folder: ask Dropbox, in the background, then mark the entries."
  (remove-overlays (point-min) (point-max) 'esploro-dropbox t)
  (setq esploro--dropbox nil)
  (let* ((top (and esploro-dropbox-status (not (consp dired-directory)) (esploro--dropbox-folder)))
         (dir (expand-file-name default-directory)))
    (when (and top (string-prefix-p top dir))
      (let ((buffer (current-buffer))
            (names (save-excursion
                     (goto-char (point-min))
                     (let (ns) (while (not (eobp))
                                 (let ((f (esploro--grid-file))) (when f (push (file-name-nondirectory (directory-file-name f)) ns)))
                                 (forward-line 1))
                          (nreverse ns))))
            (default-directory dir))
        (esploro--dropbox-run
         '("status")
         (lambda (status)
           (let ((status (car (split-string status "\n" t))))
             (cl-flet ((finish (excluded)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (when (equal (expand-file-name default-directory) dir)
                               (setq esploro--dropbox (list status nil excluded))
                               (force-mode-line-update)
                               (when (and names (not (string-match-p "isn't running" (or status ""))))
                                 (let ((default-directory dir))
                                   (esploro--dropbox-run
                                    (cons "filestatus" names)
                                    (lambda (text)
                                      (when (buffer-live-p buffer)
                                        (with-current-buffer buffer
                                          (when (equal (expand-file-name default-directory) dir)
                                            (setf (nth 1 esploro--dropbox) (esploro--dropbox-parse text))
                                            (esploro--dropbox-mark)))))))))))))
               ;; At Dropbox's top: the folders kept online only.
               (if (and (equal dir top) status (not (string-match-p "isn't running" status)))
                   (let ((default-directory dir))
                     (esploro--dropbox-run '("exclude" "list")
                                           (lambda (text)
                                             (finish (length (cdr (seq-remove (lambda (l) (string-match-p "\\`stty" l))
                                                                              (split-string text "\n" t))))))))
                 (finish nil))))))))))

(defun esploro--dropbox-mark ()
  "Each entry's Dropbox state after its name."
  (remove-overlays (point-min) (point-max) 'esploro-dropbox t)
  (let ((states (nth 1 esploro--dropbox)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((file (esploro--grid-file))
               (state (and file (cdr (assoc (file-name-nondirectory (directory-file-name file)) states)))))
          (when (and state (dired-move-to-end-of-filename t))
            (let ((o (make-overlay (point) (point))))
              (overlay-put o 'esploro-dropbox t)
              (overlay-put o 'after-string
                           (pcase state
                             ('synced (propertize "  synced" 'face 'esploro-dropbox-synced))
                             ('syncing (propertize "  syncing" 'face 'esploro-dropbox-syncing))
                             ('unsyncable (propertize "  can't sync" 'face 'esploro-dropbox-unsyncable)))))))
        (forward-line 1)))))

;;; --- Thumbnails in the list ------------------------------------------------------------

;; With View > Thumbnails (T), each picture, PDF and video has a small
;; thumbnail before its name; the core makes them (once: they're kept),
;; a batch at a time in the background, so a big folder fills in as it
;; goes.  Other rows get the same width, blank, so the names line up.

(defcustom esploro-thumbnail-height 48
  "How tall a thumbnail in the list is, in pixels."
  :type 'integer :group 'esploro)

(defvar-local esploro--thumbnails-round 0
  "Which showing of the list the thumbnails being made are for.")

(defun esploro-thumbnails-toggle ()
  "Thumbnails in the list, or none."
  (interactive)
  (esploro--in-view
   (setq esploro--thumbnails (not esploro--thumbnails))
   (esploro--thumbnails-show)
   (message "Esploro: thumbnails %s" (if esploro--thumbnails "on" "off"))))

(defun esploro--thumbnail-wanted-p (file)
  (memq (esploro--kind file) '(image pdf video)))

(defun esploro--thumbnails-show ()
  "Thumbnails before the names (or none), the missing ones asked for in the
background, a batch at a time."
  (remove-overlays (point-min) (point-max) 'esploro-thumbnail t)
  (setq esploro--thumbnails-round (1+ esploro--thumbnails-round))
  (when esploro--thumbnails
    (let ((blank (propertize " " 'display `(space :width (,(round (* 1.34 esploro-thumbnail-height))))))
          (wanted '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((file (dired-get-filename nil t)))
            (when (and file (not (member (file-name-nondirectory file) '("." "..")))
                       (dired-move-to-filename))
              (let ((o (make-overlay (point) (point))))
                (overlay-put o 'esploro-thumbnail t)
                (overlay-put o 'before-string (concat blank " "))
                (when (esploro--thumbnail-wanted-p file)
                  (push (cons file o) wanted)))))
          (forward-line 1)))
      (esploro--thumbnails-fill (current-buffer) esploro--thumbnails-round (nreverse wanted)))))

(defun esploro--thumbnail-string (png)
  "PNG as the start of a row: the picture, then blank to the slot's width,
so every name starts in the same place."
  (let* ((slot (round (* 1.34 esploro-thumbnail-height)))
         ;; A thin edge: a white page is seen on a white background.
         (image (create-image png nil nil :max-height esploro-thumbnail-height
                              :max-width slot :ascent 'center :relief -1))
         (width (or (ignore-errors (car (image-size image t))) slot)))
    (concat (propertize " " 'display image)
            (propertize " " 'display `(space :width (,(max 0 (- slot width)))))
            " ")))

(defun esploro--thumbnails-fill (buffer round wanted)
  "Ask the core for WANTED's thumbnails ((FILE . OVERLAY) ...), a batch at a
time, putting each in its overlay, while BUFFER still shows that ROUND."
  (when wanted
    (let ((batch (seq-take wanted 12))
          (rest (seq-drop wanted 12)))
      (esploro--call (append (list "thumbnails" "--size" (number-to-string (* 2 esploro-thumbnail-height)))
                             (mapcar #'car batch))
                     nil
                     (lambda (answer)
                       (when (and (buffer-live-p buffer)
                                  (= round (buffer-local-value 'esploro--thumbnails-round buffer)))
                         (dolist (pair batch)
                           (let ((png (cdr (assoc (car pair) (and (listp answer) answer)))))
                             (when (and (stringp png) (overlay-buffer (cdr pair)))
                               (overlay-put (cdr pair) 'before-string (esploro--thumbnail-string png)))))
                         (esploro--thumbnails-fill buffer round rest)))))))

;;; --- The grid: pictures side by side ------------------------------------------------------

;; View > Grid (G) shows the folder as tiles: a thumbnail for each picture,
;; PDF and video, a drawn folder or page for the rest, the name under it.
;; Underneath it's still the list, one file a line; each line is drawn as
;; a tile and the lines of a row joined, so everything that works on the
;; list (selecting, the menus, copy and paste, the Trash, undo, the
;; preview) works on the grid.  The arrow keys go between tiles.

(defcustom esploro-grid-size 128
  "How big a picture in the grid is, in pixels."
  :type 'integer :group 'esploro)

(defvar-local esploro--grid nil "Non-nil: this view is a grid of tiles.")
(defvar-local esploro--grid-columns 0 "Tiles in a row, as laid out.")
(defvar-local esploro--grid-pngs nil "Thumbnails found for the grid: FILE -> PNG, or :none.")
(defvar-local esploro--grid-round 0 "Which laying out the thumbnails asked for are for.")
(dolist (v '(esploro--grid esploro--grid-columns esploro--grid-pngs esploro--grid-round))
  (put v 'permanent-local t))

(defvar-keymap esploro-grid-mode-map
  :doc "Esploro's keys in a grid, on top of its others."
  "<right>" #'esploro-grid-right
  "<left>" #'esploro-grid-left
  "<down>" #'esploro-grid-down
  "<up>" #'esploro-grid-up
  "C-f" #'esploro-grid-right
  "C-b" #'esploro-grid-left
  "C-n" #'esploro-grid-down
  "C-p" #'esploro-grid-up
  "n" #'esploro-grid-down
  "p" #'esploro-grid-up
  ;; A tile is drawn over its line: a double click on it opens it.
  "<double-mouse-1>" #'esploro-mouse-open)

(define-minor-mode esploro-grid-mode
  "The folder as a grid of tiles (Esploro's View > Grid)."
  :keymap esploro-grid-mode-map
  (if esploro-grid-mode
      (progn
        (setq-local cursor-type nil)
        (add-hook 'post-command-hook #'esploro--grid-refresh-tiles nil t)
        (add-hook 'window-size-change-functions #'esploro--grid-resized nil t))
    (kill-local-variable 'cursor-type)
    (remove-hook 'post-command-hook #'esploro--grid-refresh-tiles t)
    (remove-hook 'window-size-change-functions #'esploro--grid-resized t)))

(defun esploro-grid-toggle ()
  "The folder as a grid of tiles, or as the list."
  (interactive)
  (esploro--in-view
   (setq esploro--grid (not esploro--grid))
   (esploro--grid-layout)
   (message "Esploro: %s" (if esploro--grid "a grid (arrows go between tiles)" "the list"))))

(defun esploro--grid-tile-size ()
  "A tile's width and height, in pixels."
  (cons (+ esploro-grid-size 24) (+ esploro-grid-size (* 2 (frame-char-height)) 12)))

(defun esploro--grid-file ()
  "The file on this line, for a tile; nil for . and .., and the lines
that aren't files."
  (let ((file (dired-get-filename nil t)))
    (and file (not (member (file-name-nondirectory (directory-file-name file)) '("." ".."))) file)))

(defun esploro--grid-layout ()
  "Lay the view out as a grid (or as the list again), and ask for the
thumbnails it hasn't got."
  (remove-overlays (point-min) (point-max) 'esploro-grid t)
  (setq esploro--grid-round (1+ esploro--grid-round))
  (esploro-grid-mode (if esploro--grid 1 -1))
  (when esploro--grid
    (unless esploro--grid-pngs (setq esploro--grid-pngs (make-hash-table :test #'equal)))
    (let* ((window (get-buffer-window (current-buffer)))
           (width (if window (window-body-width window t) 800))
           (columns (max 1 (/ width (car (esploro--grid-tile-size)))))
           (n 0) (wanted '()))
      (setq esploro--grid-columns columns)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((start (line-beginning-position))
                 (end (line-end-position))
                 (file (esploro--grid-file)))
            (if (not file)
                ;; Headings and . and ..: not in the grid.
                (let ((o (make-overlay start (min (point-max) (1+ end)))))
                  (overlay-put o 'esploro-grid t)
                  (overlay-put o 'display ""))
              (setq n (1+ n))
              (let ((o (make-overlay start end)))
                (overlay-put o 'esploro-grid t)
                (overlay-put o 'esploro-grid-file file)
                (esploro--grid-draw o))
              ;; The line's end joins it to the next tile, but at a row's end.
              (when (and (/= 0 (mod n columns)) (< end (point-max)))
                (let ((o (make-overlay end (1+ end))))
                  (overlay-put o 'esploro-grid t)
                  (overlay-put o 'display "")))
              (when (and (memq (esploro--kind file) '(image pdf video))
                         (not (gethash file esploro--grid-pngs)))
                (push file wanted))))
          (forward-line 1)))
      (esploro--grid-thumbnails (current-buffer) esploro--grid-round (nreverse wanted)))))

(defun esploro--grid-after-readin ()
  "A folder shown again: its grid laid out again, when it's a grid."
  (if esploro--grid (esploro--grid-layout) (when esploro-grid-mode (esploro-grid-mode -1))))

(defun esploro--grid-resized (window)
  "Lay out again when the window's width changes how many tiles fit."
  (with-current-buffer (window-buffer window)
    (when (and esploro--grid
               (/= esploro--grid-columns
                   (max 1 (/ (window-body-width window t) (car (esploro--grid-tile-size))))))
      (esploro--grid-layout))))

(defun esploro--grid-state (o)
  "What O's tile looks like now: (FILE MARKED CURRENT PNG)."
  (let ((file (overlay-get o 'esploro-grid-file))
        (start (overlay-start o)))
    (list file
          (and start (not (eq (char-after start) ?\s)))
          (and start (= (line-beginning-position) (save-excursion (goto-char start) (line-beginning-position))))
          (gethash file esploro--grid-pngs))))

(defun esploro--grid-draw (o)
  "Draw the tile of overlay O, as it now is."
  (let ((state (esploro--grid-state o)))
    (unless (equal state (overlay-get o 'esploro-grid-state))
      (overlay-put o 'esploro-grid-state state)
      (overlay-put o 'display (apply #'esploro--grid-tile state)))))

(defun esploro--grid-refresh-tiles ()
  "After each command: the tiles whose selection or place changed, drawn again."
  (when esploro--grid
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'esploro-grid-file)
        (esploro--grid-draw o)))))

(defun esploro--grid-color (face attribute fallback)
  "FACE's ATTRIBUTE colour as #rrggbb, which SVG reads (Emacs's names, like
gtk_selection_bg_color, it doesn't); FALLBACK when there's none."
  (let* ((c (face-attribute face attribute nil t))
         (rgb (and (stringp c) (not (string-prefix-p "unspecified" c)) (color-name-to-rgb c))))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) fallback)))

(defun esploro--grid-tile (file marked current png)
  "FILE's tile: its thumbnail PNG (or a drawn folder or page), its name under
it; MARKED shades it, CURRENT outlines it."
  (let* ((size (esploro--grid-tile-size))
         (w (car size)) (h (cdr size)) (box esploro-grid-size)
         (fg (esploro--grid-color 'default :foreground "#333333"))
         (bg (esploro--grid-color 'default :background "#ffffff"))
         (sel (esploro--grid-color 'region :background "#b5d5ff"))
         (ring (esploro--grid-color 'link :foreground "#3366cc"))
         (svg (svg-create w h))
         (name (file-name-nondirectory (directory-file-name file)))
         (font-size (* 0.85 (frame-char-height)))
         ;; A character is about 0.6 of the font's size wide.
         (chars (max 4 (floor (- w 12) (* 0.6 font-size)))))
    (svg-rectangle svg 2 2 (- w 4) (- h 4) :rx 6 :fill (if marked sel bg)
                   :stroke (if current ring "none") :stroke-width 3)
    (cond ((stringp png)
           (svg-embed svg png "image/png" nil :x 12 :y 6 :width box :height box))
          ((file-directory-p file) (esploro--grid-folder svg 12 6 box fg))
          (t (esploro--grid-page svg 12 6 box fg (upcase (or (file-name-extension file) "")))))
    ;; The name, on one line or two, shortened in the middle when longer.
    (let* ((lines (if (<= (length name) chars) (list name)
                    (list (substring name 0 chars)
                          (let ((rest (substring name chars)))
                            (if (<= (length rest) chars) rest
                              (concat (substring rest 0 (max 1 (- chars 6))) "…" (substring rest (- (length rest) 5))))))))
           (y (+ box 6 (frame-char-height))))
      (dolist (line lines)
        (svg-text svg line :x (/ w 2) :y y :text-anchor "middle" :fill fg
                  :font-family (face-attribute 'default :family nil t)
                  :font-size font-size)
        (setq y (+ y (frame-char-height)))))
    (svg-image svg :ascent 'center)))

(defun esploro--grid-folder (svg x y box color)
  (let ((top (+ y (* 0.22 box))) (w box) (h (* 0.62 box)))
    (svg-polygon svg (list (cons x top) (cons (+ x (* 0.38 w)) top) (cons (+ x (* 0.45 w)) (+ top (* 0.1 h)))
                           (cons (+ x w) (+ top (* 0.1 h))) (cons (+ x w) (+ top h)) (cons x (+ top h)))
                 :fill "#e8c46a" :stroke color :stroke-width 1)))

(defun esploro--grid-page (svg x y box color label)
  (let* ((w (* 0.62 box)) (h (* 0.8 box)) (left (+ x (/ (- box w) 2))) (top (+ y (* 0.08 box))) (fold (* 0.22 w)))
    (svg-polygon svg (list (cons left top) (cons (- (+ left w) fold) top) (cons (+ left w) (+ top fold))
                           (cons (+ left w) (+ top h)) (cons left (+ top h)))
                 :fill "#f4f4f4" :stroke color :stroke-width 1)
    (unless (string-empty-p label)
      (svg-text svg (truncate-string-to-width label 5) :x (+ left (/ w 2)) :y (+ top (* 0.6 h))
                :text-anchor "middle" :fill color :font-size (* 0.16 box) :font-weight "bold"))))

(defun esploro--grid-thumbnails (buffer round wanted)
  "Ask the core for WANTED's thumbnails, a batch at a time, drawing each
tile again as its thumbnail comes, while BUFFER is still laid out as ROUND."
  (when wanted
    (let ((batch (seq-take wanted 12)) (rest (seq-drop wanted 12)))
      (esploro--call (append (list "thumbnails" "--size" (number-to-string (* 2 esploro-grid-size))) batch) nil
                     (lambda (answer)
                       (when (and (buffer-live-p buffer)
                                  (= round (buffer-local-value 'esploro--grid-round buffer)))
                         (with-current-buffer buffer
                           (dolist (file batch)
                             (puthash file (or (cdr (assoc file (and (listp answer) answer))) :none)
                                      esploro--grid-pngs))
                           (esploro--grid-refresh-tiles))
                         (esploro--grid-thumbnails buffer round rest)))))))

(defun esploro--grid-move (lines)
  "LINES files on (or back, when negative), in the list's order."
  (let ((start (point)) (moved 0) (step (if (< lines 0) -1 1)))
    (while (and (< moved (abs lines)) (zerop (forward-line step)))
      (when (esploro--grid-file) (setq moved (1+ moved))))
    (if (and (= moved (abs lines)) (esploro--grid-file))
        (dired-move-to-filename)
      (goto-char start))))

(defun esploro-grid-right () "The next tile." (interactive) (esploro--grid-move 1))
(defun esploro-grid-left () "The tile before." (interactive) (esploro--grid-move -1))
(defun esploro-grid-down () "The tile below." (interactive) (esploro--grid-move esploro--grid-columns))
(defun esploro-grid-up () "The tile above." (interactive) (esploro--grid-move (- esploro--grid-columns)))

;;; --- Archives, opened like folders (read-only) --------------------------------------

;; Opening a zip, a tarball, a 7z or an ISO mounts it read-only (the core,
;; with archivemount) and goes in: look, preview, open, copy out, drag out.
;; Nothing can be changed inside (the core refuses: Extract Here for that).
;; Up from its top goes back beside it; leaving it, no view showing it,
;; closes it again.

(defconst esploro--archive-types
  "\\.\\(zip\\|jar\\|tar\\|tgz\\|tbz2?\\|txz\\|tzst\\|7z\\|rar\\|iso\\|cpio\\|deb\\|rpm\\|tar\\.\\(gz\\|bz2\\|xz\\|zst\\|lz4\\|lzma\\)\\)\\'"
  "Names of the archives Esploro opens like folders.")

(defun esploro--archive-p (file)
  (let ((case-fold-search t)) (string-match-p esploro--archive-types file)))

(defun esploro--archive-for-path (path)
  "The archive whose mount point PATH is in (open now or not)."
  (let ((path (directory-file-name (expand-file-name path))))
    (car (seq-find (lambda (a) (or (equal path (cdr a)) (string-prefix-p (file-name-as-directory (cdr a)) path)))
                   esploro--archives))))

(defun esploro--archive-heading (dir)
  "DIR's place in an archive, for the header: \"inside ~/x.zip (read-only): a/b\"."
  (when-let* ((archive (esploro--archive-for-path dir)))
    (let* ((point (cdr (assoc archive esploro--archives)))
           (rel (file-relative-name (directory-file-name dir) point)))
      (format "inside %s (read-only)%s" (abbreviate-file-name archive)
              (if (equal rel ".") "" (concat ": " rel))))))

(defun esploro--archive-mount (archive)
  "Open ARCHIVE through the core, waiting: its mount point, or nil (said why)."
  (pcase (esploro--call (list "archive" "open" archive) nil nil t)
    (`(:archive ,point ,_)
     (setf (alist-get archive esploro--archives nil nil #'equal) point)
     point)
    (answer (esploro--say answer "open the archive") nil)))

(defun esploro-open-archive (archive)
  "Go into ARCHIVE, opened read-only like a folder."
  (interactive (list (or (esploro--file-at) (user-error "No file here"))))
  (message "Esploro: opening %s..." (file-name-nondirectory archive))
  (esploro--close-stale-archives)
  (when-let* ((point (esploro--archive-mount (expand-file-name archive))))
    (esploro-go point)
    (message "Esploro: %s, read-only: copy files out, or Extract Here (Commands) to change them"
             (file-name-nondirectory archive))))

(defun esploro--close-stale-archives ()
  "Close the archives an earlier Emacs left open (it ended with them open)."
  (let ((open (esploro--call (list "archive" "list") nil nil t)))
    (when (and (consp open) (consp (car open)))
      (dolist (a open)
        (unless (rassoc (cdr a) esploro--archives)
          (esploro--call (list "archive" "close" (cdr a)) nil nil t))))))

(defun esploro--close-all-archives ()
  "Close every archive opened here: Emacs is ending."
  (dolist (a esploro--archives)
    (when (file-directory-p (cdr a))
      (ignore-errors (esploro--call (list "archive" "close" (cdr a)) nil nil t)))))

(add-hook 'kill-emacs-hook #'esploro--close-all-archives)

(defun esploro--close-archives-left ()
  "Close the archives no view shows any more (in the background)."
  (let ((dirs (delq nil (mapcar (lambda (b) (with-current-buffer b (and (derived-mode-p 'dired-mode) (esploro--dir b))))
                                (esploro--views)))))
    (dolist (a esploro--archives)
      (let ((point (file-name-as-directory (cdr a))))
        (when (and (file-directory-p point)
                   (not (seq-some (lambda (d) (string-prefix-p point (file-name-as-directory d))) dirs)))
          (esploro--call (list "archive" "close" (cdr a))))))))

;;; --- Open on a workspace -----------------------------------------------------------------

;; Right-click > Open on Workspace (or M-o, then its number): StumpWM goes to
;; that workspace and the file opens there: text in an Emacs frame there, a
;; folder in Esploro, the rest in its program.  The menu says what each
;; workspace is about (the project its windows are in).

(defun esploro--workspaces-menu (_items)
  "The workspaces, made as the menu opens: number, what it's about."
  (let ((files (and (esploro--view) (with-current-buffer (esploro--view) (esploro--selection))))
        (spaces (esploro--call (list "workspaces") nil nil t)))
    (if (not (and (consp spaces) (consp (car spaces))))
        (list ["StumpWM isn't answering" ignore :active nil])
      (mapcar (lambda (w)
                (pcase-let ((`(,number ,_name ,count ,folder ,current) w))
                  (vector (format "%d   %s%s" number
                                  (cond (folder folder)
                                        ((zerop count) "empty")
                                        (t (format "%d %s" count (if (= count 1) "window" "windows"))))
                                  (if current "   (this one)" ""))
                          (list 'esploro-open-on-workspace number (list 'quote files))
                          :active (and files t))))
              spaces))))

(defun esploro-open-on-workspace (number &optional files)
  "Go to workspace NUMBER, and open FILES (the selection) there."
  (interactive
   (list (let ((c (read-char "Open on workspace (1-9): ")))
           (if (and (>= c ?1) (<= c ?9)) (- c ?0) (user-error "A workspace is 1 to 9")))))
  (let ((files (or files (esploro--in-view (esploro--selection)) (user-error "Nothing selected"))))
    (esploro--call (append (list "open-on" (number-to-string number)) files) nil
                   (lambda (answer)
                     (pcase answer
                       (`(:opened ,n ,count) (message "Esploro: opened %d on workspace %d" count n))
                       (_ (esploro--say answer "open on a workspace")))))))

;;; --- Servers: a server's folders over SSH ---------------------------------------------------

;; Go > Connect to Server... (C-c k): user@host:folder, mounted by the core
;; with sshfs and shown like any folder (everything works there: copying
;; in and out, the Trash, undo).  A passphrase is asked with a dialog.
;; Servers stay connected until Go > Disconnect, or until Emacs ends: going
;; back would ask the passphrase again.  Down the side, under Servers, the
;; connected ones and the ones you've used (~/.ssh/config, known_hosts).

(defun esploro--remote-for-path (path)
  "The server (TARGET . POINT) PATH is on, or nil."
  (let ((path (directory-file-name (expand-file-name path))))
    (seq-find (lambda (r) (or (equal path (cdr r)) (string-prefix-p (file-name-as-directory (cdr r)) path)))
              esploro--remotes)))

(defun esploro--remote-heading (dir)
  "DIR's place on a server, for the header: \"on user@host:folder: docs\"."
  (when-let* ((remote (esploro--remote-for-path dir)))
    (let ((rel (file-relative-name (directory-file-name dir) (cdr remote))))
      (format "on %s%s" (string-remove-suffix ":" (car remote)) (if (equal rel ".") "" (concat ": " rel))))))

(defun esploro-connect (server)
  "Connect to SERVER (user@host:folder, or host for your home there), and
show it like a folder."
  (interactive (list (completing-read "Connect to (user@host:folder): "
                                      (let ((known (esploro--call (list "remote" "known") nil nil t)))
                                        (and (listp known) (seq-filter #'stringp known))))))
  (when (string-empty-p (string-trim server)) (user-error "Which server?"))
  (message "Esploro: connecting to %s (a dialog asks your passphrase if it's needed)..." server)
  (let ((frame (esploro--frame)))
    (esploro--call (list "remote" "open" server) nil
                   (lambda (answer)
                     (pcase answer
                       (`(:remote ,point ,_)
                        (let ((target (car (seq-find (lambda (r) (equal (cdr r) point))
                                                     (esploro--call (list "remote" "list") nil nil t)))))
                          (setf (alist-get (or target server) esploro--remotes nil nil #'equal) point))
                        (if frame (with-selected-frame frame (select-window (esploro--main-window frame)) (esploro-go point))
                          (esploro-go point))
                        (message "Esploro: connected to %s; Go > Disconnect when you're done" server))
                       (_ (esploro--say answer "connect")))))))

(defun esploro-disconnect (target)
  "Disconnect the server TARGET; a view on it goes home."
  (interactive (list (if (null esploro--remotes) (user-error "No server is connected")
                       (completing-read "Disconnect: " (mapcar #'car esploro--remotes) nil t))))
  (let ((point (cdr (assoc target esploro--remotes))))
    (dolist (b (esploro--views))
      (with-current-buffer b
        (when (and (derived-mode-p 'dired-mode) (esploro--remote-for-path (esploro--dir b)))
          (esploro-go "~"))))
    (pcase (esploro--call (list "remote" "close" point) nil nil t)
      (`(:closed ,_) (setq esploro--remotes (cl-remove target esploro--remotes :key #'car :test #'equal))
       (esploro-places-refresh)
       (message "Esploro: disconnected from %s" target))
      (`(:busy ,_) (message "Esploro: a program still has a file of %s open; close it, then disconnect" target))
      (answer (esploro--say answer "disconnect")))))

(defun esploro--disconnect-all ()
  "Disconnect every server: Emacs is ending."
  (dolist (r esploro--remotes)
    (ignore-errors (esploro--call (list "remote" "close" (cdr r)) nil nil t))))

(add-hook 'kill-emacs-hook #'esploro--disconnect-all)

(defvar esploro--known-servers nil "(TIME . SERVERS): asked of the core at TIME.")

(defun esploro--known-servers ()
  "The servers you've used, asked of the core at most once a minute (the
places are drawn on each move)."
  (unless (and esploro--known-servers (< (float-time (time-since (car esploro--known-servers))) 60))
    (let ((k (and (executable-find esploro-program)
                  (ignore-errors (esploro--call (list "remote" "known") nil nil t))))
          (connected (and (executable-find esploro-program)
                          (ignore-errors (esploro--call (list "remote" "list") nil nil t)))))
      ;; Servers still connected from before (an Emacs that ended without
      ;; disconnecting): known again here, to use or disconnect.
      (dolist (c (and (listp connected) connected))
        (when (and (consp c) (stringp (car c)) (stringp (cdr c)))
          (unless (assoc (car c) esploro--remotes)
            (push c esploro--remotes))))
      (setq esploro--known-servers (cons (current-time) (and (listp k) (seq-filter #'stringp k))))))
  (cdr esploro--known-servers))

(defun esploro--servers ()
  "Servers for the places: connected ones (NAME . FOLDER), then known ones
(NAME . (remote . NAME)) to connect to."
  (let* ((known (esploro--known-servers))   ; first: it learns of servers still connected
         (connected (mapcar (lambda (r) (cons (string-remove-suffix ":" (car r)) (cdr r))) esploro--remotes)))
    (append connected
            (mapcar (lambda (h) (cons h (cons 'remote h)))
                    (seq-remove (lambda (h) (seq-some (lambda (c) (string-match-p (regexp-quote h) (car c))) connected))
                                known)))))

;;; --- Commands in embark: on any file name in Emacs ----------------------------------

;; With embark (C-. on a file name: in a minibuffer, in dired, at point in
;; a buffer), , (comma) offers Esploro's commands for the file, and J shows it in
;; Esploro.  esploro-loaddefs.el sets this up before Esploro is loaded.

;;;###autoload
(defun esploro-file-commands (file)
  "Esploro's commands for FILE: pick one and it runs (one that changes
files, as a plan for your review)."
  (interactive (list (read-file-name "Esploro's commands for: " nil nil t)))
  (let* ((file (directory-file-name (expand-file-name file)))
         (answer (esploro--call (list "commands" file) nil nil t))
         (commands (and (consp answer) (not (keywordp (car answer))) answer)))
    (unless commands (user-error "Esploro has no commands for %s" (abbreviate-file-name file)))
    (let* ((choices (mapcar (lambda (c) (cons (concat (nth 1 c) (if (nth 3 c) "..." "")) c)) commands))
           (pick (completing-read (format "%s: " (file-name-nondirectory file))
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        `(metadata (annotation-function
                                                    . ,(lambda (label) (let ((doc (nth 2 (cdr (assoc label choices)))))
                                                                         (and doc (not (string-empty-p doc))
                                                                              (concat "  " (propertize doc 'face 'completions-annotations)))))))
                                      (complete-with-action action choices string pred)))
                                  nil t)))
      (esploro-run-command (car (cdr (assoc pick choices))) (list file)))))

;;;###autoload
(defun esploro-show-file (file)
  "FILE in Esploro, selected in its folder."
  (interactive (list (read-file-name "Show in Esploro: " nil nil t)))
  (let ((file (directory-file-name (expand-file-name file))))
    (esploro (file-name-directory file) nil file)))

(defun esploro--embark-setup ()
  (when (boundp 'embark-file-map)
    ;; Not X: where embark's map has no key, it falls back on the buffer's,
    ;; and dired's X runs the file as a shell command.
    (keymap-set embark-file-map "," #'esploro-file-commands)
    (keymap-set embark-file-map "J" #'esploro-show-file)))

(with-eval-after-load 'embark (esploro--embark-setup))

;;; --- Recipes: a change done again --------------------------------------------------------

;; The last change (moved into a folder, copied into one, put in the Trash),
;; done again on the selection; or kept by name and found under Recipes.
;; Each runs as a plan through the core: checked, journaled, undoable.

(defun esploro--recipe-run (name files what)
  "Do the recipe NAME on FILES: at once, or, for one whose plan you
review, shown for that."
  (esploro--call (append (list "recipe" "run" "--plan" name) files) nil
                 (lambda (answer)
                   (pcase answer
                     (`(:plan ,file ,why ,_n ,recipe) (esploro--review-open file why recipe (selected-frame)))
                     (`(:none ,why) (message "Esploro: nothing to do. %s" why))
                     (_ (esploro--say answer what) (esploro--refresh))))))

(defun esploro-repeat ()
  "Do the last change again, on the selection."
  (interactive)
  (let ((files (or (esploro--in-view (esploro--selection)) (user-error "Nothing selected"))))
    (pcase (esploro--call (list "recipe" "last") nil nil t)
      (`(:recipe ,_ ,description) (esploro--recipe-run "last" files description))
      (`(:none ,why) (message "Esploro: nothing to repeat: %s" why))
      (answer (esploro--say answer "repeat")))))

(defun esploro-save-recipe (name)
  "Keep the last change as a recipe called NAME, to do again from Recipes."
  (interactive (list (read-string "Keep the last change as: ")))
  (pcase (esploro--call (list "recipe" "save" name) nil nil t)
    (`(:saved ,n ,description) (message "Esploro: \"%s\": %s, under Recipes" n description))
    (`(:none ,why) (message "Esploro: can't keep it: %s" why))
    (answer (esploro--say answer "keep"))))

(defun esploro-forget-recipe (name)
  "Forget the recipe NAME."
  (interactive (list (completing-read "Forget the recipe: "
                                      (mapcar #'car (esploro--call (list "recipe" "list") nil nil t)) nil t)))
  (esploro--call (list "recipe" "forget" name) nil nil t)
  (message "Esploro: forgot \"%s\"" name))

(defun esploro--recipes-menu (_items)
  "Recipes, made as the menu opens: yours by name, renames by a pattern and
sorting by kind, and keeping the last change."
  (let ((files (and (esploro--view) (with-current-buffer (esploro--view) (esploro--selection))))
        (all (and (esploro--view) (with-current-buffer (esploro--view) (esploro--recipe-files))))
        (saved (esploro--call (list "recipe" "list") nil nil t)))
    (setq saved (and (listp saved) (not (keywordp (car saved))) saved))
    (append
     (mapcar (lambda (r)
               ;; One whose plan you review takes what's marked, else
               ;; everything here; the rest, what's selected.
               (let ((these (if (memq :review r) all files)))
                 (vector (format "%s (%s)%s" (car r) (cadr r) (if (memq :review r) "..." ""))
                         (list 'esploro--recipe-run (car r) (list 'quote these) (cadr r))
                         :active (and these t))))
             saved)
     (when saved (list "---"))
     (list ["Rename by Pattern..." esploro-rename-by-pattern]
           ["Sort into Folders by Kind..." esploro-sort-by-kind]
           "---"
           ["Save Last Change As..." esploro-save-recipe]
           (vector "Forget a Recipe..." 'esploro-forget-recipe :active (and saved t))))))

;; Renames by a pattern and sorting by kind send each file its own way:
;; their plan waits in the review panel, each file before and after,
;; until you choose Apply. They take what's marked, or, with nothing
;; marked, everything listed (a pattern picks its own files).

(defun esploro--recipe-files ()
  "The marked files, or, with none marked, every file listed here."
  (if (save-excursion (goto-char (point-min)) (re-search-forward (dired-marker-regexp) nil t))
      (esploro--selection)
    (save-excursion
      (goto-char (point-min))
      (let (files)
        (while (not (eobp))
          (let ((file (dired-get-filename nil t)))
            (when (and file (not (member (file-name-nondirectory (directory-file-name file)) '("." ".."))))
              (push file files)))
          (forward-line 1))
        (nreverse files)))))

(defun esploro--offer-plan (args what)
  "Ask the core for a plan (ARGS, which end in --plan's files), and show it
for review; WHAT says what it was, when there's none."
  (esploro--call args nil
                 (lambda (answer)
                   (pcase answer
                     (`(:plan ,file ,why ,_n ,recipe) (esploro--review-open file why recipe (selected-frame)))
                     (`(:none ,why) (message "Esploro: nothing to do. %s" why))
                     (_ (esploro--say answer what))))))

(defun esploro-rename-by-pattern (from to)
  "Rename by a pattern: the names FROM fits, as TO says.
In FROM, * stands for any run of characters and ? for any one; in TO,
#1, #2... give back what each took, #n numbers the files (01, 02...), and
## is a #.  FROM without * or ? is text to replace wherever a name holds
it.  On what's marked, or everything here.  The plan waits for your
review, each file before and after; two files to one name, or a name
already taken, and it's refused."
  (interactive
   (let* ((file (esploro--in-view (esploro--file-at)))
          (from (read-string "Rename the names like (* any run, ? any one; or text to replace): "
                             (and file (file-name-nondirectory file)))))
     (list from (read-string (format "Rename %s to (#1 #2: what * and ? took; #n: a number): " from)))))
  (let ((files (or (esploro--in-view (esploro--recipe-files)) (user-error "No files here"))))
    (esploro--offer-plan (append (list "rename-by" "--plan" from to) files) "rename")))

(defun esploro-sort-by-kind ()
  "Sort into folders by kind: pictures into Images, videos into Videos,
then Audio, Documents (PDFs), Text and Archives, each beside its file.
On what's marked, or everything here.  The plan waits for your review.
Your own kinds and folders are a recipe: see the manual."
  (interactive)
  (let ((files (or (esploro--in-view (esploro--recipe-files)) (user-error "No files here"))))
    (esploro--offer-plan (append (list "sort-by-kind" "--plan") files) "sort by kind")))

;;; --- Space: what's taking it ------------------------------------------------------------

;; View > What's Taking Space (C-c s): the folder's entries, hidden ones too,
;; by the space they take (du, on this drive), biggest first, each with its
;; size and a bar.  Biggest Anywhere Below (C-c S) is the same view of the
;; biggest things at any depth: a file, a folder of smaller things, or the
;; smaller things beside those in a folder, so no byte is shown twice.
;; Going into a folder keeps the view; the Trash, copying and undo work as
;; anywhere; F5 measures again.  The top line says what the folder takes,
;; what's marked would give back, what's free on the drive, and what the
;; Trash holds (emptying it is what frees the space).  A big folder is
;; measured in the background ("measuring..."), and leaving it stops the
;; measuring.

(defface esploro-space-bar '((t :inherit font-lock-constant-face))
  "The bar beside a size, in What's Taking Space.")

(defvar-local esploro--space-process nil "The core measuring this view now.")
(defvar-local esploro--space-sizes nil
  "In a space view: a table, each file shown (no slash at its end) to
(SHOWN . WHOLE), the bytes its row shows and all it takes.")
(defvar-local esploro--space-drive nil "(FREE TOTAL), in bytes, of the drive the view's folder is on.")
(defvar-local esploro--space-trash nil "What the Trash takes, in bytes, when it's on this drive.")
(defvar-local esploro--space-marked nil "(TICK COUNT BYTES): what's marked, as of the buffer's TICK.")
;; Found while the folder is measured, before the list is shown again.
(put 'esploro--space-drive 'permanent-local t)
(put 'esploro--space-trash 'permanent-local t)

(defun esploro-space-toggle ()
  "What's taking space in this folder, biggest first; or the folder as it was."
  (interactive)
  (esploro--in-view (esploro--space-set (not esploro--space))))

(defun esploro-space-below-toggle ()
  "The biggest things anywhere below this folder; or its own entries by
the space they take."
  (interactive)
  (esploro--in-view (esploro--space-set (if (eq esploro--space 'below) t 'below))))

(defun esploro--space-set (how)
  "The view as a space view: HOW is t (the folder's entries), `below' (the
biggest anywhere below it) or nil (the folder as it was)."
  (setq esploro--space how)
  (if how
      (esploro--space-measure)
    (esploro--space-stop)
    (setq esploro--space-sizes nil esploro--space-marked nil)
    (esploro--show (esploro--dir) nil (current-buffer))
    (dired-hide-details-mode -1)))

(defun esploro--space-stop ()
  (when (and (processp esploro--space-process) (process-live-p esploro--space-process))
    (delete-process esploro--space-process))
  (setq esploro--space-process nil))

(defun esploro--space-after-readin ()
  "A folder shown in a space view: measure it (a list made by the space view
itself is already measured)."
  (when (and esploro--space (not (consp dired-directory)))
    (esploro--space-measure)))

(defun esploro--space-measure ()
  "Ask the core what this folder's entries take, in the background."
  (esploro--space-stop)
  (let ((buffer (current-buffer)) (dir (esploro--dir)))
    (setq esploro--space-total 'measuring)
    (force-mode-line-update)
    (esploro--space-around dir)
    (let ((call
          (esploro--call (if (eq esploro--space 'below) (list "sizes" "--below" dir) (list "sizes" dir)) nil
                         (lambda (answer)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq esploro--space-process nil)
                               (when esploro--space
                                 (pcase answer
                                   ((and `(,(or :sizes :biggest) ,folder ,total ,entries)
                                         (guard (equal (file-name-as-directory folder) (file-name-as-directory dir))))
                                    (esploro--space-show
                                     folder total
                                     ;; Each as (FILE SHOWN WHOLE), FILE a whole path.
                                     (mapcar (lambda (entry)
                                               (if (consp (cdr entry))
                                                   (list (car entry) (nth 1 entry) (nth 2 entry))
                                                 (list (expand-file-name (car entry) folder) (cdr entry) (cdr entry))))
                                             entries)))
                                   (`(,(or :sizes :biggest) . ,_))   ; of a folder since left
                                   (_ (setq esploro--space-total nil) (esploro--say answer "measure"))))))))))
      ;; In the background, the process (to stop on leaving); waited for, its answer.
      (when (processp call) (setq esploro--space-process call)))))

(defun esploro--space-around (dir)
  "What's free on DIR's drive, now; and what the Trash takes, when it's on
that drive (asked in the background): emptying it is what gives space back."
  (setq esploro--space-drive
        (pcase (ignore-errors (file-system-info dir))
          (`(,total ,_free ,available) (list available total))))
  (setq esploro--space-trash nil)
  (let ((buffer (current-buffer))
        (trash (file-attributes (esploro--trash-dir)))
        (here (file-attributes dir)))
    (when (and trash here (equal (file-attribute-device-number trash) (file-attribute-device-number here)))
      (esploro--call (list "sizes" "--trash") nil
                     (lambda (answer)
                       (pcase answer
                         (`(:trash ,bytes ,_)
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (setq esploro--space-trash (and (> bytes 0) bytes))
                              (force-mode-line-update))))))))))

(defun esploro--outermost (files)
  "FILES without those inside another of them: a folder takes what's in it."
  (let ((kept '()))
    (dolist (file (sort (mapcar #'directory-file-name files) (lambda (a b) (< (length a) (length b)))))
      (unless (seq-some (lambda (k) (string-prefix-p (file-name-as-directory k) file)) kept)
        (push file kept)))
    (nreverse kept)))

(defun esploro--space-marked ()
  "(COUNT BYTES) of what's marked in this space view, or nil with nothing
marked: what trashing it and emptying the Trash would give back.  Looked
for again only when the marks changed."
  (when esploro--space-sizes
    (let ((tick (buffer-chars-modified-tick)))
      (unless (eql (car esploro--space-marked) tick)
        (let* ((marked (ignore-errors (dired-get-marked-files nil nil nil t)))
               ;; Nothing marked: the file at point alone; one mark: (t FILE).
               (files (cond ((eq (car marked) t) (cdr marked)) ((cdr marked) marked)))
               (outer (esploro--outermost files)))
          (setq esploro--space-marked
                (list tick (length outer)
                      (apply #'+ (mapcar (lambda (f) (or (cdr (gethash f esploro--space-sizes)) 0)) outer))))))
      (and (> (nth 1 esploro--space-marked) 0) (cdr esploro--space-marked)))))

(defun esploro--space-header ()
  "The top line of a space view."
  (let ((total esploro--space-total)
        (marked (esploro--space-marked)))
    ;; What matters most first: a narrow window cuts the line's end.
    (concat (format "Space: %s, %s" (abbreviate-file-name (esploro--dir))
                    (cond ((eq total 'measuring) "measuring...")
                          (total (concat (file-size-human-readable total) " in all"))
                          (t "")))
            (if (eq esploro--space 'below) ", the biggest anywhere below" ", biggest first")
            (pcase marked
              (`(,count ,bytes)
               (format "   marked: %d, %s%s" count (file-size-human-readable bytes)
                       (if (and (numberp total) (> total 0)) (format " (%d%%)" (round (* 100.0 bytes) total)) ""))))
            (pcase esploro--space-drive
              (`(,free ,all) (format "   free: %s of %s"
                                     (file-size-human-readable free) (file-size-human-readable all))))
            (when esploro--space-trash
              (format "   Trash: %s" (file-size-human-readable esploro--space-trash)))
            "   F5 measures again")))

(defun esploro--space-show (folder total entries)
  "FOLDER's ENTRIES ((FILE SHOWN WHOLE) ...), biggest first, each with the
size it shows and a bar; TOTAL in the top line."
  (let ((esploro--unsorted t)
        (space esploro--space)
        (dir (file-name-as-directory folder)))
    ;; Names as they are from the folder: one below it shows its way down.
    (esploro--show (cons dir (mapcar (lambda (e) (file-relative-name (car e) dir)) entries)) nil (current-buffer))
    (setq esploro--space space))
  (setq esploro--space-total total
        esploro--space-marked nil
        esploro--space-sizes (make-hash-table :test #'equal))
  (dolist (entry entries)
    (puthash (directory-file-name (car entry)) (cons (nth 1 entry) (nth 2 entry)) esploro--space-sizes))
  (setq-local revert-buffer-function (lambda (&rest _) (esploro--space-measure)))
  ;; Name, size, bar: the listing's own columns (permissions, date) hidden.
  (dired-hide-details-mode 1)
  (if (eq esploro--space 'below)
      (esploro--space-rows-below total (max 1 (or (nth 1 (car entries)) 1)))
    (esploro--space-rows total (max 1 (or (nth 1 (car entries)) 1))))
  (force-mode-line-update))

(defun esploro--space-rows-below (total biggest)
  "Size, bar and share before each name: the names here are ways down, of
any length, so the sizes line up on the left and a long name is cut by
the window's edge, not wrapped."
  (setq truncate-lines t)
  (let ((bar-room 10))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file (esploro--grid-file)))
          (when (and file (dired-move-to-filename))
            (let* ((sizes (gethash (directory-file-name file) esploro--space-sizes))
                   (bytes (or (car sizes) 0))
                   (bar (max (if (> bytes 0) 1 0) (round (* bar-room bytes) biggest)))
                   (o (make-overlay (point) (point))))
              (overlay-put o 'esploro-space t)
              (overlay-put o 'before-string
                           (concat (format "%6s " (file-size-human-readable bytes))
                                   (propertize (make-string bar ?█) 'face 'esploro-space-bar)
                                   (make-string (- bar-room bar) ?\s)
                                   (propertize (format "%3d%%  " (round (* 100.0 bytes) (max 1 total))) 'face 'shadow)))
              ;; A folder shown for less than it takes: the rest is in rows of their own.
              (when (and sizes (< (car sizes) (cdr sizes)) (dired-move-to-end-of-filename t))
                (let ((note (make-overlay (point) (point))))
                  (overlay-put note 'esploro-space t)
                  (overlay-put note 'after-string (propertize "  its smaller things" 'face 'shadow)))))))
        (forward-line 1)))))

(defun esploro--space-rows (total biggest)
  "Size, bar and share after each name, lined up past the longest."
  (let ((column 0)
        (bar-room 24))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (esploro--grid-file) (dired-move-to-end-of-filename t))
          (setq column (max column (current-column))))
        (forward-line 1))
      ;; The bars fit what's left of the window after the names and sizes,
      ;; so a narrow window (the preview open) doesn't wrap the lines.
      (let ((window (get-buffer-window (current-buffer))))
        (when window
          (setq bar-room (max 4 (min 24 (- (window-body-width window) column 2 9 6))))))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file (esploro--grid-file)))
          (when (and file (dired-move-to-end-of-filename t))
            (let* ((sizes (gethash (directory-file-name file) esploro--space-sizes))
                   (bytes (or (car sizes) 0))
                   (bar (max (if (> bytes 0) 1 0) (round (* bar-room bytes) biggest)))
                   (o (make-overlay (point) (point))))
              (overlay-put o 'esploro-space t)
              (overlay-put o 'after-string
                           (concat (propertize " " 'display `(space :align-to ,(+ column 2)))
                                   (format "%7s  " (file-size-human-readable bytes))
                                   (propertize (make-string bar ?█) 'face 'esploro-space-bar)
                                   (propertize (format " %d%%" (round (* 100.0 bytes) (max 1 total))) 'face 'shadow))))))
        (forward-line 1)))))

;;; --- Duplicates: files that are the same --------------------------------------------------

;; Edit > Find Duplicates: below this folder, the files that are the same,
;; byte for byte (1 KB or more; another git repository's left out), in
;; groups, the most space wasted first; each group's oldest is kept, the
;; others are copies.  Trash the Copies... proposes them for the Trash, as a
;; plan to review (and undo).

(defun esploro-find-duplicates ()
  "The files below this folder that are the same, byte for byte, in groups."
  (interactive)
  (esploro--in-view
   (let ((buffer (current-buffer)) (dir (esploro--dir)))
     (message "Esploro: looking for files that are the same below %s..." (abbreviate-file-name dir))
     (esploro--call (list "duplicates" dir) nil
                    (lambda (answer)
                      (pcase answer
                        (`(:duplicates ,folder ,groups)
                         (if (null groups)
                             (message "Esploro: no two files below %s are the same" (abbreviate-file-name folder))
                           (esploro--duplicates-show buffer folder groups)))
                        (_ (esploro--say answer "duplicates"))))))))

(defun esploro--duplicates-show (buffer folder groups &optional again)
  "GROUPS ((SIZE KEPT COPY ...) ...) below FOLDER, in BUFFER: each group
together, its kept one first."
  (with-current-buffer buffer
    (unless again
      (let ((here (and (derived-mode-p 'dired-mode) (expand-file-name default-directory))))
        (when here (push here esploro--back) (setq esploro--forward '()))))
    (let* ((root (file-name-as-directory folder))
           (files (mapcan (lambda (g) (copy-sequence (cdr g))) groups)))
      (let ((esploro--unsorted t))
        (esploro--show (cons root (mapcar (lambda (f) (file-relative-name f root)) files)) nil buffer))
      (setq esploro--duplicates (cons folder groups))
      (setq-local revert-buffer-function
                  (lambda (&rest _)
                    (esploro--call (list "duplicates" folder) nil
                                   (lambda (answer)
                                     (when (and (buffer-live-p buffer) (eq (car-safe answer) :duplicates))
                                       (if (nth 2 answer)
                                           (esploro--duplicates-show buffer folder (nth 2 answer) t)
                                         (message "Esploro: no copies left below %s" (abbreviate-file-name folder))))))))
      (rename-buffer "Esploro: Duplicates" t)
      ;; Each one says its group, and whether it's the one kept.
      (let ((n 0) (said (make-hash-table :test #'equal)))
        (dolist (g groups)
          (setq n (1+ n))
          (puthash (cadr g) (format "  %d · kept (the oldest)" n) said)
          (dolist (c (cddr g)) (puthash c (format "  %d · copy, %s" n (file-size-human-readable (car g))) said)))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((file (esploro--grid-file)) (text (and file (gethash file said))))
              (when (and text (dired-move-to-end-of-filename t))
                (let ((o (make-overlay (point) (point))))
                  (overlay-put o 'esploro-duplicate t)
                  (overlay-put o 'after-string
                               (propertize text 'face (if (string-match-p "kept" text) 'success 'warning))))))
            (forward-line 1))))
      (force-mode-line-update)
      (let ((copies (apply #'+ (mapcar (lambda (g) (length (cddr g))) groups)))
            (wasted (apply #'+ (mapcar (lambda (g) (* (car g) (length (cddr g)))) groups))))
        (message "Esploro: %d %s of %d %s, %s: Edit > Trash the Copies... to review a plan"
                 copies (if (= copies 1) "copy" "copies") (length groups) (if (= (length groups) 1) "file" "files")
                 (file-size-human-readable wasted))))))

(defun esploro-trash-duplicates ()
  "Propose the copies found for the Trash, as a plan to review."
  (interactive)
  (esploro--in-view
   (unless esploro--duplicates (user-error "Find Duplicates first (Edit menu)"))
   (esploro--call (list "duplicates" "--plan" (car esploro--duplicates)) nil
                  (lambda (answer)
                    (pcase answer
                      (`(:plan ,file ,why ,_n ,from) (esploro--review-open file why from (selected-frame)))
                      (`(:none ,why) (message "Esploro: nothing to do. %s" why))
                      (_ (esploro--say answer "duplicates")))))))

;;; --- Recent: the files opened lately ------------------------------------------------------

;; Esploro notes each file it opens; GTK's programs note theirs in
;; recently-used.xbel; Emacs's recentf, when it's on, has its own.  Recent
;; (at the top of the places, Go > Recent Files, C-c r) shows them all,
;; newest first, as a list to open, copy or drag from like any folder.

(defun esploro--recent-show (buffer files &optional again)
  "Show FILES, newest first, in BUFFER (a view); AGAIN when it's F5."
  (let* ((home (file-name-as-directory (expand-file-name "~")))
         (seen (make-hash-table :test #'equal))
         (files (seq-filter (lambda (f) (and (not (gethash f seen)) (puthash f t seen) (file-regular-p f)))
                            (append files (and (bound-and-true-p recentf-mode) (bound-and-true-p recentf-list)
                                               (seq-remove #'file-remote-p recentf-list))))))
    (if (null files)
        (message "Esploro: nothing opened lately yet (files you open from Esploro will show here)")
      (with-current-buffer buffer
        (unless again
          (let ((here (and (derived-mode-p 'dired-mode) (expand-file-name default-directory))))
            (when here (push here esploro--back) (setq esploro--forward '()))))
        (let ((esploro--unsorted t))
          (esploro--show (cons home (mapcar (lambda (f) (if (string-prefix-p home f) (file-relative-name f home) f)) files))
                         nil buffer))
        (setq esploro--recent t)
        (setq-local revert-buffer-function #'esploro--recent-again)
        (rename-buffer "Esploro: Recent" t)
        (esploro-places-refresh)))))

(defun esploro--recent-again (&rest _)
  "Look again at what was opened lately."
  (let ((buffer (current-buffer)))
    (esploro--call (list "recent") nil
                   (lambda (answer) (when (and (buffer-live-p buffer) (eq (car-safe answer) :recent))
                                      (esploro--recent-show buffer (cadr answer) t))))))

(defun esploro-recent ()
  "The files opened lately, newest first."
  (interactive)
  (esploro--in-view
   (let ((buffer (current-buffer)))
     (esploro--call (list "recent") nil
                    (lambda (answer)
                      (if (eq (car-safe answer) :recent)
                          (esploro--recent-show buffer (cadr answer))
                        (esploro--say answer "recent")))))))

(defun esploro--from-places-recent ()
  "Recent, in the pane used last of the frame whose places were clicked."
  (let ((frame (esploro--frame)))
    (when frame
      (with-selected-frame frame
        (select-window (esploro--main-window frame))
        (esploro-recent)))))

;;; --- Searches: folders that are questions ------------------------------------------

;; The core looks (`esploro query'), below a folder, for the files a few
;; words describe; the view shows them as one folder, and F5 looks
;; again.  Kept by name, a search is down the side under Searches.

(defun esploro--searches-file ()
  (expand-file-name "esploro/searches.lisp" (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun esploro--searches ()
  "Your searches, for the places: (NAME . (search . NAME))."
  (let ((file (esploro--searches-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let (forms form)
          (while (setq form (ignore-errors (read (current-buffer))))
            (when (and (eq (car-safe form) :search) (stringp (nth 1 form)))
              (push (cons (nth 1 form) (cons 'search (nth 1 form))) forms)))
          (nreverse forms))))))

(defun esploro--search-show (buffer answer &optional name again)
  "Show the core's ANSWER to a search in BUFFER, a view; AGAIN when it's F5,
so the view's history stays as it is."
  (pcase answer
    (`(:found ,root ,words ,paths ,more)
     (if (and (null paths) (not again))
         (message "Esploro: nothing below %s is %s" (abbreviate-file-name root) words)
       (with-current-buffer buffer
         (unless again
           (let ((here (and (derived-mode-p 'dired-mode) (expand-file-name default-directory))))
             (when here (push here esploro--back) (setq esploro--forward '()))))
         (esploro--show (cons (file-name-as-directory root) (mapcar (lambda (f) (file-relative-name f root)) paths)) nil buffer)
         (setq esploro--search (list words root name))
         (setq-local revert-buffer-function #'esploro--search-again)
         (rename-buffer (format "Esploro: %s" (or name words)) t))
       (unless again
         (message "Esploro: %d found%s" (length paths) (if more " (the first ones)" "")))))
    (_ (esploro--say answer "search"))))

(defun esploro--search-again (&rest _)
  "Look again, for the search this view shows."
  (let ((buffer (current-buffer)))
    (pcase-let ((`(,words ,root ,name) esploro--search))
      (esploro--call (list "query" words root) nil
                     (lambda (answer) (when (buffer-live-p buffer)
                                        (esploro--search-show buffer answer name t)))))))

(defun esploro-search (words)
  "The files below this folder that WORDS describe.
Words a name holds, *.pdf, kind:pdf (folder, image, video, audio, text,
archive), has:word (inside the file: text, and PDFs), newer:7 or older:30
(days), larger:10M, smaller:1k, -word for not."
  (interactive (list (read-string "Look below here for (words, *.pdf, kind:pdf, has:word, newer:7, larger:10M): ")))
  (esploro--in-view
   (let ((buffer (current-buffer)))
     (message "Esploro: looking...")
     (esploro--call (list "query" words (esploro--dir)) nil
                    (lambda (answer) (esploro--search-show buffer answer))))))

(defun esploro-run-search (name)
  "The search kept as NAME, in the pane used last."
  (interactive (list (completing-read "Search: " (mapcar #'car (esploro--searches)) nil t)))
  (let* ((frame (esploro--frame))
         (buffer (if frame (window-buffer (esploro--main-window frame)) (esploro--view))))
    (unless (esploro--view-p buffer) (user-error "No Esploro here (M-x esploro)"))
    (message "Esploro: looking...")
    (esploro--call (list "search" "run" name) nil
                   (lambda (answer) (esploro--search-show buffer answer name)
                     (esploro-places-refresh)))))

(defun esploro--searches-menu (_items)
  "Go > Searches: yours, and keeping or forgetting one."
  (let ((saved (esploro--searches)))
    (append
     (mapcar (lambda (s) (vector (car s) (list 'esploro-run-search (car s)))) saved)
     (when saved (list "---"))
     (list ["Search Below..." esploro-search :keys "M-s s"]
           (vector "Keep This Search..." 'esploro-save-search :active (and (esploro--value 'esploro--search) t))
           (vector "Forget a Search..." 'esploro-forget-search :active (and saved t))))))

(defun esploro-save-search (name)
  "Keep the search this view shows as NAME, down the side under Searches."
  (interactive (list (esploro--in-view
                      (unless esploro--search (user-error "This isn't a search (View > Search...)"))
                      (read-string "Keep this search as: " (or (nth 2 esploro--search) (car esploro--search))))))
  (esploro--in-view
   (pcase-let ((`(,words ,root ,_) esploro--search))
     (pcase (esploro--call (list "search" "save" name words root) nil nil t)
       (`(:saved . ,_)
        (setf (nth 2 esploro--search) name)
        (rename-buffer (format "Esploro: %s" name) t)
        (esploro-places-refresh)
        (message "Esploro: \"%s\" is under Searches" name))
       (answer (esploro--say answer "keep"))))))

(defun esploro-forget-search (name)
  "Forget the search NAME."
  (interactive (list (completing-read "Forget the search: " (mapcar #'car (esploro--searches)) nil t)))
  (esploro--call (list "search" "forget" name) nil nil t)
  (esploro-places-refresh)
  (message "Esploro: forgot \"%s\"" name))

;;; --- Closing a project: what's open in it ----------------------------------------------

;; Done with a project for now: what's open in it, unsaved first, to save
;; and close; and the other windows that have something in it, to go to.

(defvar-local esploro--project nil "The project this panel is about: its folder.")
(put 'esploro--project 'permanent-local t)

(defvar-keymap esploro-project-mode-map
  :doc "Closing a project."
  :parent special-mode-map
  "g" #'esploro-project-refresh
  "S" #'esploro-project-save-all
  "C" #'esploro-project-close-all
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(esploro--install-menu-bar esploro-project-mode-map)

(define-derived-mode esploro-project-mode special-mode "Project"
  "What's open in a project, to save and close."
  (setq-local esploro--menus-only t)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (visual-line-mode 1))

(defun esploro--in-folder-p (file root)
  (and file (string-prefix-p (file-name-as-directory root) (file-name-as-directory (expand-file-name file)))))

(defun esploro--project-buffers (root)
  "Emacs's buffers in ROOT: files, dired, and shells or other programs
working there; not Esploro's own views."
  (seq-filter (lambda (b)
                (with-current-buffer b
                  (and (not (string-prefix-p " " (buffer-name)))
                       (not esploro--view)
                       (if buffer-file-name (esploro--in-folder-p buffer-file-name root)
                         (and (or (derived-mode-p 'dired-mode) (get-buffer-process b)
                                  (derived-mode-p 'magit-mode))
                              (esploro--in-folder-p default-directory root))))))
              (buffer-list)))

(defun esploro--unsaved-p (buffer)
  (and (buffer-file-name buffer) (buffer-modified-p buffer)))

(defun esploro--project-close-buffer (buffer)
  "Close BUFFER; unsaved, only when you say its changes may go."
  (when (buffer-live-p buffer)
    (if (esploro--unsaved-p buffer)
        (when (yes-or-no-p (format "%s has changes not saved.  Close it, losing them? " (buffer-name buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
      (kill-buffer buffer))))

(defun esploro-close-project (&optional dir)
  "What's open in DIR's project (this folder's), to save and close."
  (interactive)
  (let* ((dir (or dir (esploro--dir) default-directory))
         (answer (esploro--call (list "project" (expand-file-name dir)) nil nil t)))
    (pcase answer
      (`(:project ,root ,_)
       (let ((buffer (get-buffer-create (format "*Esploro: %s*" (abbreviate-file-name root)))))
         (with-current-buffer buffer
           (esploro-project-mode)
           (setq esploro--project root)
           (esploro--project-show (nth 2 answer)))
         (pop-to-buffer buffer)))
      (`(:none ,why) (message "Esploro: %s" why))
      (_ (esploro--say answer "project")))))

(defun esploro-project-refresh ()
  "Look again at what's open in the project."
  (interactive)
  (let ((answer (esploro--call (list "project" esploro--project) nil nil t)))
    (esploro--project-show (and (eq (car-safe answer) :project) (nth 2 answer)))))

(defun esploro--project-button (label action)
  (insert-text-button label 'action (lambda (_) (funcall action) (esploro-project-refresh)) 'follow-link t)
  (insert " "))

(defun esploro--project-show (windows)
  "The panel, from Emacs's buffers and the core's WINDOWS."
  (let* ((inhibit-read-only t)
         (root esploro--project)
         (buffers (esploro--project-buffers root))
         (unsaved (seq-filter #'esploro--unsaved-p buffers))
         (rest (seq-remove #'esploro--unsaved-p buffers))
         (line (point)))
    (erase-buffer)
    (insert (propertize (format "Closing %s" (abbreviate-file-name root)) 'face 'bold) "\n\n")
    (cl-flet ((name (b) (if (buffer-file-name b) (file-relative-name (buffer-file-name b) root) (buffer-name b))))
      (when unsaved
        (insert (propertize "Not saved" 'face 'warning) "\n")
        (dolist (b unsaved)
          (insert "  ")
          (esploro--project-button "Save" (lambda () (with-current-buffer b (save-buffer))))
          (esploro--project-button "Close" (lambda () (esploro--project-close-buffer b)))
          (insert (name b) "\n"))
        (insert "\n"))
      (when rest
        (insert (propertize "Open in Emacs" 'face 'bold) "\n")
        (dolist (b rest)
          (insert "  ")
          (esploro--project-button "Close" (lambda () (esploro--project-close-buffer b)))
          (insert (name b) "\n"))
        (insert "\n")))
    (when windows
      (insert (propertize "In other windows" 'face 'bold) "\n")
      (dolist (w windows)
        (pcase-let ((`(,id ,class ,group ,title ,paths) w))
          (insert "  ")
          (esploro--project-button "Go" (lambda () (esploro--call (list "focus" (number-to-string id)))))
          (insert (format "%s%s%s: %s\n" class (if group (format " on %s" group) "")
                          (if (and title (not (string-empty-p title))) (format " (%s)" title) "")
                          (mapconcat (lambda (p) (let ((r (file-relative-name p root))) (if (equal r ".") "the project" r)))
                                     paths ", ")))))
      (insert "\n"))
    (if (or buffers windows)
        (progn
          (when unsaved (esploro--project-button "Save All" #'esploro-project-save-all))
          (when buffers (esploro--project-button "Close All" #'esploro-project-close-all))
          (insert "\n\n" (propertize "Close All closes what's saved; what isn't stays, to save or close on its own.  Other windows are yours to close.  g looks again." 'face 'shadow) "\n"))
      (insert "Nothing open in it.\n"))
    (goto-char (min line (point-max)))))

(defun esploro-project-save-all ()
  "Save every unsaved file of the project."
  (interactive)
  (dolist (b (seq-filter #'esploro--unsaved-p (esploro--project-buffers esploro--project)))
    (with-current-buffer b (save-buffer)))
  (when (called-interactively-p 'any) (esploro-project-refresh)))

(defun esploro-project-close-all ()
  "Close the project's buffers that are saved; the others stay."
  (interactive)
  (let* ((buffers (esploro--project-buffers esploro--project))
         (unsaved (seq-filter #'esploro--unsaved-p buffers)))
    (mapc #'kill-buffer (seq-remove #'esploro--unsaved-p buffers))
    (message "Esploro: closed %d%s" (- (length buffers) (length unsaved))
             (if unsaved (format "; %d not saved, still open" (length unsaved)) "")))
  (when (called-interactively-p 'any) (esploro-project-refresh)))

;;; --- Habits: what you keep doing, offered back ---------------------------------------

;; When several of your plans have moved like files into one folder, the
;; core says what they have in common; Edit > Habits Noticed shows each,
;; to keep as a rule for agents, as a recipe, or to find more like them.
;; Nothing happens on its own; Not This waves one away for good.

(defvar esploro-habits-nudge t
  "Non-nil: after a move, say so when Esploro notices a new habit.")

(defun esploro--habits-nudge ()
  "Ask, in the background, whether there's a habit not told of yet; say it."
  (when esploro-habits-nudge
    (esploro--call (list "habits" "--new") nil
                   (lambda (answer)
                     (when (and (consp answer) (consp (car answer)))
                       (run-at-time 1.5 nil #'message "Esploro noticed: %s  (Edit > Habits Noticed...)"
                                    (nth 1 (car answer))))))))

(defvar-keymap esploro-habits-mode-map
  :doc "Habits Esploro noticed."
  :parent special-mode-map
  "g" #'esploro-habits-refresh
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(esploro--install-menu-bar esploro-habits-mode-map)

(define-derived-mode esploro-habits-mode special-mode "Habits"
  "What you keep doing with files, to keep as a rule, a recipe, or a search."
  (setq-local esploro--menus-only t)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (visual-line-mode 1))

(defun esploro-habits ()
  "What you keep doing with files, noticed from your plans."
  (interactive)
  (let ((buffer (get-buffer-create "*Esploro: habits*")))
    (with-current-buffer buffer
      (esploro-habits-mode)
      (esploro-habits-refresh))
    (pop-to-buffer buffer)))

(defun esploro--habits-button (label action help)
  (insert-text-button label 'action (lambda (_) (funcall action)) 'follow-link t
                      'face 'esploro-button 'help-echo help)
  (insert "  "))

(defun esploro-habits-refresh ()
  "Look again at what you keep doing."
  (interactive)
  (let ((habits (esploro--call (list "habits") nil nil t))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Habits noticed" 'face 'bold) "\n"
            (propertize "Like files you keep moving into one folder.  Keep one as a rule for agents (your sorting.md), as a recipe (Edit > Recipes), or find more like it; Not This and it's never offered again." 'face 'shadow)
            "\n\n")
    (if (not (and (consp habits) (consp (car habits))))
        (insert "Nothing yet: a habit is three or more like files, moved into one folder by two or more of your plans.\n")
      (dolist (h habits)
        (pcase-let ((`(,key ,said ,rule ,recipe ,words ,from) h))
          (insert said "\n  ")
          (esploro--habits-button " Keep as Rule... "
                                  (lambda () (esploro--habit-rule rule))
                                  "Add it to your rules for where files go, which agents read")
          (esploro--habits-button " Keep as Recipe... "
                                  (lambda () (esploro--habit-recipe recipe))
                                  "Move what's selected there, from Edit > Recipes")
          (when from
            (esploro--habits-button " Find More "
                                    (lambda () (esploro--habit-find words from))
                                    (format "Search %s for %s" (abbreviate-file-name from) words)))
          (esploro--habits-button " Not This "
                                  (lambda () (esploro--call (list "habits" "--dismiss" key) nil nil t)
                                    (esploro-habits-refresh))
                                  "Never offer this one again")
          (insert "\n\n"))))
    (goto-char (point-min))))

(defun esploro--habit-rule (rule)
  (let ((text (string-trim (read-string "Rule for agents (make it say what you mean): " rule))))
    (unless (string-empty-p text)
      (pcase (esploro--call (list "learn" "--add" text) nil nil t)
        (`(:added ,_) (message "Esploro: added to ~/.config/esploro/sorting.md"))
        (answer (esploro--say answer "rule"))))))

(defun esploro--habit-recipe (recipe)
  (let ((name (read-string "Keep as the recipe: "
                           (format "Into %s" (file-name-nondirectory (directory-file-name (cadr recipe)))))))
    (unless (string-empty-p name)
      (pcase (esploro--call (list "recipe" "add" name (prin1-to-string recipe)) nil nil t)
        (`(:saved ,n ,description) (message "Esploro: \"%s\": %s, under Edit > Recipes" n description))
        (answer (esploro--say answer "keep"))))))

(defun esploro--habit-find (words from)
  "A search of FROM for WORDS, in the Esploro window."
  (let* ((frame (esploro--frame))
         (buffer (if frame (window-buffer (esploro--main-window frame)) (esploro--view))))
    (unless (esploro--view-p buffer) (user-error "No Esploro here (M-x esploro)"))
    (when frame (select-frame-set-input-focus frame))
    (esploro--call (list "query" words from) nil
                   (lambda (answer) (esploro--search-show buffer answer)))))

;;; --- Changes: the journal, to read and undo from ------------------------------------------

;; Edit > Changes... (C-c u): every change Esploro made, newest first, in
;; words, each with Undo, any one of them and not only the last (checked
;; whole first: when its files have moved on since, nothing changes and it
;; says why).  RET or TAB on one shows its steps; Show goes to its folder.

(defvar-keymap esploro-changes-mode-map
  :doc "Esploro's changes."
  :parent special-mode-map
  "g" #'esploro-changes-refresh
  "RET" #'esploro-changes-toggle
  "TAB" #'esploro-changes-toggle
  "n" #'next-line
  "p" #'previous-line)

(esploro--install-menu-bar esploro-changes-mode-map)

(define-derived-mode esploro-changes-mode special-mode "Changes"
  "Every change Esploro made, to read and undo."
  (setq-local esploro--menus-only t)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (setq-local truncate-lines t))

(defvar-local esploro--changes-open nil "The changes whose steps are shown: their ids.")

(defun esploro-changes ()
  "Every change Esploro made, newest first, to read and undo any one."
  (interactive)
  (let ((buffer (get-buffer-create "*Esploro: changes*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'esploro-changes-mode) (esploro-changes-mode))
      (esploro-changes-refresh))
    (pop-to-buffer buffer)))

(defun esploro--changes-when (time)
  "\"2026-10-04T06:15:59\" as \"today 06:15\", \"yesterday 23:53\", or a date."
  (let* ((day (substring time 0 10))
         (clock (substring time 11 16))
         (today (format-time-string "%Y-%m-%d"))
         (yesterday (format-time-string "%Y-%m-%d" (time-subtract nil (* 24 3600)))))
    (cond ((equal day today) (concat "today " clock))
          ((equal day yesterday) (concat "yesterday " clock))
          (t (concat day " " clock)))))

(defun esploro-changes-refresh ()
  "Look again at the changes."
  (interactive)
  (let ((changes (esploro--call (list "changes") nil nil t))
        (inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (insert (propertize "Changes" 'face 'bold) "\n"
            (propertize "Everything Esploro changed in your files, newest first.\nUndo takes any one back, when its files are still as it left them.\nRET shows a change's steps." 'face 'shadow)
            "\n\n")
    (if (not (and (consp changes) (consp (car changes))))
        (insert "No changes yet.\n")
      (dolist (c changes)
        (pcase-let ((`(,id ,time ,summary ,undone ,steps ,folder) c))
          (let ((start (point)))
            (insert (format "%-17s " (esploro--changes-when time)))
            ;; The buttons before the words, so a long one doesn't hide them.
            (if undone
                (insert (propertize "undone" 'face 'shadow))
              (insert-text-button "Undo" 'action (lambda (_) (esploro-changes-undo id summary))
                                  'follow-link t 'face 'esploro-button 'help-echo "Take this change back"))
            (insert " ")
            (if (and folder (file-directory-p folder))
                (insert-text-button "Show" 'action (lambda (_) (esploro--from-places folder))
                                    'follow-link t 'help-echo (abbreviate-file-name folder))
              (insert "    "))
            (insert "  " (if undone (propertize summary 'face 'shadow) summary) "\n")
            (put-text-property start (point) 'esploro-change id)
            (when (member id esploro--changes-open)
              (dolist (s steps)
                (insert (propertize (concat "                    " s "\n") 'face 'shadow 'esploro-change id))))))))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun esploro-changes-toggle ()
  "Show the steps of the change on this line, or hide them."
  (interactive)
  (let ((id (get-text-property (point) 'esploro-change)))
    (when id
      (setq esploro--changes-open (if (member id esploro--changes-open) (delete id esploro--changes-open)
                                    (cons id esploro--changes-open)))
      (esploro-changes-refresh))))

(defun esploro-changes-undo (id summary)
  "Take back the change ID (SUMMARY says what it was)."
  (when (y-or-n-p (format "Undo \"%s\"? " summary))
    (pcase (esploro--call (list "changes" "undo" id) nil nil t)
      (`(:undone ,steps) (message "Esploro: undone: %s" (string-join steps "; ")) (esploro--refresh))
      (`(:refused ,problems)
       (message "Esploro: can't undo it, its files have moved on since (nothing changed): %s" (string-join problems "; ")))
      (answer (esploro--say answer "undo")))
    (when (derived-mode-p 'esploro-changes-mode) (esploro-changes-refresh))))

;;; --- Plans to review: an agent's proposals ---------------------------------------------

;; An agent never changes files itself: it proposes a plan (vikix mcp's
;; propose_file_changes, which runs esploro propose), the core checks it,
;; and it waits here for you. Apply runs it through the core, journaled,
;; so undo takes it back; Cancel drops it.

(defun esploro--describe-step (step)
  (let ((short #'abbreviate-file-name))
    (pcase step
      (`(:copy ,a ,b) (format "copy %s to %s" (funcall short a) (funcall short b)))
      (`(:move ,a ,b) (format "move %s to %s" (funcall short a) (funcall short b)))
      (`(:rename ,a ,b) (format "rename %s to %s" (funcall short a) b))
      (`(:mkdir ,a) (format "make the folder %s" (funcall short a)))
      (`(:trash ,a) (format "put %s in the Trash" (funcall short a)))
      (_ (format "%S" step)))))

(defun esploro--read-plan (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let (steps form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (push form steps))
      (nreverse steps))))

;; Edit opens the plan's own file (the core keeps a copy, one step a
;; line); saving it checks it again and shows it here as it now is.

(defvar-local esploro--review-file nil "The plan under review: its file.")
(defvar-local esploro--review-why nil "Why the agent proposes it.")
(defvar-local esploro--review-recipe nil
  "When the plan is a recipe's (a rename by a pattern, a sorting): the recipe,
which the review offers to keep by name.")
(defvar-local esploro--review-problems nil
  "What the core found wrong with the plan as it now is; nil when it can be applied.")
(defvar-local esploro--review-edited nil "Whether you've changed the plan.")
(defvar-local esploro--review-proposed nil
  "A copy of the agent's plan as it came, to learn from what you changed.")
(defvar-local esploro--review-of nil "In a plan being edited: its review buffer.")

(defvar-keymap esploro-review-mode-map
  :doc "A plan to review: Esploro's menus."
  :parent special-mode-map
  "e" #'esploro-review-edit
  "k" #'esploro-review-keep-recipe
  "C-x C-c" #'esploro-close)
(esploro--install-menu-bar esploro-review-mode-map)

(defface esploro-button '((t :box (:line-width 2 :style released-button) :weight bold :inherit default))
  "Apply, Edit and Cancel, under a plan to review.")

(define-derived-mode esploro-review-mode special-mode "Plan"
  "A plan proposed to you: Apply, Edit or Cancel."
  (setq-local esploro--menus-only t)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (setq-local revert-buffer-function (lambda (&rest _) (esploro--review-show (current-buffer))))
  (visual-line-mode 1))

(defun esploro--review-show (buffer)
  "Show in BUFFER the plan in its file, as it now is."
  (with-current-buffer buffer
    (let ((steps (esploro--read-plan esploro--review-file))
          (inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "A plan for you to review" 'face 'bold)
              (if esploro--review-edited (propertize "  (edited by you)" 'face 'shadow) "")
              "\n")
      (insert (propertize (if esploro--review-recipe
                              "Nothing happens until you apply it; undo takes it back after.\n"
                            "An agent proposes these changes. Nothing happens until you apply them; undo takes them back after.\n")
                          'face 'shadow))
      (when (and esploro--review-why (not (string-empty-p esploro--review-why)))
        (insert "\n" (if esploro--review-recipe "" "Why: ") esploro--review-why "\n"))
      (insert "\n")
      (esploro--review-insert-steps steps)
      (when esploro--review-problems
        (insert "\n" (propertize "It can't be applied as it is:" 'face 'error) "\n")
        (dolist (problem esploro--review-problems)
          (insert "  " problem "\n")))
      (insert "\n")
      (insert-text-button " Apply " 'action (lambda (_) (esploro--review-done buffer t))
                          'follow-link t 'face 'esploro-button 'help-echo "Make these changes (undo takes them back)")
      (insert "   ")
      (insert-text-button " Edit " 'action (lambda (_) (with-current-buffer buffer (esploro-review-edit)))
                          'follow-link t 'face 'esploro-button 'help-echo "Change the plan as text, one step a line (e)")
      (insert "   ")
      (insert-text-button " Cancel " 'action (lambda (_) (esploro--review-done buffer nil))
                          'follow-link t 'face 'esploro-button 'help-echo "Drop the plan: nothing changes")
      (when (consp esploro--review-recipe)
        (insert "   ")
        (insert-text-button " Keep as Recipe... " 'action (lambda (_) (with-current-buffer buffer (call-interactively #'esploro-review-keep-recipe)))
                            'follow-link t 'face 'esploro-button
                            'help-echo "Keep this by name, to do again from Recipes (k)"))
      (insert "\n")
      (goto-char (point-min)))))

(defun esploro-review-plan (file &optional why where recipe)
  "Show the plan in FILE (an agent's, or a recipe's, checked by the core)
in Esploro, on your workspace (WHERE, as `esploro' takes it), with Apply,
Edit and Cancel; and when it's RECIPE's, Keep as Recipe."
  (let* ((steps (esploro--read-plan file))
         (first-path (cadr (car steps)))
         (dir (if (and first-path (file-directory-p (file-name-directory first-path)))
                  (file-name-directory first-path)
                "~")))
    (esploro--review-open file why recipe (esploro dir where))))

(defun esploro--review-open (file why recipe frame)
  "The review of the plan in FILE, beside the folder in FRAME (which stays
where it is): WHY says what it does, RECIPE what made it, if one did."
  (let ((steps (esploro--read-plan file))
        (buffer (generate-new-buffer "*Esploro: a plan to review*")))
    (with-current-buffer buffer
      (esploro-review-mode)
      (setq esploro--review-file file
            esploro--review-why why
            esploro--review-recipe recipe)
      (unless recipe (esploro--review-keep-proposed)))
    (esploro--review-show buffer)
    (with-selected-frame frame
      (select-window (display-buffer-in-side-window
                      buffer '((side . bottom) (slot . 0) (window-height . 0.35)
                               (window-parameters (no-delete-other-windows . t))))))
    (message "Esploro: a plan of %d %s to review" (length steps) (if (= (length steps) 1) "step" "steps"))
    buffer))

(defun esploro--review-keep-proposed ()
  "Keep the agent's plan under review as it came, so what you change in it
can become a rule for the next one."
  (let ((proposed (concat esploro--review-file ".proposed")))
    (when (ignore-errors (copy-file esploro--review-file proposed t) t)
      (setq esploro--review-proposed proposed))))

(defun esploro--review-insert-steps (steps)
  "STEPS, numbered.  Renames all in one folder are a table, each name
before and after, under the folder's name."
  (let* ((renames (and steps (seq-every-p (lambda (s) (eq (car s) :rename)) steps)))
         (folders (and renames (seq-uniq (mapcar (lambda (s) (file-name-directory (cadr s))) steps))))
         (n 0))
    (if (not (and renames (= (length folders) 1)))
        (dolist (step steps)
          (insert (format "%d. %s\n" (setq n (1+ n)) (esploro--describe-step step))))
      (insert (format "In %s, %d %s:\n" (abbreviate-file-name (car folders)) (length steps)
                      (if (= (length steps) 1) "rename" "renames")))
      (let ((width (apply #'max (mapcar (lambda (s) (string-width (file-name-nondirectory (cadr s)))) steps))))
        (dolist (step steps)
          (let ((old (file-name-nondirectory (cadr step))))
            (insert (format "%3d. %s%s  →  %s\n" (setq n (1+ n)) old
                            (make-string (- width (string-width old)) ?\s) (caddr step)))))))))

(defun esploro-review-keep-recipe (name)
  "Keep the recipe that made the plan under review by NAME, to do again
from Recipes."
  (interactive (list (if esploro--review-recipe (read-string "Keep this recipe as: ")
                       (user-error "This plan isn't a recipe's"))))
  (pcase (esploro--call (list "recipe" "add" name (prin1-to-string esploro--review-recipe)) nil nil t)
    (`(:saved ,n ,description) (message "Esploro: \"%s\": %s, under Recipes" n description))
    (answer (esploro--say answer "keep"))))

(defun esploro--review-edit-buffer (review)
  "The buffer editing REVIEW's plan, if there is one."
  (seq-find (lambda (b) (eq (buffer-local-value 'esploro--review-of b) review))
            (buffer-list)))

(defvar-keymap esploro-plan-edit-mode-map
  :doc "Editing a plan to review."
  "C-c C-c" #'esploro-plan-edit-done
  "C-c C-k" #'esploro-plan-edit-abort)

(define-minor-mode esploro-plan-edit-mode
  "A plan under review, as text: one step a line.
\\<esploro-plan-edit-mode-map>\\[esploro-plan-edit-done] saves it and goes back to the review, \
\\[esploro-plan-edit-abort] drops your changes."
  :lighter " Plan"
  (if esploro-plan-edit-mode
      (progn
        (add-hook 'after-save-hook #'esploro--plan-edit-saved nil t)
        (setq header-line-format
              (substitute-command-keys
               "  One step a line.  \\<esploro-plan-edit-mode-map>\\[esploro-plan-edit-done]: done, back to the review   \\[esploro-plan-edit-abort]: drop your changes")))
    (remove-hook 'after-save-hook #'esploro--plan-edit-saved t)
    (setq header-line-format nil)))

(defun esploro-review-edit ()
  "Edit the plan under review as text, one step a line.
Saving checks it again and shows it in the review as it now is."
  (interactive)
  (let* ((review (current-buffer))
         (edit (or (esploro--review-edit-buffer review)
                   (let ((b (find-file-noselect esploro--review-file)))
                     (with-current-buffer b
                       (unless (derived-mode-p 'lisp-data-mode) (lisp-data-mode))
                       (setq esploro--review-of review)
                       (esploro-plan-edit-mode 1))
                     b))))
    (pop-to-buffer edit '((display-buffer-reuse-window display-buffer-use-some-window)))))

(defun esploro--review-check (review)
  "Ask the core whether REVIEW's plan can be applied, and show it again."
  (with-current-buffer review
    (let ((answer (esploro--call (list "check" esploro--review-file) nil nil t)))
      (setq esploro--review-problems
            (pcase answer
              (`(:ok ,_) nil)
              (`(:refused ,problems) problems)
              (`(:error ,text) (list text))
              (_ nil)))))
  (esploro--review-show review))

(defun esploro--plan-edit-saved ()
  (let ((review esploro--review-of))
    (when (buffer-live-p review)
      (with-current-buffer review (setq esploro--review-edited t))
      (esploro--review-check review)
      (message (if (buffer-local-value 'esploro--review-problems review)
                   "Esploro: saved, but the plan can't be applied as it is (see the review)"
                 "Esploro: saved; the review shows the plan as it now is")))))

(defun esploro--plan-edit-close (edit review)
  (let ((window (get-buffer-window edit)))
    (with-current-buffer edit (set-buffer-modified-p nil))
    (kill-buffer edit)
    (when (and (window-live-p window) (not (eq window (get-buffer-window review))))
      (ignore-errors (delete-window window))))
  (when-let* ((window (and (buffer-live-p review) (get-buffer-window review t))))
    (select-window window)))

(defun esploro-plan-edit-done ()
  "Save the plan, and go back to its review."
  (interactive)
  (let ((review esploro--review-of))
    (save-buffer)
    (esploro--plan-edit-close (current-buffer) review)))

(defun esploro-plan-edit-abort ()
  "Drop your unsaved changes to the plan, and go back to its review."
  (interactive)
  (esploro--plan-edit-close (current-buffer) esploro--review-of))

(defun esploro--review-done (buffer apply)
  "Apply BUFFER's plan (APPLY), or drop it; either way the review goes."
  (let ((edit (esploro--review-edit-buffer buffer))
        (file (buffer-local-value 'esploro--review-file buffer)))
    (if (and apply edit (buffer-modified-p edit)
             (not (y-or-n-p "You've changed the plan without saving it: save, and apply that? ")))
        (message "Esploro: not applied; the plan is still open for editing")
      (when (and apply edit (buffer-modified-p edit))
        (with-current-buffer edit (save-buffer)))
      (when apply (esploro--review-check buffer))
      (if (and apply (buffer-local-value 'esploro--review-problems buffer))
          (message "Esploro: not applied, the plan can't be applied as it is: %s"
                   (string-join (buffer-local-value 'esploro--review-problems buffer) "; "))
        (let ((steps (esploro--read-plan file))
              (proposed (buffer-local-value 'esploro--review-proposed buffer))
              (edited (buffer-local-value 'esploro--review-edited buffer)))
          (if apply
              (esploro--review-apply steps file proposed edited)
            (message "Esploro: the proposed plan was dropped; nothing changed")
            (ignore-errors (delete-file file))
            (when proposed (ignore-errors (delete-file proposed))))
          (when edit (esploro--plan-edit-close edit buffer))
          (when-let* ((window (get-buffer-window buffer t))) (delete-window window))
          (kill-buffer buffer))))))

(defun esploro--review-apply (steps file proposed edited)
  "Apply STEPS, the plan in FILE; when you EDITED it, offer what you changed
from PROPOSED (the agent's) as rules for agents.  The files go after."
  (message "Esploro: the proposed plan, applying...")
  (let ((esploro--progress (esploro--progress-reporter "the proposed plan")))
    (esploro--call (list "apply") (esploro--plan-text steps)
                   (lambda (answer)
                     (esploro--say answer "the proposed plan, applied")
                     (esploro--refresh)
                     (let ((learn (and edited proposed (eq (car-safe answer) :done)
                                       (esploro--call (list "learn" proposed file) nil nil t))))
                       (ignore-errors (delete-file file))
                       (when proposed (ignore-errors (delete-file proposed)))
                       (pcase learn
			 (`(:corrections ,rules)
                          (when rules
                            ;; Asked from Emacs's command loop, not from the
                            ;; core's answer coming in.
                            (if esploro--wait (esploro--offer-rules rules)
                              (run-at-time 0 nil #'esploro--offer-rules rules))))))))))

(defvar esploro-learn-ask t
  "Non-nil: after you apply an agent's plan you edited, offer what you
changed as rules in ~/.config/esploro/sorting.md.")

(defun esploro--offer-rules (rules)
  "Offer RULES, what you changed in an agent's plan in words, one by one:
each shown first, to edit into something general, and written only on
your yes."
  (when (and esploro-learn-ask
             (y-or-n-p (format "You changed %d of the agent's steps.  Keep %s as %s for agents (sorting.md)? "
                               (length rules) (if (cdr rules) "them" "it") (if (cdr rules) "rules" "a rule"))))
    (let ((added 0))
      (dolist (rule rules)
        (let ((text (string-trim (read-string "Rule (make it general; empty skips it): " rule))))
          (unless (string-empty-p text)
            (when (eq (car-safe (esploro--call (list "learn" "--add" text) nil nil t)) :added)
              (setq added (1+ added))))))
      (message "Esploro: %d %s added to ~/.config/esploro/sorting.md" added (if (= added 1) "rule" "rules")))))

(defun esploro-sorting-rules ()
  "Open your rules for where files go: what agents read before proposing."
  (interactive)
  (find-file (expand-file-name "esploro/sorting.md" (or (getenv "XDG_CONFIG_HOME") "~/.config"))))

;;; --- The manual ---------------------------------------------------------------------------

(defconst esploro--code-folder
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Where this file is: the manual is beside it, in ../doc.")

(defun esploro--manual-file ()
  "Esploro's Info manual: beside this code (the repository, or where Vikix
built it), else the one Info knows by name."
  (let ((beside (expand-file-name "../doc/esploro.info" esploro--code-folder)))
    (if (file-readable-p beside) beside "esploro")))

(defvar-keymap esploro-manual-mode-map
  :doc "In Esploro's manual: Info's menu, not Emacs's others."
  "C-x C-c" #'esploro-close)
(dolist (key esploro--hidden-menus)
  (define-key esploro-manual-mode-map (vector 'menu-bar key) 'undefined))

(define-minor-mode esploro-manual-mode
  "Esploro's manual, in Info: its menus kept to Info's, lines wrapped by word."
  :keymap esploro-manual-mode-map
  (when esploro-manual-mode (visual-line-mode 1)))

(defun esploro-manual (&optional node)
  "Esploro's manual, at NODE (its top by default), beside the folder."
  (interactive)
  (let ((buffer (save-window-excursion
                  (info (format "(%s)%s" (esploro--manual-file) (or node "Top")) "*Esploro manual*")
                  (current-buffer))))
    (with-current-buffer buffer (esploro-manual-mode 1))
    (select-window (display-buffer-in-side-window
                    buffer '((side . right) (slot . 0) (window-width . 0.5)
                             (window-parameters (no-delete-other-windows . t)))))))

(defun esploro-manual-keys ()
  "The manual's page of keys and what the mouse does."
  (interactive)
  (esploro-manual "Keys and mouse"))

(define-obsolete-function-alias 'esploro-help #'esploro-manual "0.3")

(provide 'esploro)
;;; esploro.el ends here
