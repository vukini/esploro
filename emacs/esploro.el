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
(dolist (v '(esploro--view esploro--back esploro--forward esploro--sort esploro--reverse esploro--hidden))
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

(defun esploro--call (args &optional input then sync)
  "Run the core with ARGS, INPUT on its standard input; THEN gets its answer.
In the background, so a long copy never stops Emacs; SYNC waits (tests)."
  (if (not (executable-find esploro-program))
      (message "Esploro: the esploro command isn't installed (vikix add esploro)")
    ;; From /: the core is given whole paths, and the folder Emacs happens
    ;; to be in may be gone.
    (let ((out (generate-new-buffer " *esploro-out*"))
          (default-directory "/"))
      (with-current-buffer out (setq default-directory "/"))
      (if (or sync esploro--wait)
          (let ((answer (with-current-buffer out
                          (when input (insert input))
                          (apply #'call-process-region (point-min) (point-max)
                                 esploro-program t t nil args)
                          (esploro--read-answer (buffer-string)))))
            (kill-buffer out)
            (when then (funcall then answer))
            answer)
        (let ((process (make-process
                        :name "esploro" :buffer out :command (cons esploro-program args)
                        :connection-type 'pipe :noquery t
                        :sentinel (lambda (process _event)
                                    (unless (process-live-p process)
                                      (let ((answer (with-current-buffer (process-buffer process)
                                                      (esploro--read-answer (buffer-string)))))
                                        (kill-buffer (process-buffer process))
                                        (when then (funcall then answer))))))))
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
    (esploro--call (list "apply") (esploro--plan-text steps)
                   (lambda (answer) (esploro--say answer what) (esploro--refresh))
                   sync)))

;;; --- Showing a folder -------------------------------------------------------------

(defun esploro--switches ()
  "ls's switches for the sort, the order and hidden files.
No owner or group (-g -G): size, time and name are what a file manager shows."
  (concat "-lhgG --group-directories-first --time-style=long-iso -"
          (if esploro--hidden "a" "")
          (pcase esploro--sort ('size "S") ('time "t") ('kind "X") (_ "v"))
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
      (setq esploro--filter nil)
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
      (rename-buffer (format "Esploro: %s" (abbreviate-file-name (directory-file-name
                                                                   (if (consp what) (car what) dir))))
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
         (up (file-name-directory here)))
    (if (equal (file-name-as-directory here) up) (message "Esploro: this is the top")
      (esploro-go up here))))

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
  "An Emacs frame to show a file in: a visible one that isn't Esploro's."
  (seq-find (lambda (f) (and (frame-visible-p f) (not (frame-parameter f 'esploro))
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
    (if (file-directory-p file)
        (esploro-go file)
      (if (executable-find esploro-program)
          (esploro--call (list "open" file) nil
                         (lambda (answer)
                           (pcase answer
                             (`(:emacs) (esploro--visit file))
                             (`(:window ,class) (message "Esploro: %s has it: went there" class))
                             (`(:error ,text) (message "Esploro: %s" text)))))
        (call-process "xdg-open" nil 0 nil file)))))

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
  (let ((files (or (esploro--selection) (user-error "Nothing selected"))))
    (esploro--apply (mapcar (lambda (f) (list :trash (directory-file-name f))) files)
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
  "Delete everything in the Trash, for good: this can't be undone."
  (interactive)
  (when (yes-or-no-p "Delete everything in the Trash for good? This can't be undone. ")
    (esploro--call (list "empty-trash") nil
                   (lambda (answer) (esploro--say answer "emptied") (esploro--refresh)))))

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
      (unless (get-buffer-window buffer t) (kill-buffer buffer)))))

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
  "M-s f" #'esploro-find
  "<f5>" #'revert-buffer
  "<f9>" #'esploro-places-toggle
  "<f3>" #'esploro-split
  "<f11>" #'esploro-preview-toggle
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
  `((file "File"
          ["New Window" esploro-new-window :keys "C-x 5 2"]
          ["New Folder..." esploro-new-folder :keys "+"]
          "---"
          ["Open" esploro-open :active (esploro--file-at)]
          ["Open With..." esploro-open-with :active (esploro--marked-or-point-p)]
          ["Terminal Here" esploro-terminal-here]
          ["Properties" esploro-properties]
          ("Commands" :filter esploro--commands-menu)
          "---"
          ["Close Esploro" esploro-close :keys "C-x C-c"])
    (edit "Edit"
          ["Undo" esploro-undo :keys "C-/"]
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
          ["Move to Trash" esploro-trash :keys "Delete" :active (esploro--marked-or-point-p)]
          "---"
          ["Select All" esploro-select-all]
          ["Select None" esploro-select-none]
          ["Invert Selection" esploro-invert-selection])
    (view "View"
          ["Sort by Name" (esploro-sort 'name) :style radio :selected (eq (esploro--value 'esploro--sort) 'name)]
          ["Sort by Size" (esploro-sort 'size) :style radio :selected (eq (esploro--value 'esploro--sort) 'size)]
          ["Sort by Time" (esploro-sort 'time) :style radio :selected (eq (esploro--value 'esploro--sort) 'time)]
          ["Sort by Kind" (esploro-sort 'kind) :style radio :selected (eq (esploro--value 'esploro--sort) 'kind)]
          ["The Other Way Round" (esploro-sort (esploro--value 'esploro--sort))
           :style toggle :selected (esploro--value 'esploro--reverse)]
          "---"
          ["Hidden Files" esploro-toggle-hidden :style toggle :selected (esploro--value 'esploro--hidden)]
          ["Filter..." esploro-filter :keys "/"]
          ["Find Below..." esploro-find :keys "M-s f"]
          ["Refresh" esploro--refresh :keys "F5"]
          "---"
          ["Two Panes" esploro-split :keys "F3" :style toggle :selected (esploro--two-panes-p)]
          ["Preview" esploro-preview-toggle :keys "F11" :style toggle :selected (esploro--preview-window)]
          ["Places" esploro-places-toggle :keys "F9" :style toggle
           :selected (get-buffer-window esploro-places-buffer-name)])
    (go "Go"
        ["Back" esploro-back :keys "M-<left>" :active (esploro--value 'esploro--back)]
        ["Forward" esploro-forward :keys "M-<right>" :active (esploro--value 'esploro--forward)]
        ["Up" esploro-up :keys "M-<up>"]
        ["Home" esploro-home]
        ["Go to Folder..." esploro-go-to :keys "C-l"]
        "---"
        ["The Trash" esploro-show-trash]
        ["Restore from the Trash" esploro-restore :active (esploro--in-trash-p)]
        ["Empty the Trash..." esploro-empty-trash])
    (help-menu "Help"
               ["Esploro Manual" esploro-manual :keys "?"]
               ["Keys and Mouse" esploro-manual-keys])))

(defconst esploro--hidden-menus
  '(options buffer tools operate mark regexp immediate subdir)
  "Emacs's menus (Options, Buffers, Tools) and dired's, hidden in Esploro.")

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
    ["Move to Trash" esploro-trash :visible (not (esploro--in-trash-p))]
    ["Restore" esploro-restore :visible (esploro--in-trash-p)]
    ["Properties" esploro-properties]))

(easy-menu-define esploro-folder-menu nil
  "Right-click on the folder (no file under the mouse)."
  '("Folder"
    ["Paste" esploro-paste]
    ["New Folder..." esploro-new-folder]
    ["Terminal Here" esploro-terminal-here]
    ["Two Panes" esploro-split :style toggle :selected (esploro--two-panes-p)]
    ["New Window" esploro-new-window]
    "---"
    ["Hidden Files" esploro-toggle-hidden :style toggle :selected esploro--hidden]
    ["Sort by Name" (esploro-sort 'name) :style radio :selected (eq esploro--sort 'name)]
    ["Sort by Time" (esploro-sort 'time) :style radio :selected (eq esploro--sort 'time)]
    ["Sort by Size" (esploro-sort 'size) :style radio :selected (eq esploro--sort 'size)]
    ["Refresh" revert-buffer]
    "---"
    ["Empty the Trash..." esploro-empty-trash :visible (esploro--in-trash-p)]
    ["Undo" esploro-undo]))

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
                    (esploro-find "search" "Find")
                    (revert-buffer "refresh" "Refresh")
                    (esploro-manual "help" "Help")))
      (if (null item)
          (define-key-after map (vector (gensym "sep")) menu-bar-separator)
        (tool-bar-local-item (nth 1 item) (nth 0 item) (nth 0 item) map
                             :label (nth 2 item) :help (nth 2 item))))
    map)
  "Esploro's tool bar.")

(defun esploro--header ()
  (concat " " (abbreviate-file-name (esploro--dir))
          (format "   sorted by %s%s" esploro--sort (if esploro--reverse ", the other way" ""))
          (if esploro--hidden "   hidden shown" "")
          (if esploro--filter (format "   only \"%s\" (F5: all)" esploro--filter) "")
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
    (setq-local header-line-format '(:eval (esploro--header)))
    (add-hook 'post-command-hook #'esploro--note-window nil t)
    (add-hook 'post-command-hook #'esploro--preview-schedule nil t)
    (add-hook 'dired-after-readin-hook #'esploro--whole-row-drag nil t)
    (add-hook 'dired-after-readin-hook #'esploro--annotate nil t)))

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

(defun esploro--drives ()
  "Mounted drives (udiskie mounts them in /run/media/USER): (NAME . FOLDER)."
  (let ((media (format "/run/media/%s" user-login-name)))
    (when (file-directory-p media)
      (mapcar (lambda (d) (cons (file-name-nondirectory d) d))
              (directory-files media t "\\`[^.]")))))

(defun esploro--places ()
  "Everything down the side, in groups: ((GROUP (NAME . FOLDER)...)...)."
  (seq-filter #'cdr
              (list (cons "Places" (seq-filter (lambda (p) (file-directory-p (cdr p)))
                                               (mapcar (lambda (p) (cons (car p) (expand-file-name (cdr p))))
                                                       esploro-places)))
                    (cons "Drives" (esploro--drives))
                    (cons "Bookmarks" (esploro--bookmarks))
                    (cons "" (list (cons "Trash" (esploro--trash-dir)))))))

(defvar-keymap esploro-places-mode-map
  :doc "Esploro's places."
  :parent special-mode-map
  "<remap> <save-buffers-kill-terminal>" #'esploro-close
  "<remap> <save-buffers-kill-emacs>" #'esploro-close)

(esploro--install-menu-bar esploro-places-mode-map)

(define-derived-mode esploro-places-mode special-mode "Places"
  "Esploro's places: click one, or RET on it."
  (setq-local cursor-type nil)
  (setq-local tool-bar-map esploro-tool-bar-map)
  (setq-local mode-line-format nil))

(defun esploro-places-refresh ()
  (when-let* ((buffer (get-buffer esploro-places-buffer-name)))
    ;; One places buffer for every Esploro frame: it shows where the pane
    ;; used last, in the frame you're in, is.
    (let ((here (when-let* ((frame (esploro--frame))
                            (view (esploro--view frame)))
                  (esploro--dir view))))
     (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (group (esploro--places))
          (unless (string-empty-p (car group))
            (insert (propertize (car group) 'face 'bold) "\n"))
          (dolist (place (cdr group))
            (insert "  ")
            (insert-text-button (car place)
                                'action (lambda (_) (esploro--from-places (cdr place)))
                                'follow-link t
                                'help-echo (abbreviate-file-name (cdr place))
                                'face (if (equal (file-name-as-directory (cdr place)) here) 'highlight 'default))
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

(defun esploro--commands-menu (_items)
  "The Commands submenu, made when it opens: what suits the selection."
  (let ((files (and (esploro--view) (with-current-buffer (esploro--view) (esploro--selection)))))
    (if (null files)
        (list ["Select a file first" ignore :active nil])
      (let ((answer (esploro--call (cons "commands" files) nil nil t)))
        (if (and (listp answer) (not (keywordp (car answer))) answer)
            (mapcar (lambda (c)
                      (vector (concat (nth 1 c) (if (nth 3 c) "..." ""))
                              (list 'esploro-run-command (nth 0 c))
                              :help (nth 2 c)))
                    answer)
          (list ["No commands for these" ignore :active nil]))))))

(defun esploro-run-command (name &optional files)
  "Run the file command NAME on FILES (the selection): at once, or, for one
that changes files, as a plan for your review."
  (interactive (list (read-string "Command: ")))
  (let ((files (or files (esploro--in-view (esploro--selection)) (user-error "Nothing selected"))))
    (esploro--call (append (list "run" name) files) nil
                   (lambda (answer)
                     (pcase answer
                       (`(:done ,_) (message "Esploro: %s, done" name) (esploro--refresh))
                       (`(:proposed ,n) (message "Esploro: %s proposes %d %s: review it" name n (if (= n 1) "step" "steps")))
                       (_ (esploro--say answer name)))))))

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
(defvar-local esploro--review-problems nil
  "What the core found wrong with the plan as it now is; nil when it can be applied.")
(defvar-local esploro--review-edited nil "Whether you've changed the plan.")
(defvar-local esploro--review-of nil "In a plan being edited: its review buffer.")

(defvar-keymap esploro-review-mode-map
  :doc "A plan to review: Esploro's menus."
  :parent special-mode-map
  "e" #'esploro-review-edit
  "C-x C-c" #'esploro-close)
(esploro--install-menu-bar esploro-review-mode-map)

(defface esploro-button '((t :box (:line-width 2 :style released-button) :weight bold :inherit default))
  "Apply, Edit and Cancel, under a plan to review.")

(define-derived-mode esploro-review-mode special-mode "Plan"
  "A plan proposed to you: Apply, Edit or Cancel."
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
      (insert (propertize "An agent proposes these changes. Nothing happens until you apply them; undo takes them back after.\n" 'face 'shadow))
      (when (and esploro--review-why (not (string-empty-p esploro--review-why)))
        (insert "\nWhy: " esploro--review-why "\n"))
      (insert "\n")
      (let ((n 0))
        (dolist (step steps)
          (insert (format "%d. %s\n" (setq n (1+ n)) (esploro--describe-step step)))))
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
      (insert "\n")
      (goto-char (point-min)))))

(defun esploro-review-plan (file &optional why where)
  "Show the plan in FILE (an agent's, checked by the core) in Esploro, on
your workspace (WHERE, as `esploro' takes it), with Apply, Edit and Cancel."
  (let* ((steps (esploro--read-plan file))
         (first-path (cadr (car steps)))
         (dir (if (and first-path (file-directory-p (file-name-directory first-path)))
                  (file-name-directory first-path)
                "~"))
         (frame (esploro dir where))
         (buffer (generate-new-buffer "*Esploro: a plan to review*")))
    (with-current-buffer buffer
      (esploro-review-mode)
      (setq esploro--review-file file
            esploro--review-why why))
    (esploro--review-show buffer)
    (with-selected-frame frame
      (select-window (display-buffer-in-side-window
                      buffer '((side . bottom) (slot . 0) (window-height . 0.35)
                               (window-parameters (no-delete-other-windows . t))))))
    (message "Esploro: a plan of %d %s to review" (length steps) (if (= (length steps) 1) "step" "steps"))
    buffer))

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
        (let ((steps (esploro--read-plan file)))
          (when apply (esploro--apply steps "the proposed plan, applied"))
          (unless apply (message "Esploro: the proposed plan was dropped; nothing changed"))
          (when edit (esploro--plan-edit-close edit buffer))
          (ignore-errors (delete-file file))
          (when-let* ((window (get-buffer-window buffer t))) (delete-window window))
          (kill-buffer buffer))))))

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
