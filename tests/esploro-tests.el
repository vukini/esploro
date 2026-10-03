;;; esploro-tests.el --- Esploro's window, in batch  -*- lexical-binding: t -*-

;; make test, or:
;;   emacs --batch -Q -L emacs -l tests/esploro-tests.el -f ert-run-tests-batch-and-exit
;;
;; No frame is needed: the folder is shown in its buffer, and changes go
;; through the real esploro command (./esploro, which make builds first),
;; in a home folder of the tests' own.

(require 'ert)
(require 'esploro)

(defvar esploro-tests--top nil "Each test's folder, made by `esploro-tests--world'.")

(defun esploro-tests--path (&rest names)
  (expand-file-name (string-join names "/") esploro-tests--top))

(defun esploro-tests--file (name &optional text)
  (let ((path (esploro-tests--path name)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert (or text "hello")))
    path))

(defmacro esploro-tests--world (&rest body)
  "BODY in a fresh folder of its own, HOME and the Trash inside it, the core waited for."
  `(let* ((esploro-tests--top (file-name-as-directory (make-temp-file "esploro-el-" t)))
          (process-environment (append (list (concat "HOME=" esploro-tests--top)
                                             (concat "XDG_DATA_HOME=" esploro-tests--top ".local/share")
                                             (concat "XDG_STATE_HOME=" esploro-tests--top ".local/state")
                                             "ESPLORO_SWANK_PORT=9"
                                             "EMACS_SOCKET_NAME=/nonexistent/emacs-server")
                                       process-environment))
          (esploro-program (expand-file-name "esploro" (file-name-directory (directory-file-name
                                                                              (file-name-directory (locate-library "esploro"))))))
          (esploro--wait t)
          (esploro--clipboard nil))
     (unwind-protect (progn ,@body)
       (delete-other-windows)
       (mapc #'kill-buffer (esploro--views))
       (delete-directory esploro-tests--top t))))

(defun esploro-tests--names ()
  "The names listed in the Esploro buffer, . and .. left out."
  (with-current-buffer (esploro--view)
    (save-excursion
      (goto-char (point-min))
      (let (names)
        (while (not (eobp))
          (let ((name (dired-get-filename 'no-dir t)))
            (when (and name (not (member name '("." "..")))) (push name names)))
          (forward-line 1))
        (nreverse names)))))

;;; --- Pieces --------------------------------------------------------------------------

(ert-deftest esploro-switches ()
  (let ((esploro--hidden nil) (esploro--sort 'name) (esploro--reverse nil))
    (should (equal (esploro--switches) "-lhgG --group-directories-first --time-style=long-iso -v")))
  (let ((esploro--hidden t) (esploro--sort 'size) (esploro--reverse t))
    (should (equal (esploro--switches) "-lhgG --group-directories-first --time-style=long-iso -aSr"))))

(ert-deftest esploro-uris ()
  (let ((odd "/tmp/a b/ünï%code [x].txt"))
    (should (equal (esploro--uri-file (esploro--uri odd)) odd)))
  (should (null (esploro--uri-file "https://example.com/x")))
  (should (equal (esploro--uri-file "file://localhost/tmp/x") "/tmp/x")))

(ert-deftest esploro-uri-list-crlf ()
  ;; Dragged out, the file list ends its lines in CRLF, as RFC 2483 says
  ;; (winit, in Alacritty, reads nothing else).
  (should (equal (esploro--uri-list-crlf '(text/uri-list . "file:///a\nfile:///b\n"))
                 '(text/uri-list . "file:///a\r\nfile:///b\r\n")))
  (should (equal (esploro--uri-list-crlf '(text/uri-list . "file:///a\r\n"))
                 '(text/uri-list . "file:///a\r\n")))
  (should (null (esploro--uri-list-crlf nil)))
  (when (fboundp 'xselect-convert-to-text-uri-list)
    (should (string-suffix-p "\r\n" (cdr (xselect-convert-to-text-uri-list 'XdndSelection 'text/uri-list "/tmp/x"))))))

(ert-deftest esploro-copied-files ()
  (should (equal (esploro--parse-copied "copy\nfile:///tmp/a%20b\nfile:///tmp/c")
                 '(copy "/tmp/a b" "/tmp/c")))
  (should (equal (esploro--parse-copied "cut\nfile:///tmp/a") '(cut "/tmp/a")))
  ;; A plain URI list (a browser, a terminal) copies.
  (should (equal (esploro--parse-copied "file:///tmp/a\r\nfile:///tmp/b\r\n") '(copy "/tmp/a" "/tmp/b")))
  (should (null (esploro--parse-copied "just some text")))
  (should (null (esploro--parse-copied ""))))

(ert-deftest esploro-paste-steps ()
  (esploro-tests--world
   (let ((a (esploro-tests--file "here/a.txt"))
         (b (esploro-tests--file "there/b.txt"))
         (here (esploro-tests--path "here")))
     (esploro-tests--file "here/b.txt")
     (should (equal (esploro--paste-steps 'copy (list a) here)
                    (list (list :copy a (esploro-tests--path "here/a copy.txt")))))
     (should (null (esploro--paste-steps 'cut (list a) here)))
     (should (equal (esploro--paste-steps 'cut (list b) here)
                    (list (list :move b (esploro-tests--path "here/b 2.txt")))))
     (should (null (esploro--paste-steps 'copy (list here) (esploro-tests--path "here/inside")))))))

(ert-deftest esploro-bookmarks-and-places ()
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment)))
     (make-directory (esploro-tests--path "config/gtk-3.0") t)
     (make-directory (esploro-tests--path "my work") t)
     (with-temp-file (esploro-tests--path "config/gtk-3.0/bookmarks")
       (insert (esploro--uri (esploro-tests--path "my work")) " Work\n"
               (esploro--uri (esploro-tests--path "my work")) "\n"))
     (should (equal (esploro--bookmarks)
                    (list (cons "Work" (esploro-tests--path "my work"))
                          (cons "my work" (esploro-tests--path "my work")))))
     (should (assoc "Bookmarks" (esploro--places)))
     (should (equal (cdr (assoc "Trash" (cdr (assoc "" (esploro--places)))))
                    (esploro--trash-dir))))))

(ert-deftest esploro-menus-and-the-manual ()
  ;; A file manager's menus; Emacs's and dired's hidden on Esploro's frame.
  (dolist (map (list esploro-mode-map esploro-places-mode-map))
    (dolist (key '(file edit view go help-menu))
      (should (keymapp (lookup-key map (vector 'menu-bar key)))))
    (dolist (key '(options buffer tools operate mark regexp immediate subdir))
      (should (eq (lookup-key map (vector 'menu-bar key)) 'undefined))))
  (should (eq (keymap-lookup esploro-mode-map "?") #'esploro-manual))
  (should (eq (keymap-lookup esploro-mode-map "<f1>") #'esploro-manual))
  ;; The manual is beside the code, and opens at the page asked for.
  (should (string-suffix-p "doc/esploro.info" (esploro--manual-file)))
  (save-window-excursion
    (esploro-manual-keys)
    (with-current-buffer "*Esploro manual*"
      (should (equal Info-current-node "Keys and mouse")))
    (kill-buffer "*Esploro manual*")))

(ert-deftest esploro-menus-and-keys ()
  (should (keymapp esploro-file-menu))
  (should (keymapp esploro-folder-menu))
  (should (keymapp esploro-tool-bar-map))
  (should (eq (keymap-lookup esploro-mode-map "<mouse-3>") #'esploro-context-menu))
  (should (eq (keymap-lookup esploro-mode-map "C-y") #'esploro-paste))
  ;; Quitting in Esploro closes Esploro, never all of Emacs.
  (should (eq (keymap-lookup esploro-mode-map "<remap> <save-buffers-kill-terminal>") #'esploro-close))
  (should (eq (keymap-lookup esploro-places-mode-map "<remap> <save-buffers-kill-terminal>") #'esploro-close))
  (with-temp-buffer
    (esploro-mode 1)
    (should (eq (key-binding (kbd "C-x C-c")) #'esploro-close))
    (should (eq (key-binding [menu-bar file exit-emacs]) #'esploro-close))))

;;; --- A folder in the buffer --------------------------------------------------------------

(ert-deftest esploro-shows-a-folder ()
  (esploro-tests--world
   (esploro-tests--file "f/b.txt" "bbbbbbbbbb")
   (esploro-tests--file "f/a.txt" "a")
   (esploro-tests--file "f/.hidden")
   (make-directory (esploro-tests--path "f/sub"))
   (esploro-go (esploro-tests--path "f"))
   (should (equal (esploro-tests--names) '("sub" "a.txt" "b.txt")))
   (with-current-buffer (esploro--view)
     (should esploro-mode)
     (should (eq mouse-1-click-follows-link 'double))
     (should dired-mouse-drag-files)
     (should (eq (cdr (assoc "^file:" dnd-protocol-alist)) #'esploro--dnd-file))
     ;; A file can be dragged out from anywhere on its row, not only its name.
     (save-excursion
       (dired-goto-file (esploro-tests--path "f/a.txt"))
       (should (eq (lookup-key (get-text-property (line-beginning-position) 'keymap) [down-mouse-1])
                   #'dired-mouse-drag))))
   (esploro-toggle-hidden)
   (should (member ".hidden" (esploro-tests--names)))
   (esploro-toggle-hidden)
   (esploro-sort 'size)
   (should (equal (esploro-tests--names) '("sub" "b.txt" "a.txt")))
   (esploro-filter "a")
   (should (equal (esploro-tests--names) '("a.txt")))
   (esploro--refresh)
   (should (= (length (esploro-tests--names)) 3))))

(ert-deftest esploro-history ()
  (esploro-tests--world
   (make-directory (esploro-tests--path "one/two") t)
   (esploro-go (esploro-tests--path "one"))
   (esploro-go (esploro-tests--path "one/two"))
   (esploro-back)
   (should (equal (esploro--dir) (esploro-tests--path "one/")))
   (with-current-buffer (esploro--view)
     (should (equal (dired-get-filename 'no-dir t) "two")))
   (esploro-forward)
   (should (equal (esploro--dir) (esploro-tests--path "one/two/")))
   (esploro-up)
   (should (equal (esploro--dir) (esploro-tests--path "one/")))
   (with-current-buffer (esploro--view)
     (should (equal (dired-get-filename 'no-dir t) "two")))))

;;; --- Changes, through the real core ------------------------------------------------------

(ert-deftest esploro-changes-through-the-core ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((a (esploro-tests--file "w/a.txt")))
     (esploro-go (esploro-tests--path "w"))
     ;; Copy, then paste in the same folder: a copy beside it.
     (setq esploro--clipboard (cons 'copy (list a)))
     (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
       (esploro-paste))
     (should (file-exists-p (esploro-tests--path "w/a copy.txt")))
     (should (member "a copy.txt" (esploro-tests--names)))
     ;; Undo takes it away.
     (esploro-undo)
     (should-not (file-exists-p (esploro-tests--path "w/a copy.txt")))
     ;; A new folder, a rename, the Trash.
     (esploro-new-folder "made")
     (should (file-directory-p (esploro-tests--path "w/made")))
     (esploro-rename a "renamed.txt")
     (should (file-exists-p (esploro-tests--path "w/renamed.txt")))
     (esploro--apply (list (list :trash (esploro-tests--path "w/renamed.txt"))) "trashed")
     (should-not (file-exists-p (esploro-tests--path "w/renamed.txt")))
     ;; (Undoing the copy put the copy in the Trash too: undo never deletes.)
     (should (member "renamed.txt" (mapcar #'car (esploro--call (list "trash-list")))))
     (should (member "a copy.txt" (mapcar #'car (esploro--call (list "trash-list")))))
     ;; The Trash, and restoring from it.
     (esploro-show-trash)
     (should (esploro--in-trash-p))
     (should (member "renamed.txt" (esploro-tests--names)))
     (with-current-buffer (esploro--view)
       (dired-goto-file (expand-file-name "renamed.txt" (esploro--trash-dir)))
       (esploro-restore))
     (should (file-exists-p (esploro-tests--path "w/renamed.txt")))
     ;; A refused plan changes nothing and says why.
     (let ((answer (esploro--call (list "apply") (esploro--plan-text
                                                   (list (list :copy (esploro-tests--path "w/renamed.txt")
                                                               (esploro-tests--path "w/made")))))))
       (should (eq (car answer) :refused))))))

(ert-deftest esploro-drops-go-through-the-core ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((x (esploro-tests--file "elsewhere/x.txt"))
         (y (esploro-tests--file "elsewhere/y.txt")))
     (make-directory (esploro-tests--path "drop"))
     (esploro-go (esploro-tests--path "drop"))
     (with-current-buffer (esploro--view)
       (should (eq (esploro--dnd-file (esploro--uri x) 'copy) 'copy))
       (should (eq (esploro--dnd-file (esploro--uri y) 'move) 'move)))
     (esploro--apply-dropped)
     (should (file-exists-p (esploro-tests--path "drop/x.txt")))
     (should (file-exists-p x))
     (should (file-exists-p (esploro-tests--path "drop/y.txt")))
     (should-not (file-exists-p y))
     ;; Dropped back on its own folder: nothing happens, no copy.
     (with-current-buffer (esploro--view)
       (esploro--dnd-file (esploro--uri (esploro-tests--path "drop/x.txt")) 'copy))
     (esploro--apply-dropped)
     (should-not (file-exists-p (esploro-tests--path "drop/x copy.txt"))))))

(ert-deftest esploro-two-panes ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (esploro-tests--file "left/a.txt")
   (make-directory (esploro-tests--path "right"))
   (esploro-go (esploro-tests--path "left"))
   (let ((left (selected-window)))
     (esploro-split)
     (should (= (length (esploro--view-windows)) 2))
     (let ((right (esploro--other-pane left)))
       ;; The new pane starts at the same folder, then goes its own way.
       (should (equal (esploro--dir (window-buffer right)) (esploro-tests--path "left/")))
       (with-selected-window right (esploro-go (esploro-tests--path "right")))
       (should (equal (esploro--dir (window-buffer left)) (esploro-tests--path "left/")))
       (should (equal (esploro--dir (window-buffer right)) (esploro-tests--path "right/")))
       ;; Each pane has its own history and sort.
       (with-current-buffer (window-buffer right) (should (equal esploro--back (list (esploro-tests--path "left/")))))
       (with-current-buffer (window-buffer left) (should (null esploro--back)))
       (with-selected-window left (esploro-sort 'size))
       (with-current-buffer (window-buffer right) (should (eq esploro--sort 'name)))
       ;; Copy to the other pane, through the core.
       (with-selected-window left
         (with-current-buffer (window-buffer left)
           (dired-goto-file (esploro-tests--path "left/a.txt"))
           (esploro-copy-to-other-pane)))
       (should (file-exists-p (esploro-tests--path "right/a.txt")))
       (should (member "a.txt" (with-current-buffer (window-buffer right)
                                 (save-excursion (goto-char (point-min))
                                                 (let (n) (while (not (eobp))
                                                            (push (dired-get-filename 'no-dir t) n)
                                                            (forward-line 1))
                                                      n)))))
       ;; F3 again: one pane, and the other's view is gone.
       (let ((gone (window-buffer right)))
         (with-selected-window left (esploro-split))
         (should (= (length (esploro--view-windows)) 1))
         (should-not (buffer-live-p gone)))))))

(ert-deftest esploro-preview ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CACHE_HOME=" esploro-tests--top "cache") process-environment)))
     (esploro-tests--file "p/notes.el" "(defun hello () \"hi\")\n")
     (esploro-tests--file "p/data.bin" (string 0 1 2 3))
     (make-directory (esploro-tests--path "p/sub/inner") t)
     (esploro-tests--file "p/sub/one.txt")
     (should (eq (esploro--kind (esploro-tests--path "p/notes.el")) 'text))
     (should (eq (esploro--kind (esploro-tests--path "p/data.bin")) 'other))
     (should (eq (esploro--kind (esploro-tests--path "p/sub")) 'folder))
     (should (eq (esploro--kind "/x/a.JPG") 'image))
     (should (eq (esploro--kind "/x/a.mkv") 'video))
     (esploro-go (esploro-tests--path "p"))
     (esploro--preview-show)
     (let ((preview (esploro--preview-buffer)))
       (should (esploro--preview-window))
       ;; Text: its first lines, in its mode's colours.
       (with-current-buffer (esploro--view) (dired-goto-file (esploro-tests--path "p/notes.el")))
       (esploro--preview-update)
       (with-current-buffer preview
         (should (string-match-p "notes.el" (buffer-string)))
         (should (string-match-p "(defun hello" (buffer-string)))
         (goto-char (point-min)) (search-forward "defun")
         (should (get-text-property (1- (point)) 'face)))
       ;; A folder: what's in it.
       (with-current-buffer (esploro--view) (dired-goto-file (esploro-tests--path "p/sub")))
       (esploro--preview-update)
       (with-current-buffer preview
         (should (string-match-p "a folder of 2" (buffer-string)))
         (should (string-match-p "^inner/" (buffer-string))))
       ;; Two selected: how many, and their size.
       (with-current-buffer (esploro--view)
         (dired-goto-file (esploro-tests--path "p/notes.el")) (dired-mark 1)
         (dired-goto-file (esploro-tests--path "p/data.bin")) (dired-mark 1))
       (esploro--preview-update)
       (with-current-buffer preview (should (string-match-p "2 selected" (buffer-string))))
       (with-current-buffer (esploro--view) (dired-unmark-all-marks))
       ;; A picture too big to show as it is: the core's thumbnail, in place of the wait.
       (when (executable-find "magick")
         (call-process "magick" nil nil nil "-size" "300x200" "xc:red" (esploro-tests--path "p/big.tif"))
         (esploro--refresh)
         (with-current-buffer (esploro--view) (dired-goto-file (esploro-tests--path "p/big.tif")))
         (esploro--preview-update)
         (with-current-buffer preview
           (should-not (string-match-p "making a preview" (buffer-string)))
           (should (string-match-p "esploro/thumbs/.*\\.png" (buffer-string)))))
       ;; F11: hidden, and hidden stays hidden; again, shown.
       (esploro-preview-toggle)
       (should-not (esploro--preview-window))
       (should-not (esploro--preview-wanted-p))
       (esploro-preview-toggle)
       (should (esploro--preview-window))
       (should (esploro--preview-wanted-p))
       (delete-window (esploro--preview-window))
       (kill-buffer preview)))))

(defun esploro-tests--review (plan)
  "A review buffer for the plan in PLAN, as `esploro-review-plan' makes it,
without a frame."
  (let ((buffer (generate-new-buffer "review")))
    (with-current-buffer buffer
      (esploro-review-mode)
      (setq esploro--review-file plan esploro--review-why "a test")
      (esploro--review-keep-proposed))
    (esploro--review-show buffer)
    buffer))

(ert-deftest esploro-review-a-proposed-plan ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((plan (esploro-tests--path "plan.lisp"))
         (steps (list (list :mkdir (esploro-tests--path "made")))))
     (with-temp-file plan (insert (esploro--plan-text steps) "\n"))
     (should (equal (esploro--read-plan plan) steps))
     (should (string-match-p "make the folder .*made" (esploro--describe-step (car steps))))
     (should (equal (esploro--describe-step (list :rename "/a/b.txt" "c.txt")) "rename /a/b.txt to c.txt"))
     ;; Apply, Edit and Cancel, under the steps.
     (let ((buffer (esploro-tests--review plan)))
       (with-current-buffer buffer
         (should (string-match-p "1\\. make the folder" (buffer-string)))
         (should (string-match-p " Apply .* Edit .* Cancel " (buffer-string))))
       ;; Cancel: nothing happens, the proposal goes.
       (esploro--review-done buffer nil)
       (should-not (file-exists-p (esploro-tests--path "made")))
       (should-not (file-exists-p plan))
       (should-not (buffer-live-p buffer)))
     ;; Apply: through the core, so undo takes it back.
     (with-temp-file plan (insert (esploro--plan-text steps) "\n"))
     (esploro--review-done (esploro-tests--review plan) t)
     (should (file-directory-p (esploro-tests--path "made")))
     (esploro-undo)
     (should-not (file-directory-p (esploro-tests--path "made"))))))

(ert-deftest esploro-review-edit-the-plan ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let* ((plan (esploro-tests--path "plan.lisp"))
          (made (esploro-tests--path "made"))
          (other (esploro-tests--path "other"))
          (review nil) (edit nil))
     (with-temp-file plan (insert (esploro--plan-text (list (list :mkdir made))) "\n"))
     (setq review (esploro-tests--review plan))
     (with-current-buffer review (esploro-review-edit))
     (setq edit (esploro--review-edit-buffer review))
     (should edit)
     (with-current-buffer edit
       (should esploro-plan-edit-mode)
       ;; A step the core refuses: the review says so, and Apply won't.
       (erase-buffer)
       (insert (format "(:trash %S)\n" (esploro-tests--path "not-there")))
       (save-buffer))
     (with-current-buffer review
       (should esploro--review-edited)
       (should esploro--review-problems)
       (should (string-match-p "can't be applied" (buffer-string))))
     (esploro--review-done review t)
     (should (buffer-live-p review))
     ;; Mended: the review shows the plan as it now is, and applies that.
     (with-current-buffer edit
       (erase-buffer)
       (insert (format "(:mkdir %S)\n" other))
       (esploro-plan-edit-done))
     (should-not (buffer-live-p edit))
     (with-current-buffer review
       (should-not esploro--review-problems)
       (should (string-match-p "edited by you" (buffer-string)))
       (should (string-match-p "make the folder .*other" (buffer-string))))
     (esploro--review-done review t)
     (should (file-directory-p other))
     (should-not (file-exists-p made))
     (should-not (file-exists-p plan)))))

(ert-deftest esploro-commands-from-the-menu ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (skip-unless (executable-find "zip"))
  (esploro-tests--world
   (esploro-tests--file "c/note.txt" "hello")
   (esploro-go (esploro-tests--path "c"))
   (with-current-buffer (esploro--view) (dired-goto-file (esploro-tests--path "c/note.txt")))
   (let ((labels (mapcar (lambda (v) (aref v 0)) (esploro--commands-menu nil))))
     (should (member "Compress" labels))
     (should (member "Copy path" labels))
     (should-not (member "Duplicate..." labels)) ; Edit has Duplicate already
     (should-not (member "Shrink" labels)))      ; not for text
   (esploro-run-command "compress" (list (esploro-tests--path "c/note.txt")))
   (should (file-exists-p (esploro-tests--path "c/note.txt.zip")))))

(ert-deftest esploro-recipes ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment)))
     (esploro-tests--file "r/a.txt") (esploro-tests--file "r/b.txt")
     (make-directory (esploro-tests--path "r/done"))
     (esploro-go (esploro-tests--path "r"))
     (esploro--apply (list (list :move (esploro-tests--path "r/a.txt") (esploro-tests--path "r/done/a.txt"))) "moved")
     ;; z: the same again, on b.
     (with-current-buffer (esploro--view) (dired-goto-file (esploro-tests--path "r/b.txt")) (esploro-repeat))
     (should (file-exists-p (esploro-tests--path "r/done/b.txt")))
     ;; Kept by name, it's on the Recipes menu.
     (esploro-save-recipe "Done")
     (should (member "Done (move into ~/r/done)"
                     (mapcar (lambda (v) (and (vectorp v) (aref v 0))) (esploro--recipes-menu nil)))))))

(defun esploro-tests--reviews ()
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'esploro-review-mode)) (buffer-list)))

(defmacro esploro-tests--with-reviews (&rest body)
  "BODY, the review panels it opens closed after."
  `(unwind-protect (progn ,@body)
     (mapc #'kill-buffer (esploro-tests--reviews))))

(ert-deftest esploro-rename-by-pattern ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (esploro-tests--with-reviews
    (let ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment)))
      (esploro-tests--file "p/IMG_1.jpg") (esploro-tests--file "p/IMG_2.jpg") (esploro-tests--file "p/notes.txt")
      (esploro-go (esploro-tests--path "p"))
      ;; Nothing marked: everything here, and the pattern picks.
      (with-current-buffer (esploro--view) (esploro-rename-by-pattern "IMG_*.jpg" "Pic #n.jpg"))
      (let ((review (car (esploro-tests--reviews))))
        (should review)
        (with-current-buffer review
          (should (equal esploro--review-recipe '(:rename-by "IMG_*.jpg" "Pic #n.jpg")))
          (let ((text (buffer-string)))
            ;; Each file before and after; nothing renamed yet.
            (should (string-match-p "In ~?/.*p/, 2 renames" text))
            (should (string-match-p "1\\. IMG_1\\.jpg  →  Pic 1\\.jpg" text))
            (should (string-match-p "2\\. IMG_2\\.jpg  →  Pic 2\\.jpg" text))
            (should (string-match-p "left as they are, not fitting: notes.txt" text))
            (should (string-match-p "Keep as Recipe" text))
            (should-not (string-match-p "An agent" text))))
        (should (file-exists-p (esploro-tests--path "p/IMG_1.jpg")))
        (with-current-buffer review (esploro-review-keep-recipe "Pics"))
        (esploro--review-done review t))
      (should (equal (esploro-tests--names) '("Pic 1.jpg" "Pic 2.jpg" "notes.txt")))
      (esploro-undo)
      (should (equal (esploro-tests--names) '("IMG_1.jpg" "IMG_2.jpg" "notes.txt")))
      ;; Two files to one name: refused, no review.
      (with-current-buffer (esploro--view) (esploro-rename-by-pattern "*.jpg" "same.jpg"))
      (should-not (esploro-tests--reviews))
      ;; Kept, it's under Recipes, and marked files are what it takes.
      (let ((item (seq-find (lambda (v) (and (vectorp v) (string-prefix-p "Pics" (aref v 0))))
                            (with-current-buffer (esploro--view) (esploro--recipes-menu nil)))))
        (should (equal (aref item 0) "Pics (rename IMG_*.jpg to Pic #n.jpg)...")))
      (with-current-buffer (esploro--view)
        (dired-goto-file (esploro-tests--path "p/IMG_2.jpg")) (dired-mark 1)
        (esploro--recipe-run "Pics" (esploro--recipe-files) "Pics"))
      (let ((review (car (esploro-tests--reviews))))
        (should (string-match-p "IMG_2\\.jpg  →  Pic 1\\.jpg" (with-current-buffer review (buffer-string))))
        (esploro--review-done review nil))
      (should (equal (esploro-tests--names) '("IMG_1.jpg" "IMG_2.jpg" "notes.txt")))))))

(ert-deftest esploro-sort-by-kind ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (esploro-tests--with-reviews
    (esploro-tests--file "k/a.jpg") (esploro-tests--file "k/b.pdf") (esploro-tests--file "k/c.docx")
    (esploro-tests--file "k/Images/a.jpg")
    (esploro-go (esploro-tests--path "k"))
    (with-current-buffer (esploro--view) (esploro-sort-by-kind))
    (let ((review (car (esploro-tests--reviews))))
      (with-current-buffer review
        (let ((text (buffer-string)))
          (should (string-match-p "1 into .*k/Images, 1 into .*k/Documents" text))
          (should (string-match-p "c.docx (of no kind named)" text))
          (should (string-match-p "a.jpg as a 2.jpg" text))
          (should (string-match-p "make the folder .*Documents" text))))
      (esploro--review-done review t))
    (should (equal (esploro-tests--names) '("Documents" "Images" "c.docx")))
    (should (file-exists-p (esploro-tests--path "k/Images/a 2.jpg")))
    (should (file-exists-p (esploro-tests--path "k/Documents/b.pdf")))
    (esploro-undo)
    (should (equal (esploro-tests--names) '("Images" "a.jpg" "b.pdf" "c.docx"))))))

(ert-deftest esploro-searches ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment)))
     (esploro-tests--file "s/a/report.pdf") (esploro-tests--file "s/b/notes.md")
     (esploro-go (esploro-tests--path "s"))
     (with-current-buffer (esploro--view)
       (esploro-search "kind:pdf")
       (should (equal (car esploro--search) "kind:pdf"))
       (should (save-excursion (goto-char (point-min)) (search-forward "a/report.pdf" nil t)))
       (should-not (save-excursion (goto-char (point-min)) (search-forward "notes.md" nil t)))
       ;; Back goes to the folder searched.
       (should (equal (car esploro--back) (esploro-tests--path "s/")))
       (esploro-save-search "PDFs"))
     (should (equal (mapcar #'car (esploro--searches)) '("PDFs")))
     (should (assoc "Searches" (esploro--places)))
     ;; F5 looks again: a new one shows.
     (esploro-tests--file "s/b/new.pdf")
     (with-current-buffer (esploro--view)
       (revert-buffer)
       (should (save-excursion (goto-char (point-min)) (search-forward "b/new.pdf" nil t)))
       (esploro-go (esploro-tests--path "s"))
       (should-not esploro--search))
     (esploro-forget-search "PDFs")
     (should-not (esploro--searches)))))

(ert-deftest esploro-selection-for-others ()
  ;; What the core asks Emacs (esploro selection), asked here.
  (let ((elisp (with-temp-buffer
                 (insert-file-contents (expand-file-name "../src/where.lisp" (file-name-directory (locate-library "esploro"))))
                 (search-forward "(defparameter *selection-elisp*")
                 (goto-char (match-beginning 0))
                 (car (read-from-string (nth 2 (read (current-buffer))))))))
    (esploro-tests--world
     (esploro-tests--file "sel/a.txt") (esploro-tests--file "sel/b.txt") (esploro-tests--file "sel/c.txt")
     (esploro-go (esploro-tests--path "sel"))
     (with-current-buffer (esploro--view)
       (dired-goto-file (esploro-tests--path "sel/b.txt"))
       (should (equal (eval elisp t) (list (esploro-tests--path "sel/") (list (esploro-tests--path "sel/b.txt")))))
       (dired-goto-file (esploro-tests--path "sel/a.txt")) (dired-mark 1)
       (dired-goto-file (esploro-tests--path "sel/c.txt")) (dired-mark 1)
       (should (equal (cadr (eval elisp t)) (list (esploro-tests--path "sel/a.txt") (esploro-tests--path "sel/c.txt"))))))))

(ert-deftest esploro-closing-a-project ()
  (esploro-tests--world
   (make-directory (esploro-tests--path "proj/.git") t)
   (let* ((saved (find-file-noselect (esploro-tests--file "proj/a.txt")))
          (unsaved (find-file-noselect (esploro-tests--file "proj/src/b.txt")))
          (outside (find-file-noselect (esploro-tests--file "elsewhere.txt"))))
     (with-current-buffer unsaved (insert "more"))
     (unwind-protect
         (progn
           (esploro-go (esploro-tests--path "proj/src"))
           (esploro-close-project)
           (with-current-buffer (format "*Esploro: %s*" (abbreviate-file-name (directory-file-name (esploro-tests--path "proj"))))
             (should (equal esploro--project (directory-file-name (esploro-tests--path "proj"))))
             (let ((text (buffer-string)))
               ;; Unsaved first; what's outside the project isn't there.
               (should (< (string-search "Not saved" text) (string-search "src/b.txt" text)
                          (string-search "Open in Emacs" text)
                          (string-search "a.txt" text (string-search "Open in Emacs" text))))
               (should-not (string-search "elsewhere" text)))
             (esploro-project-close-all)
             (should-not (buffer-live-p saved))
             (should (buffer-live-p unsaved))
             (esploro-project-save-all)
             (should-not (buffer-modified-p unsaved))
             (esploro-project-close-all)
             (should-not (buffer-live-p unsaved))
             (should (buffer-live-p outside))
             (esploro-project-refresh)
             (should (string-search "Nothing open in it" (buffer-string)))
             (kill-buffer)))
       (dolist (b (list saved unsaved outside))
         (when (buffer-live-p b) (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))))))

(ert-deftest esploro-menus-say-each-thing-once ()
  ;; No item twice in a menu, and Commands leaves out what the menus have.
  (cl-labels ((names (items) (delq nil (mapcar (lambda (i) (cond ((vectorp i) (aref i 0)) ((consp i) (car i)))) items))))
    (dolist (menu (append (mapcar #'cdr esploro--menu-bar)
                          (list (cdr (assq 'menu-bar nil)))))
      (let ((n (names (cdr menu))))
        (should (equal n (seq-uniq n))))))
  (should (member "trash" esploro--commands-on-menus))
  (let ((n (mapcar (lambda (v) (aref v 0)) (seq-filter #'vectorp (esploro--recipes-menu nil)))))
    (should-not (member "Repeat Last Change" n))
    (should (member "Save Last Change As..." n)))
  (let ((n (mapcar (lambda (v) (aref v 0)) (seq-filter #'vectorp (esploro--searches-menu nil)))))
    (should (member "Keep This Search..." n))))

(ert-deftest esploro-learn-from-your-edits ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let* ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment))
          (zip (esploro-tests--file "Downloads/Dataroom LME.zip"))
          (plan (esploro-tests--path "plan.lisp"))
          (rules (esploro-tests--path "config/esploro/sorting.md"))
          (asked nil) review)
     (make-directory (esploro-tests--path "Archives")) (make-directory (esploro-tests--path "Work"))
     (with-temp-file plan
       (insert (esploro--plan-text (list (list :move zip (esploro-tests--path "Archives/Dataroom LME.zip")))) "\n"))
     (setq review (esploro-tests--review plan))
     ;; You send it to Work instead.
     (with-current-buffer review (esploro-review-edit))
     (with-current-buffer (esploro--review-edit-buffer review)
       (erase-buffer)
       (insert (esploro--plan-text (list (list :move zip (esploro-tests--path "Work/Dataroom LME.zip")))) "\n")
       (esploro-plan-edit-done))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (q) (setq asked q) t))
               ((symbol-function 'read-string)
                (lambda (_prompt initial) (should (string-match-p "goes in ~/Work, not in ~/Archives" initial))
                  "A work zip goes in Work, not in Archives.")))
       (esploro--review-done review t))
     (should (file-exists-p (esploro-tests--path "Work/Dataroom LME.zip")))
     (should (string-match-p "1 of the agent's steps" asked))
     (should (string-match-p "## Learnt from my corrections\n\n- A work zip goes in Work, not in Archives\\."
                             (with-temp-buffer (insert-file-contents rules) (buffer-string))))
     ;; The plan's files are gone; the correction is kept for later.
     (should-not (file-exists-p plan))
     (should-not (file-exists-p (concat plan ".proposed")))
     (should (file-exists-p (esploro-tests--path ".local/state/esploro/corrections.lisp"))))))

(defun esploro-tests--button (label)
  "Where the button LABEL is, in this buffer."
  (save-excursion
    (goto-char (point-min))
    (let (b)
      (while (and (setq b (next-button (point))) (not (equal (button-label b) label)))
        (goto-char (button-end b)))
      (and b (button-start b)))))

(ert-deftest esploro-habits-noticed ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment))
         (said nil))
     (make-directory (esploro-tests--path "Pics/Trips") t)
     (dolist (n '("trip-1.jpg" "trip-2.jpg" "trip 3.png"))
       (esploro-tests--file (concat "in/" n)))
     (esploro-go (esploro-tests--path "in"))
     (cl-letf (((symbol-function 'run-at-time) (lambda (_ _ _f fmt &rest args) (setq said (apply #'format fmt args)))))
       (esploro--apply (list (list :move (esploro-tests--path "in/trip-1.jpg") (esploro-tests--path "Pics/Trips/trip-1.jpg"))) "moved")
       (should-not said)
       (esploro--apply (list (list :move (esploro-tests--path "in/trip-2.jpg") (esploro-tests--path "Pics/Trips/trip-2.jpg"))
                             (list :move (esploro-tests--path "in/trip 3.png") (esploro-tests--path "Pics/Trips/trip 3.png")))
                       "moved"))
     ;; Told once, after the move that made it a habit.
     (should (string-match-p "noticed: You've moved 3 pictures named \"trip\"" said))
     (save-window-excursion
       (esploro-habits)
       (with-current-buffer "*Esploro: habits*"
         (should (string-match-p "Keep as Recipe" (buffer-string)))
         (cl-letf (((symbol-function 'read-string) (lambda (_p initial) initial)))
           (push-button (esploro-tests--button " Keep as Recipe... ")))
         (should (assoc "Into Trips" (esploro--call (list "recipe" "list") nil nil t)))
         (push-button (esploro-tests--button " Not This "))
         (should (string-match-p "Nothing yet" (buffer-string)))
         (kill-buffer))))))

;;; esploro-tests.el ends here
