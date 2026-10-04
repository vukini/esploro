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
    (dolist (key '(esploro-file esploro-edit esploro-view esploro-go esploro-help))
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

(ert-deftest esploro-commands-from-embark ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (skip-unless (executable-find "zip"))
  ;; The loader names the commands for embark before Esploro is loaded.
  (with-temp-buffer
    (insert-file-contents (expand-file-name "esploro-loaddefs.el" (file-name-directory (locate-library "esploro"))))
    (should (search-forward "esploro-file-commands" nil t))
    (should (search-forward "embark-file-map \",\"" nil t)))
  (esploro-tests--world
   (let ((file (esploro-tests--file "e/notes.txt")))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt table &rest _)
                  (let ((labels (all-completions "" table)))
                    (should (member "Copy path" labels))
                    (should (member "Duplicate..." labels))  ; outside Esploro, everything
                    "Compress"))))
       (esploro-file-commands file))
     (should (file-exists-p (esploro-tests--path "e/notes.txt.zip"))))))

(ert-deftest esploro-progress-of-a-long-copy ()
  ;; The core's progress lines, however they arrive, reach the reporter.
  (let* ((got '())
         (filter (esploro--progress-filter (lambda (done all name) (push (list done all name) got)))))
    (funcall filter nil "(:progress 10 100 \"big")
    (funcall filter nil ".iso\")\n(:progress 50 100 \"big.iso\")\nnoise\n(:prog")
    (should (equal (reverse got) '((10 100 "big.iso") (50 100 "big.iso")))))
  (let ((said nil))
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (funcall (esploro--progress-reporter "copied") 524288000 1048576000 "big.iso")
      (should (string-match-p "big.iso, 500M of 1000M (50%).*C-c C-k stops" said))
      (esploro--say '(:cancelled 2) "copied")
      (should (string-match-p "stopped; 2 steps done stay" said))))
  (should (eq (keymap-lookup esploro-mode-map "C-c C-k") #'esploro-cancel)))

(ert-deftest esploro-archives-like-folders ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (skip-unless (and (executable-find "archivemount") (file-exists-p "/dev/fuse")))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CACHE_HOME=" esploro-tests--top "cache") process-environment))
         (esploro--archives '()))
     (esploro-tests--file "z/in/notes.txt" "hello")
     (let ((default-directory "/")) (call-process "tar" nil nil nil "czf" (esploro-tests--path "z/x.tgz") "-C" (esploro-tests--path "z") "in"))
     (delete-directory (esploro-tests--path "z/in") t)
     (esploro-go (esploro-tests--path "z"))
     (unwind-protect
         (with-current-buffer (esploro--view)
           (esploro-open (esploro-tests--path "z/x.tgz"))
           (let ((point (cdr (assoc (esploro-tests--path "z/x.tgz") esploro--archives))))
             (should point)
             (should (equal (esploro--dir) (file-name-as-directory point)))
             (should (string-match-p "inside .*x.tgz (read-only)" (esploro--header)))
             (esploro-go (expand-file-name "in" point))
             (should (string-match-p "x.tgz (read-only): in" (esploro--header)))
             ;; Copied out like from any folder.
             (esploro--apply (list (list :copy (expand-file-name "in/notes.txt" point) (esploro-tests--path "z/notes.txt"))) "copied")
             (should (file-exists-p (esploro-tests--path "z/notes.txt")))
             ;; Up, up: beside the archive again, and it's closed.
             (esploro-up) (esploro-up)
             (should (equal (esploro--dir) (esploro-tests--path "z/")))
             (should-not (file-directory-p point))
             ;; Back goes in again.
             (esploro-back)
             (should (file-directory-p point))
             (esploro-go (esploro-tests--path "z"))
             (should-not (file-directory-p point))))
       (dolist (a esploro--archives)
         (let ((default-directory "/")) (call-process "fusermount" nil nil nil "-u" (cdr a))))))))

(ert-deftest esploro-thumbnails-in-the-list ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (skip-unless (or (executable-find "magick") (executable-find "convert")))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CACHE_HOME=" esploro-tests--top "cache") process-environment)))
     (make-directory (esploro-tests--path "t") t)
     (let ((default-directory "/"))
       (call-process (if (executable-find "magick") "magick" "convert") nil nil nil
                     "-size" "300x200" "xc:red" (esploro-tests--path "t/red.png")))
     (esploro-tests--file "t/notes.txt")
     (esploro-go (esploro-tests--path "t"))
     (with-current-buffer (esploro--view)
       (esploro-thumbnails-toggle)
       (should esploro--thumbnails)
       (let ((shown (seq-filter (lambda (o) (overlay-get o 'esploro-thumbnail))
                                (overlays-in (point-min) (point-max)))))
         ;; A row each; the picture's has its thumbnail, the text's a blank.
         (should (= 2 (length shown)))
         (should (seq-some (lambda (o) (let ((d (get-text-property 0 'display (overlay-get o 'before-string))))
                                         (eq (car-safe d) 'image)))
                           shown)))
       ;; Kept across a refresh, and gone when toggled off.
       (revert-buffer)
       (should (seq-some (lambda (o) (overlay-get o 'esploro-thumbnail)) (overlays-in (point-min) (point-max))))
       (esploro-thumbnails-toggle)
       (should-not (seq-some (lambda (o) (overlay-get o 'esploro-thumbnail)) (overlays-in (point-min) (point-max))))))))

(ert-deftest esploro-grid ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CACHE_HOME=" esploro-tests--top "cache") process-environment)))
     (dolist (n '("a.txt" "b.txt" "c.txt" "d.txt" "e.txt" "f.txt" "g.txt"))
       (esploro-tests--file (concat "g/" n)))
     (make-directory (esploro-tests--path "g/sub"))
     (esploro-go (esploro-tests--path "g"))
     (with-current-buffer (esploro--view)
       (esploro-grid-toggle)
       (should esploro-grid-mode)
       (let ((tiles (seq-filter (lambda (o) (overlay-get o 'esploro-grid-file)) (overlays-in (point-min) (point-max)))))
         (should (= 8 (length tiles)))
         (should (seq-every-p (lambda (o) (eq (car-safe (overlay-get o 'display)) 'image)) tiles)))
       ;; The arrows: along a row, and a row down.
       (setq esploro--grid-columns 3)
       (goto-char (point-min)) (esploro--grid-move 1)
       (let ((first (esploro--file-at)))
         (esploro-grid-right)
         (should-not (equal first (esploro--file-at)))
         (esploro-grid-left)
         (should (equal first (esploro--file-at)))
         (esploro-grid-down)
         (should (equal (esploro--file-at) (nth 3 (seq-filter #'identity
                                                              (save-excursion (goto-char (point-min))
                                                                              (let (fs) (while (not (eobp)) (push (esploro--grid-file) fs) (forward-line 1)) (nreverse fs))))))))
       ;; A selected tile is drawn as selected; the list's commands work.
       (dired-mark 1) (esploro--grid-refresh-tiles)
       (should (seq-some (lambda (o) (nth 1 (overlay-get o 'esploro-grid-state))) (overlays-in (point-min) (point-max))))
       (should (= 1 (length (dired-get-marked-files nil nil nil nil))))
       ;; A grid stays a grid when the folder is shown again; and back to the list.
       (revert-buffer)
       (should (seq-some (lambda (o) (overlay-get o 'esploro-grid-file)) (overlays-in (point-min) (point-max))))
       (esploro-grid-toggle)
       (should-not esploro-grid-mode)
       (should-not (seq-some (lambda (o) (overlay-get o 'esploro-grid)) (overlays-in (point-min) (point-max))))))))

(defvar esploro-tests-other-mode nil "A stand-in for another package's minor mode.")

(ert-deftest esploro-only-its-own-menus ()
  ;; Another package's menu, and Emacs's own File: hidden in Esploro's
  ;; buffers, as the menu bar is drawn; Esploro's stay.
  (esploro-tests--world
   (esploro-tests--file "m/a.txt")
   (esploro-go (esploro-tests--path "m"))
   (let ((other (make-sparse-keymap)))
     (define-key other [menu-bar virtual-envs] (cons "Virtual Envs" (make-sparse-keymap "Virtual Envs")))
     (let ((minor-mode-map-alist (cons (cons 'esploro-tests-other-mode other) minor-mode-map-alist))
           (esploro-tests-other-mode t))
       (with-current-buffer (esploro--view)
         (should esploro--menus-only)
         (run-hooks 'menu-bar-update-hook)
         (dolist (key '(virtual-envs file edit help-menu options))
           (should (eq (lookup-key esploro-menu-hider-map (vector 'menu-bar key)) 'undefined)))
         (should-not (lookup-key esploro-menu-hider-map [menu-bar esploro-file]))
         (should (keymapp (lookup-key esploro-mode-map [menu-bar esploro-file]))))))))

(ert-deftest esploro-open-on-a-workspace ()
  (esploro-tests--world
   (let ((file (esploro-tests--file "w/notes.txt")) (asked nil))
     (esploro-go (esploro-tests--path "w"))
     (with-current-buffer (esploro--view) (dired-goto-file file))
     (cl-letf (((symbol-function 'esploro--call)
                (lambda (args &optional _input then _sync)
                  (setq asked args)
                  (let ((answer (if (equal args '("workspaces"))
                                    '((1 "1" 2 "~/src/vikix" nil) (2 "2" 3 nil t) (3 "3" 0 nil nil))
                                  '(:opened 3 1))))
                    (if then (funcall then answer) answer)))))
       ;; Each workspace, and what it's about.
       (let ((labels (mapcar (lambda (v) (aref v 0)) (esploro--workspaces-menu nil))))
         (should (equal labels '("1   ~/src/vikix" "2   3 windows   (this one)" "3   empty"))))
       ;; M-o, then a number: the core is asked to go there and open it.
       (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?3)))
         (with-current-buffer (esploro--view) (call-interactively #'esploro-open-on-workspace)))
       (should (equal asked (list "open-on" "3" file)))
       (should (eq (keymap-lookup esploro-mode-map "M-o") #'esploro-open-on-workspace))))))

(ert-deftest esploro-bookmark-this-folder ()
  (esploro-tests--world
   (let ((process-environment (cons (concat "XDG_CONFIG_HOME=" esploro-tests--top "config") process-environment))
         (dir (esploro-tests--path "Work/Tenders")))
     (make-directory dir t)
     (esploro-go dir)
     (esploro-bookmark-folder "Tenders 2026")
     (should (equal (assoc "Tenders 2026" (esploro--bookmarks)) (cons "Tenders 2026" dir)))
     ;; GTK's own form, which PCManFM and the file dialogs read.
     (should (string-match-p "\\`file:///.*/Work/Tenders Tenders 2026\n\\'"
                             (with-temp-buffer (insert-file-contents (esploro--bookmarks-file)) (buffer-string))))
     (should (esploro--bookmarked-p dir))
     (should-error (esploro-bookmark-folder "again") :type 'user-error)
     (esploro-remove-bookmark)
     (should-not (esploro--bookmarked-p dir)))))

(ert-deftest esploro-git-status ()
  (skip-unless (executable-find "git"))
  (esploro-tests--world
   (let* ((repo (esploro-tests--path "repo"))
          (default-directory "/")
          (git (lambda (&rest args) (apply #'call-process "git" nil nil nil "-C" repo args))))
     (esploro-tests--file "repo/kept.txt" "one")
     (esploro-tests--file "repo/src/code.el" "one")
     (funcall git "init" "-q" "-b" "main")
     (funcall git "-c" "user.email=t@t" "-c" "user.name=t" "add" ".")
     (funcall git "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "first")
     (esploro-tests--file "repo/kept.txt" "two")       ; modified
     (esploro-tests--file "repo/fresh.md" "new")       ; new
     (esploro-tests--file "repo/src/code.el" "two")    ; a change inside src
     (esploro-go repo)
     (with-current-buffer (esploro--view)
       ;; git is asked in the background: wait for it.
       (let ((n 0)) (while (and (null esploro--git) (< n 50)) (accept-process-output nil 0.1) (setq n (1+ n))))
       (should esploro--git)
       (should (string-match-p "git: main" (esploro--header)))
       (let ((said (mapcar (lambda (o) (substring-no-properties (overlay-get o 'after-string)))
                           (seq-filter (lambda (o) (overlay-get o 'esploro-git)) (overlays-in (point-min) (point-max))))))
         (should (member "  modified" said))
         (should (member "  new" said))
         (should (member "  changes inside" said)))))))

(ert-deftest esploro-git-parse ()
  (should (equal (esploro--git-parse "## main...origin/main [ahead 2]\0 M a.txt\0?? b.md\0A  c.el\0R  new.txt\0old.txt\0UU d.org\0")
                 '("main...origin/main [ahead 2]" ("d.org" . conflict) ("new.txt" . staged) ("c.el" . added)
                   ("b.md" . new) ("a.txt" . modified))))
  (should (equal (esploro--git-branch-words "main...origin/main [ahead 2, behind 1]") "main, 2 to push, 1 to pull"))
  (should (equal (esploro--git-branch-words "No commits yet on main") "main")))

(ert-deftest esploro-opens-on-this-workspace ()
  ;; A frame on another workspace is iconified to Emacs: not where a file goes.
  (cl-letf (((symbol-function 'frame-list) (lambda () '(other here)))
            ((symbol-function 'frame-visible-p) (lambda (f) (if (eq f 'other) 'icon t)))
            ((symbol-function 'frame-parameter) (lambda (_f _p) nil))
            ((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
    (should (eq (esploro--other-frame) 'here)))
  (cl-letf (((symbol-function 'frame-list) (lambda () '(other)))
            ((symbol-function 'frame-visible-p) (lambda (_f) 'icon))
            ((symbol-function 'frame-parameter) (lambda (_f _p) nil))
            ((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
    (should-not (esploro--other-frame))))

(ert-deftest esploro-recent-files ()
  (esploro-tests--world
   (let ((a (esploro-tests--file "r/zeta.txt")) (b (esploro-tests--file "r/alpha.txt")))
     (esploro-go (esploro-tests--path "r"))
     (cl-letf (((symbol-function 'esploro--call)
                (lambda (_args &optional _input then _sync) (funcall then (list :recent (list a b))))))
       (with-current-buffer (esploro--view)
         (esploro-recent)
         (should esploro--recent)
         (should (string-match-p "Recent" (esploro--header)))
         ;; Newest first, as given: not sorted by name.
         (goto-char (point-min))
         (let ((names '()))
           (while (not (eobp)) (let ((f (esploro--grid-file))) (when f (push (file-name-nondirectory f) names))) (forward-line 1))
           (should (equal (nreverse names) '("zeta.txt" "alpha.txt"))))
         ;; F5 asks again; going to a folder leaves Recent.
         (revert-buffer)
         (should esploro--recent)
         (esploro-go (esploro-tests--path "r"))
         (should-not esploro--recent)))
     (should (assoc "Recent" (cdr (assoc "Places" (esploro--places))))))))

(ert-deftest esploro-whats-taking-space ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (esploro-tests--file "s/big/a.bin" (make-string 300000 ?x))
   (esploro-tests--file "s/big/inner/c.bin" (make-string 100000 ?x))
   (esploro-tests--file "s/mid.bin" (make-string 100000 ?x))
   (esploro-tests--file "s/tiny.txt" "x")
   (esploro-go (esploro-tests--path "s"))
   (with-current-buffer (esploro--view)
     (esploro-space-toggle)
     (should esploro--space)
     (should (numberp esploro--space-total))
     (cl-flet ((names () (save-excursion (goto-char (point-min))
                                         (let (ns) (while (not (eobp)) (let ((f (esploro--grid-file))) (when f (push (file-name-nondirectory (directory-file-name f)) ns))) (forward-line 1)) (nreverse ns)))))
       ;; Biggest first, whatever the name.
       (should (equal (names) '("big" "mid.bin" "tiny.txt")))
       (should (seq-some (lambda (o) (and (overlay-get o 'esploro-space) (string-match-p "█" (overlay-get o 'after-string))))
                         (overlays-in (point-min) (point-max))))
       ;; Into a folder: still a space view, measured there.
       (esploro-go (esploro-tests--path "s/big"))
       (should esploro--space)
       (should (equal (names) '("a.bin" "inner")))
       (should (string-match-p "Space: .*big" (esploro--header)))
       ;; And back to the list.
       (esploro-space-toggle)
       (should-not esploro--space)
       ;; The list again: folders first, by name.
       (should (equal (names) '("inner" "a.bin")))))))

(ert-deftest esploro-duplicates ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (let ((same (make-string 3000 ?q)))
     (esploro-tests--file "d/one/photo.jpg" same)
     (esploro-tests--file "d/two/photo (1).jpg" same)
     (esploro-tests--file "d/two/else.jpg" (make-string 3000 ?r))
     (set-file-times (esploro-tests--path "d/one/photo.jpg") (encode-time '(0 0 0 1 1 2020 nil nil t)))
     (esploro-go (esploro-tests--path "d"))
     (with-current-buffer (esploro--view)
       (esploro-find-duplicates)
       (should esploro--duplicates)
       (should (string-match-p "Duplicates below" (esploro--header)))
       (let ((said (mapcar (lambda (o) (substring-no-properties (overlay-get o 'after-string)))
                           (seq-filter (lambda (o) (overlay-get o 'esploro-duplicate)) (overlays-in (point-min) (point-max))))))
         (should (member "  1 · kept (the oldest)" said))
         (should (seq-some (lambda (s) (string-prefix-p "  1 · copy" s)) said)))
       (esploro-trash-duplicates)
       (let ((review (seq-find (lambda (b) (eq (buffer-local-value 'major-mode b) 'esploro-review-mode)) (buffer-list))))
         (should review)
         (with-current-buffer review
           (should (string-match-p "put .*photo (1).jpg in the Trash" (buffer-string)))
           (should-not (string-match-p "An agent proposes" (buffer-string)))
           (should-not (string-match-p "Keep as Recipe" (buffer-string))))
         (esploro--review-done review t))
       (should (file-exists-p (esploro-tests--path "d/one/photo.jpg")))
       (should-not (file-exists-p (esploro-tests--path "d/two/photo (1).jpg")))))))

(ert-deftest esploro-space-biggest-below ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (esploro-tests--file "b/deep/er/huge.bin" (make-string 4000000 ?x))
   (esploro-tests--file "b/photos/film.bin" (make-string 1000000 ?x))
   (dotimes (i 40) (esploro-tests--file (format "b/photos/p%d.jpg" i) (make-string 30000 ?x)))
   (esploro-tests--file "b/note.txt" "x")
   (esploro-go (esploro-tests--path "b"))
   (with-current-buffer (esploro--view)
     (cl-flet ((rows () (save-excursion
                          (goto-char (point-min))
                          (let (rows)
                            (while (not (eobp))
                              (let ((f (esploro--grid-file)))
                                (when f (push (file-relative-name (directory-file-name f) (esploro--dir)) rows)))
                              (forward-line 1))
                            (nreverse rows))))
               (after (name) (save-excursion
                               (dired-goto-file (esploro-tests--path name))
                               (mapconcat (lambda (o) (concat (overlay-get o 'before-string) (overlay-get o 'after-string)))
                                          (seq-filter (lambda (o) (overlay-get o 'esploro-space))
                                                      (overlays-in (line-beginning-position) (1+ (line-end-position))))
                                          ""))))
       ;; From the plain list, C-c S goes straight to the biggest below.
       (esploro-space-below-toggle)
       (should (eq esploro--space 'below))
       ;; The file four folders down first, by its way down; then the folder
       ;; of photos for its small things, and the film in it by itself.
       (should (equal (rows) '("deep/er/huge.bin" "photos" "photos/film.bin")))
       (should (string-match-p "its smaller things" (after "b/photos")))
       ;; Size, bar and share come before the name here.
       (should (string-match-p "[0-9.]+M █+ +[0-9]+%" (after "b/deep/er/huge.bin")))
       (should truncate-lines)
       (should-not (string-match-p "its smaller things" (after "b/photos/film.bin")))
       (should (string-match-p "the biggest anywhere below" (esploro--header)))
       (should (string-match-p "free: [0-9.]+[kMGT]? of " (esploro--header)))
       ;; Nothing marked, nothing said about marks.
       (should-not (string-match-p "marked:" (esploro--header)))
       ;; One marked: its size.  The folder and the film in it: the folder
       ;; counted once, whole (the film goes with it).
       (dired-goto-file (esploro-tests--path "b/photos/film.bin"))
       (dired-mark 1)
       (should (string-match-p "marked: 1, 9[0-9][0-9]k\\|marked: 1, 1\\(\\.0\\)?M" (esploro--header)))
       (dired-goto-file (esploro-tests--path "b/photos"))
       (dired-mark 1)
       (should (string-match-p "marked: 1, 2\\.[0-9]M" (esploro--header)))
       (should (equal (esploro--outermost (esploro--selection)) (list (esploro-tests--path "b/photos"))))
       ;; To the Trash: one step, the folder; the view measures again, and
       ;; the top line says what the Trash now holds.
       (esploro-trash)
       (should (equal (rows) '("deep/er/huge.bin")))
       (should-not (file-exists-p (esploro-tests--path "b/photos")))
       (should (string-match-p "Trash: 2\\.[0-9]M" (esploro--header)))
       ;; Emptying it says what it gives back, and the top line forgets it.
       (let (asked)
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) t)))
           (esploro-empty-trash))
         (should (string-match-p "(1 thing, 2\\.[0-9]M)" asked)))
       (should-not (string-match-p "Trash:" (esploro--header)))
       ;; C-c S again: this folder's own entries; C-c s: the list as it was.
       (esploro-space-below-toggle)
       (should (eq esploro--space t))
       (should (equal (rows) '("deep" "note.txt")))
       (esploro-space-toggle)
       (should-not esploro--space)))))

(ert-deftest esploro-empty-trash-already-empty ()
  (skip-unless (file-executable-p (expand-file-name "../esploro" (file-name-directory (locate-library "esploro")))))
  (esploro-tests--world
   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "asked about an empty Trash"))))
     (esploro-empty-trash))))

(ert-deftest esploro-dropbox-status ()
  ;; A stand-in dropbox command, and Dropbox's info.json saying where it is.
  (esploro-tests--world
   (let* ((bin (esploro-tests--path "bin"))
          (box (esploro-tests--path "Dropbox"))
          (exec-path (cons bin exec-path))
          (process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment)))
     (esploro-tests--file "Dropbox/Admin/a.pdf")
     (esploro-tests--file "Dropbox/notes.org")
     (esploro-tests--file "Dropbox/big.iso")
     (esploro-tests--file ".dropbox/info.json" (format "{\"personal\": {\"path\": \"%s\"}}" box))
     (esploro-tests--file "bin/dropbox"
                          (concat "#!/bin/sh\n"
                                  "case \"$1\" in\n"
                                  "  status) echo 'Syncing 2 files' ;;\n"
                                  "  exclude) echo 'Excluded: '; echo 'Old'; echo 'Photos' ;;\n"
                                  "  filestatus) shift; for f; do case \"$f\" in big.iso) echo \"$f: syncing\";; notes.org) echo \"$f: unsyncable\";; *) echo \"$f: up to date\";; esac; done ;;\n"
                                  "esac\n"))
     (set-file-modes (esploro-tests--path "bin/dropbox") #o755)
     (esploro-go box)
     (with-current-buffer (esploro--view)
       (let ((n 0)) (while (and (null (nth 1 esploro--dropbox)) (< n 50)) (accept-process-output nil 0.1) (setq n (1+ n))))
       (should (string-match-p "Dropbox: syncing 2 files, 2 folders online only" (esploro--header)))
       (let ((said (mapcar (lambda (o) (substring-no-properties (overlay-get o 'after-string)))
                           (seq-filter (lambda (o) (overlay-get o 'esploro-dropbox)) (overlays-in (point-min) (point-max))))))
         (should (member "  synced" said))
         (should (member "  syncing" said))
         (should (member "  can't sync" said))))
     ;; Outside Dropbox: nothing said.
     (esploro-go (esploro-tests--path "bin"))
     (with-current-buffer (esploro--view)
       (accept-process-output nil 0.3)
       (should-not esploro--dropbox)))))

;;; esploro-tests.el ends here
