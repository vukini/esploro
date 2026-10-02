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
          (esploro--back nil) (esploro--forward nil) (esploro--sort 'name)
          (esploro--reverse nil) (esploro--hidden nil) (esploro--clipboard nil))
     (unwind-protect (progn ,@body)
       (when (get-buffer esploro-buffer-name) (kill-buffer esploro-buffer-name))
       (delete-directory esploro-tests--top t))))

(defun esploro-tests--names ()
  "The names listed in the Esploro buffer, . and .. left out."
  (with-current-buffer esploro-buffer-name
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

(ert-deftest esploro-menus-and-keys ()
  (should (keymapp esploro-file-menu))
  (should (keymapp esploro-folder-menu))
  (should (keymapp esploro-tool-bar-map))
  (should (eq (keymap-lookup esploro-mode-map "<mouse-3>") #'esploro-context-menu))
  (should (eq (keymap-lookup esploro-mode-map "C-y") #'esploro-paste)))

;;; --- A folder in the buffer --------------------------------------------------------------

(ert-deftest esploro-shows-a-folder ()
  (esploro-tests--world
   (esploro-tests--file "f/b.txt" "bbbbbbbbbb")
   (esploro-tests--file "f/a.txt" "a")
   (esploro-tests--file "f/.hidden")
   (make-directory (esploro-tests--path "f/sub"))
   (esploro-go (esploro-tests--path "f"))
   (should (equal (esploro-tests--names) '("sub" "a.txt" "b.txt")))
   (with-current-buffer esploro-buffer-name
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
   (with-current-buffer esploro-buffer-name
     (should (equal (dired-get-filename 'no-dir t) "two")))
   (esploro-forward)
   (should (equal (esploro--dir) (esploro-tests--path "one/two/")))
   (esploro-up)
   (should (equal (esploro--dir) (esploro-tests--path "one/")))
   (with-current-buffer esploro-buffer-name
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
     (with-current-buffer esploro-buffer-name
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
     (with-current-buffer esploro-buffer-name
       (should (eq (esploro--dnd-file (esploro--uri x) 'copy) 'copy))
       (should (eq (esploro--dnd-file (esploro--uri y) 'move) 'move)))
     (esploro--apply-dropped (esploro-tests--path "drop"))
     (should (file-exists-p (esploro-tests--path "drop/x.txt")))
     (should (file-exists-p x))
     (should (file-exists-p (esploro-tests--path "drop/y.txt")))
     (should-not (file-exists-p y))
     ;; Dropped back on its own folder: nothing happens, no copy.
     (with-current-buffer esploro-buffer-name
       (esploro--dnd-file (esploro--uri (esploro-tests--path "drop/x.txt")) 'copy))
     (esploro--apply-dropped (esploro-tests--path "drop"))
     (should-not (file-exists-p (esploro-tests--path "drop/x copy.txt"))))))

;;; esploro-tests.el ends here
