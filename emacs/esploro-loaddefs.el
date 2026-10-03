;;; esploro-loaddefs.el --- Esploro, before its first window  -*- lexical-binding: t -*-

;; Loaded at Emacs's start (one line in your config), this makes Esploro
;; there before you first open it, without loading it: M-x esploro, and,
;; with embark, X (Esploro's commands) and J (show it in Esploro) on any
;; file name.  Esploro itself (esploro.el, beside this) loads on first use.
;;
;;   (load "~/.local/opt/esploro/emacs/esploro-loaddefs" t t)

;;; Code:

(declare-function esploro-file-commands "esploro" (file))
(declare-function esploro-show-file "esploro" (file))

(let ((esploro (expand-file-name "esploro" (file-name-directory (or load-file-name buffer-file-name)))))
  (autoload 'esploro esploro "Show a folder in an Esploro frame." t)
  (autoload 'esploro-file-commands esploro "Esploro's commands for a file." t)
  (autoload 'esploro-show-file esploro "A file in Esploro, selected in its folder." t))

(with-eval-after-load 'embark
  (when (boundp 'embark-file-map)
    (keymap-set embark-file-map "X" #'esploro-file-commands)
    (keymap-set embark-file-map "J" #'esploro-show-file)))

(provide 'esploro-loaddefs)

;;; esploro-loaddefs.el ends here
