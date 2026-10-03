;;;; esploro.asd — Esploro, a file explorer for Lisp desktops.
;;;;
;;;; Two systems, both needing only SBCL: esploro/core (folders, plans,
;;;; where files are open, commands) and esploro, the command the window
;;;; calls. The window is in Emacs (emacs/esploro.el); the McCLIM window
;;;; it had first is kept on the branch mcclim.

(defsystem "esploro/core"
  :description "Esploro without its window: folders, plans, commands, and where files are open."
  :author "Vid <vukini@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ((:require "sb-posix") (:require "sb-bsd-sockets") (:require "sb-md5"))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "files")
               (:file "plan")
               (:file "stumpwm")
               (:file "where")
               (:file "commands")
               (:file "recipes")
               (:file "searches")
               (:file "learn")
               (:file "habits")
               (:file "preview")))

(defsystem "esploro"
  :description "A file explorer for Lisp desktops: StumpWM, with Emacs beside it. This is its command; the window is in Emacs (emacs/esploro.el)."
  :author "Vid <vukini@gmail.com>"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("esploro/core")
  :pathname "src/"
  :serial t
  :components ((:file "main")))
