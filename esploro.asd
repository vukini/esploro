;;;; esploro.asd — Esploro, a file explorer for Lisp desktops.
;;;;
;;;; Two systems: esploro/core has no window and needs only SBCL (so its
;;;; tests run anywhere, without X or McCLIM); esploro is the window,
;;;; with McCLIM from Quicklisp.

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
               (:file "preview")))

(defsystem "esploro"
  :description "A file explorer in Common Lisp for Lisp desktops: StumpWM, with Emacs beside it."
  :author "Vid <vukini@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("esploro/core" "mcclim")
  :pathname "src/"
  :serial t
  :components ((:file "ui")
               (:file "main")))
