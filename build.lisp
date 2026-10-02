;;;; build.lisp — the esploro command as one program: make (or sbcl --load build.lisp)
;;;;
;;;; Needs only SBCL. Writes ./esploro.new (make moves it to ./esploro). The
;;;; window is in Emacs (emacs/esploro.el), so nothing here needs Quicklisp.

(require :asdf)
(asdf:load-asd (merge-pathnames "esploro.asd" (directory-namestring *load-truename*)))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "esploro"))

;; Read only now: the package exists once the system is loaded.
(sb-ext:save-lisp-and-die
 (concatenate 'string (directory-namestring *load-truename*) "esploro.new")
 :executable t
 ;; esploro --help is ours, not the SBCL runtime's.
 :save-runtime-options t
 :toplevel (symbol-function (find-symbol "MAIN" "ESPLORO")))
