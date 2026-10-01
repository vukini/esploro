;;;; build.lisp — Esploro as one program: make (or sbcl --load build.lisp)
;;;;
;;;; Needs Quicklisp (for McCLIM). Writes ./esploro.new (make moves it to
;;;; ./esploro), which opens at once instead of loading McCLIM each time.

(push (directory-namestring *load-truename*) asdf:*central-registry*)
(ql:quickload :esploro :silent t)

;; Read only now: the package exists once the system is loaded.
(sb-ext:save-lisp-and-die
 (concatenate 'string (directory-namestring *load-truename*) "esploro.new")
 :executable t
 ;; esploro --help is ours, not the SBCL runtime's.
 :save-runtime-options t
 :toplevel (symbol-function (find-symbol "MAIN" "ESPLORO")))
