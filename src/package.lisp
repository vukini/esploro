;;;; package.lisp

(defpackage #:esploro
  (:use #:cl)
  (:export
   ;; Paths are native strings, "/home/vid/notes.org": CL pathnames trip
   ;; on names with * or [ in them, and a plan must say exactly one file.
   #:normalize-path #:path-parent #:path-name #:join-path #:path-inside-p
   #:path-exists-p #:directory-p #:path-kind #:home-folder
   ;; Folders
   #:entry #:entry-path #:entry-name #:entry-kind #:entry-size #:entry-mtime
   #:entry-link-p #:entry-hidden-p #:list-folder #:kind-is
   ;; Plans
   #:*plan-operations* #:check-plan #:apply-plan #:undo-last #:read-plan
   #:read-plan-file #:write-plan #:describe-step #:journal-entries #:trash-folder #:state-folder
   #:plan-refused #:plan-refused-problems #:step-failed #:step-failed-step
   #:step-failed-reason #:skip-step #:retry-step #:stop-here #:undo-done
   ;; StumpWM
   #:stumpwm-eval #:stumpwm-unreachable
   ;; Where files are open
   #:window #:window-id #:window-class #:window-title #:window-group
   #:window-pid #:scan-where #:file-where #:focus-window #:window-files
   ;; Commands
   #:define-file-command #:file-command #:file-command-name
   #:file-command-doc #:file-command-changes #:file-command-label
   #:commands-for #:find-file-command #:run-file-command #:launch
   #:open-path
   ;; The window
   #:run #:main))

;;; Text read from elsewhere (Swank's replies, Emacs's answers, a plan
;;; edited by hand) is read in here, so whatever symbols it holds land in
;;; a package of their own, never in Esploro's.
(defpackage #:esploro.read
  (:use #:cl))
