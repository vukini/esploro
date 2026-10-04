;;;; tags.lisp — words of your own on files: adding, taking off, and which you've used.
;;;;
;;;; A tag is kept on the file itself (files.lisp: user.xdg.tags). Tagging is a
;;;; plan like any change, (:tag PATH "a,b"), so it is journaled and undone.
;;;; The tags you've used are remembered in ~/.config/esploro/tags.lisp, for the
;;;; menus and the side of the window; one found on a file joins them.

(in-package #:esploro)

(defun tags-file ()
  (join-path (env-folder "XDG_CONFIG_HOME" ".config") "esploro" "tags.lisp"))

(defun known-tags ()
  "The tags you've used, by name."
  (let ((file (tags-file)))
    (sort (remove-duplicates
           (and (path-exists-p file)
                (loop for form in (ignore-errors (read-plan-file file))
                      when (and (consp form) (eq (first form) :tag) (stringp (second form)))
                        append (parse-tags (second form))))
           :test #'string-equal)
          #'string-lessp)))

(defun write-known-tags (tags)
  (write-forms (tags-file) (mapcar (lambda (tag) (list :tag tag)) tags)
               :comment ";; The tags you've used in Esploro, for its menus: (:tag \"NAME\"). The tags themselves are on the files.")
  tags)

(defun remember-tags (tags)
  "TAGS join the ones you've used."
  (let* ((known (known-tags))
         (new (remove-if (lambda (tag) (member tag known :test #'string-equal)) tags)))
    (when new
      (ignore-errors (write-known-tags (sort (append known (remove-duplicates new :test #'string-equal)) #'string-lessp))))
    tags))

(defun forget-tag (tag)
  "TAG leaves the ones offered; the files that have it keep it."
  (write-known-tags (remove tag (known-tags) :test #'string-equal)))

(defun tag-steps (how tag paths)
  "The plan that adds TAG to PATHS (HOW :add) or takes it off (:remove): a
step for each file it changes."
  (let ((tag (first (parse-tags tag))))
    (unless tag (error "a tag is a word or a few, without commas"))
    (loop for path in paths
          for have = (file-tags path)
          for has = (member tag have :test #'string-equal)
          when (and (eq how :add) (not has))
            collect (list :tag path (tags-text (append have (list tag))))
          when (and (eq how :remove) has)
            collect (list :tag path (tags-text (remove tag have :test #'string-equal))))))

(defun folder-tags (folder)
  "The entries of FOLDER that have tags: ((NAME TAG ...) ...)."
  (loop for name in (ignore-errors (folder-names folder))
        for tags = (file-tags (join-path folder name))
        when tags collect (cons name tags)))
