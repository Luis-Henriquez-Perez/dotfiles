(defun oo-src-block-headline (title lang)
  "Return a headline with a source block."
  (let! lang (or lang "emacs-lisp"))
  (let! src-block (org-ml-build-src-block :language lang))
  (let! id-node (org-ml-build-node-property "ID" (org-id-new)))
  (let! property-drawer (org-ml-build-property-drawer id-node))
  (thread-last (org-ml-build-headline! :title-text "%?")
               (org-ml-headline-set-section (list property-drawer src-block))
               (org-ml-to-trimmed-string)))

(defmacro! defcapture! (&rest args)
  "Define a capture template with doct and add it to `org-capture-templates'."
  (declare (indent defun))
  (let! (name arglist metadata body) (oo-defun-components args))
  (let! name (oo-args-to-string name))
  (while (keywordp (car body))
    (appending! pairs (list (pop body) (pop body))))
  (unless (plist-get pairs :template)
    (appending! pairs (list :template body)))
  (unless (plist-get pairs :keys)
    (appending! pairs (list :keys (char-to-string (seq-first name)))))
  `(set! org-capture-templates (doct-add-to org-capture-templates (list ,name ,@pairs))))

(defcapture! emacs ()
  :file (expand-file-name "~/dotfiles/emacs.org")
  :olp '("emacs" "uncategorized")
  :template (-partial (function oo-src-block-headline) nil "emacs-lisp"))

(defcapture! emacs ()
  :file (expand-file-name "~/dotfiles/dotfiles.org")
  :olp '("emacs" "uncategorized")
  :template (-partial (function oo-src-block-headline) nil "emacs-lisp"))

(defcapture! notes ()
  :file (expand-file-name "~/notes.org")
  :prepend t
  :template (-partial (function oo-src-block-headline) nil "emacs-lisp"))
