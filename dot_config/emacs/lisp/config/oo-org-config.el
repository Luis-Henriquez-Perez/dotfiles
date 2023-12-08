;;; org
;;;; refiling

;;;; capturing
;;;;; use =completing-read= for choosing a template
;; Org uses its own ugly looking interface for choosing a capture template.  I'd rather keep things
;; consistent and use =completing-read= which is what I use for everything else.
(defun! oo-choose-capture-template ()
  "Choose a capture template."
  (interactive)
  (dolist (template org-capture-templates)
    (let! (keys name) template)
    (collecting! alist (cons name keys)))
  (let! selected (completing-read "capture template: " alist nil t))
  (org-capture nil (alist-get selected alist nil nil #'string=)))

;;;;; open capture templates at bottom
(oo-popup-at-bottom "CAPTURE[^z-a]+")

;;;;; generate a generic source block headline
(defun oo-src-block-headline (title lang)
  (let! lang (or lang "emacs-lisp"))
  (let! src-block (org-ml-build-src-block :language lang))
  (let! id-node (org-ml-build-node-property "ID" (org-id-new)))
  (let! property-drawer (org-ml-build-property-drawer id-node))
  (thread-last (org-ml-build-headline! :title-text "%?")
               (org-ml-headline-set-section (list property-drawer src-block))
               (org-ml-to-trimmed-string)))
;;;;; defcapture!
;; This is a declarative macro for conveniently defining capture templates. I'll note that the plist
;; arguments for the macro are evaluated. This is because I feel that having dynamic arguments is more
;; useful.
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

;;;;; generic capture template
;; This capture template is designed for code that I write on the fly and don't know where it should be
;; organized yet. It will capture the headline in a headline deemed "uncategorized".
(defcapture! emacs ()
  :file (expand-file-name "~/dotfiles/emacs.org")
  :olp '("emacs" "uncategorized")
  :template (-partial (function oo-src-block-headline) nil "emacs-lisp"))

;;;;; outshine capture template
;; This is a capture template that inserts.
;; 1. a function that finds where to insert the template
;; 2. a function that generates the template
(defcapture! emacs ()
  :file (expand-file-name "~/dotfiles/dotfiles.org")
  :olp '("emacs" "uncategorized")
  :template (-partial (function oo-src-block-headline) nil "emacs-lisp"))

;;; provide
(provide 'oo-org-capture-configuration)
