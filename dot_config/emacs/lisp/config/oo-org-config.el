;;; org
;;;; source blocks 
;;;; refiling
;;;;; create a heading if necessary
(set! org-refile-allow-creating-parent-nodes t)

;;;;; set the refile targets to all my org files
;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(set! org-refile-targets '((oo-directory-files :maxlevel . 10)))

;;;;; don't complete in steps; show me all the information at once
;; [[https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html][Aaron Bieber's post]] explains this pretty well.  Since we're using a completion framework--in my
;; case, primarily =corfu= we don't need to complete in steps.  It's more expedient to just select from
;; all the candidates.
(setq org-outline-path-complete-in-steps nil)

;;;;; don't use a cache for refiling
(set! org-refile-use-cache nil)

;;;;; allow files as targets for refiling
;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(set! org-refile-use-outline-path 'file)

;;;;; when refiling, only consider headlines without a source block
;; :PROPERTIES:
;; :ID:       20230825T145959.961216
;; :END:
;; I will never let a headline with a source block be the parent of another headline.  It is simply not my style.
(set! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
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
(provide 'oo-org-config)
