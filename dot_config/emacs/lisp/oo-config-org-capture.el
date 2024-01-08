(require 'doct)

(defun oo-src-block-headline (title lang)
  "Return a headline with a source block."
  (let! lang (or lang "emacs-lisp"))
  (let! src-block (org-ml-build-src-block :language lang))
  (let! id-node (org-ml-build-node-property "ID" (org-id-new)))
  (let! property-drawer (org-ml-build-property-drawer id-node))
  (thread-last (org-ml-build-headline! :title-text "%?")
               (org-ml-headline-set-section (list property-drawer src-block))
               (org-ml-to-trimmed-string)))

;; (oo-it (list ,name :file (expand-file-name "~/dotfiles/emacs.org")))

;; (set! org-capture-templates (doct-add-to org-capture-templates (oo-it)))

;; (oo-it (list ))

;; (set! org-capture-templates (doct-add-to org-capture-templates (oo-it)))

;; (define! capture-template
;;          (:name)
;;          (:file)
;;          (:template))
