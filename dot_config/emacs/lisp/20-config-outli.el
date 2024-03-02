;; TODO: figure out how to make this a named advice.
(advice-add 'load-theme :after (lambda (&rest _) (outli-reset-all-faces)))

;; TODO: set the style for all configs.
(setf (cl-fourth (assoc 'emacs-lisp-mode outli-heading-config)) nil)

(provide '20-config-outli)
