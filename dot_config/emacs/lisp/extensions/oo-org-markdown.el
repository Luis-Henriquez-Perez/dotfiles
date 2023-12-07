(defun oo-add-org-italic-emphasis ()
  "Add Org-style italic emphasis to comments in Lisp modes."
  (font-lock-add-keywords nil '(("\\(/[^/\n]+/\\)" 1 'italic))))

(add-hook 'emacs-lisp-mode-hook 'oo-add-org-italic-emphasis)
(add-hook 'lisp-mode-hook 'oo-add-org-italic-emphasis)
