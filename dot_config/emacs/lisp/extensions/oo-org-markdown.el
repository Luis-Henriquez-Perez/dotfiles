;; This extension adds org markup to non-org files.

;; See the [[][relevant-section for this]].  I'd really like some expressiveness.

(defun oo-add-org-italic-emphasis ()
  "Add Org-style italic emphasis to comments in Lisp modes."
  (font-lock-add-keywords nil '(("\\(/[^/\n]+/\\)" 1 'italic))))

(add-hook 'emacs-lisp-mode-hook 'oo-add-org-italic-emphasis)
(add-hook 'lisp-mode-hook 'oo-add-org-italic-emphasis)

(provide 'oo-org-markup-minor-mode)
