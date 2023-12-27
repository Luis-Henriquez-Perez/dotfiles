(oo-add-hook 'prog-mode-hook #'captain-mode)
(oo-add-hook 'text-mode-hook #'captain-mode)

(defhook! auto-capitalize-sentences (text-mode-hook)
  (setq-local captain-predicate (lambda () t)))

(defhook! auto-capitalize-sentences-in-docstrings-and-comments (prog-mode-hook)
  (setq-local captain-predicate #'+captain-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'+captain-prog-mode-sentence-start-function))

