(defsubst +captain-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss (point))))

(defun! +captain-prog-mode-sentence-start-function ()
  "Return point at the start of the last sentence.
Mean to be used as the value of `captain-predicate'."
  (cl-assert (require 'smartparens nil 'noerror))
  (awhen (car (bounds-of-thing-at-point 'sentence))
    (pushing! points it))
  (acond ((save-excursion (and (comment-beginning) (point)))
          (pushing! points it))
         ((and (nth 8 (syntax-ppss (point))) (sp-in-docstring-p nil nil 'string))
          (pushing! points it)))
  (apply #'max points))

(defhook! auto-capitalize-sentences-in-docstrings-and-comments (prog-mode-hook)
  (captain-mode 1)
  (setq-local captain-predicate #'+captain-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'+captain-prog-mode-sentence-start-function))

(defhook! auto-capitalize-sentences (text-mode-hook)
  (captain-mode 1)
  (setq-local captain-predicate (lambda () t)))

(provide 'oo-captain-config)

