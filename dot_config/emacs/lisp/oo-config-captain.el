(defsubst +captain-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss (point))))

(defun! +captain-prog-mode-sentence-start-function ()
  "Return point at the start of the last sentence.
Mean to be used as the value of `captain-predicate'."
  (cl-assert (require 'smartparens nil 'noerror))
  (awhen (car (bounds-of-thing-at-point 'sentence))
    (pushing! points it))
  (apply #'max points))
