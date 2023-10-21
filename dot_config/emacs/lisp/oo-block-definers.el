(require 'oo-block)

(defun oo-defun-components (arglist)
  "Return the components of defun.
ARGLIST is the arglist of `defun' or similar macro.
The components returned are in the form of (name args (docstring declaration interactive-form) body)."
  (block! nil
    (let! (name args . body) arglist)
    (let! docstring (when (stringp (car body)) (pop body)))
    (let! declarations (when (equal 'declare (car-safe (car body))) (pop body)))
    (let! interactive-form (when (equal 'interactive (car-safe (car body))) (pop body)))
    (list name args (list docstring declarations interactive-form) body)))

(provide 'oo-block-definers)
