;; This file extends.
(require 'dash)
(require 'oo-base-ing-macros)

;; Emacs uses these symbols as markers in =defun= and =defmacro= signatures.  I'm
;; defining a function specifically for identifying these symbols so I can
;; differentiate them from actual argument symbols.
(defun oo-ampersand-symbol-p (obj)
  "Return non-nil of OBJ is an ampersand symbol.
An ampersand symbol is a symbol that starts with `&'."
  (and (symbolp obj) (string-match-p "\\`&" (symbol-name obj))))

;; In macros I often want access to defun arg components (see defhook! and
;; defadvice!).  Until I get better destructuring, I use this function to quickly
;; access these values.  Notably, I put docstring and declarations in a sublist
;; because it's so often that I want to lump them together.
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

;; This is a specific variant of function allows me to destructure the
;; arguments of a defun-like macro that can take keyword arguments.
(defun oo-destructure-defun-plus (arglist)
  "Return list (name args (docstring declarations) plist body) from DEFUN-ARGS."
  (block! nil
    (let! (name args (docstring declarations) body) (oo-defun-components arglist))
    (let! (args plist) (-split-with (-not #'keywordp) args))
    (let! (raw body) (-split-with (-compose #'keywordp #'car-safe) body))
    (for! ((elt &as k v) raw)
      (let! (k . v) elt)
      (pushing! alist (cons k (if (cdr v) v (car v)))))
    (let! new (if (and plist alist)
                  (map-merge 'plist plist alist)
                (map-into (or alist plist) 'plist)))
    (list name args (list docstring declarations) new body)))

;; (defmacro lambda! (args &rest body)
;;   "Wrapper around lambda that uses `block!'"
;;   (declare (indent defun))
;;   `(lambda (,@args) (block! nil ,@body)))

(defmacro defmacro! (&rest args)
  "Wrapper around `defmacro!'."
  (declare (indent defun) (doc-string 3))
  (-let [(name arglist metadata body) (oo-defun-components args)]
    `(cl-defmacro ,name ,arglist
       ,@(-non-nil metadata)
       (block! ,name
         (excluding! ,@(-remove #'oo-ampersand-symbol-p (-flatten arglist)))
         ,@body))))

(defmacro! defun! (&rest args)
  "Wrapper around `defun'."
  (declare (indent defun) (doc-string 3))
  (-let [(name arglist metadata body) (oo-defun-components args)]
    `(cl-defun ,name ,arglist
       ,@(-non-nil metadata)
       (block! ,name
         (excluding! ,@(-remove #'oo-ampersand-symbol-p (flatten-list arglist)))
         ,@body))))

(provide 'oo-base-block-definers)
