(require 'log4e)

(defun! oo-advice-create (&rest args)
  "Return a function that advises SYMBOL at PLACE.
PROPS is the same as in `advice-add'.  When EXPIRE is non-nil, each function will
remove itself as advice after it is run once.  If EXPIRE is a function, call it
on the return value in order to determine whether to remove a function as advice."
  (let! (symbol place function (&plist :name :expire :props)) args)
  (let! name (or name (if (symbolp function) function (gensym "anonymous-advice-"))))
  (let! new-advice (oo-args-to-symbol symbol '@ name))
  (fset new-advice
	    `(lambda (&rest args)
	       ,@(when (symbolp function) `((oo-try-load-feature #',function)))
	       (aprog1 (apply #',function args)
	         (when (and ',expire (or (not (functionp ',expire)) (funcall ',expire it)))
	           (advice-remove ',symbol ',new-advice)))))
  (advice-add symbol place new-advice props)
  new-advice)

(cl-defun oo-add-advice (symbols place function &key expire props)
  "SYMBOLS, WHERE, FUNCTIONS, and PROPS correspond to the arguments for
advice-add.  Unlike advice-add, SYMBOLS and FUNCTIONS can be single items or
lists.  When EXPIRE is non-nil, each function will remove itself as advice
after it is run once.  If EXPIRE is a function, call it on the return value in
order to determine whether to remove a function as advice."
  (dolist (symbol (-list symbols))
    (oo-advice-create symbol place function :expire expire :props props)))

(defmacro! defadvice! (&rest args)
  "Define an advice for FUNCTION called FUNCTION@NAME+.
DOCSTRING and BODY are the docstring and body of the defined advice.  PLACE is
the same as in `add-function' except it is a symbol instead of a keyword.  FLAGS
is set of key value pairs.  Optionally,

Optional keyword arguments:
- `:args' - the arguments for the advice, \(&rest _) by default.
- `:props' - the same as in `advice-add'.
- `:expire' - a function that returns non-nil when the advice should be removed.

\(fn NAME (PLACE FUNCTION [OTHER-FUNCTIONS] [KEY VAL]...) [DOCSTRING] [interactive-form] [[KEY VAL]...]
BODY...)"
  (declare (indent defun) (doc-string 3))
  (let! (name (place . symbols) metadata plist body) (oo-destructure-defun-plus args))
  (dolist (symbol symbols)
    (let! lambda `(lambda ,(or (plist-get plist :args) '(&rest _)) ,@metadata ,@body))
    (let! kargs (map-delete (plist-put plist :name (macroexp-quote (oo-args-to-symbol name '+))) :args))
    (pushing! forms `(oo-advice-create ',symbol ,(oo-args-to-keyword place) ,lambda ,@kargs)))
  (macroexp-progn (nreverse forms)))

(provide 'oo-advice)
