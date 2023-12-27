(require 'oo-base-utils)
(require 'oo-base-block-definers)

;; There are two ways that I name an advice.  One is =SYMBOL@FUNCTION=.  The other is
;; =SYMBOL@A-NAME+=.  The former is when I generate an advice based on an existing
;; function.  While the latter is when I create a completely new advice.  The reason
;; I create this function is because I want to create dynamically named advices and
;; macros such as =defadvice!= are not good for this because their arguments are
;; unevaluated.
(defun! oo-add-advice (symbol place function &key name expire props when)
  "Return a function that advises SYMBOL at PLACE.
PROPS is the same as in `advice-add'.  When EXPIRE is non-nil, each function will
remove itself as advice after it is run once.  If EXPIRE is a function, call it
on the return value in order to determine whether to remove a function as advice."
  (let! name (or name (if (symbolp function) function (gensym "anonymous-advice-"))))
  (let! new-advice (oo-args-to-symbol symbol '@ name))
  (fset new-advice
        `(lambda (&rest args)
           (aprog1 (apply (oo-autoload-function #',function) args)
             (when (and ',expire (or (not (functionp ',expire)) (funcall ',expire it)))
               (advice-remove ',symbol ',new-advice)))))
  (advice-add symbol place new-advice props)
  new-advice)

;; This macro lets me define advices in a concise, declarative and easy to read
;; syntax.  Additionally, I can name the advices with a meaningful name that can be
;; distinguished from other advices.  Note that these advices are named in the
;; format.

;; Although this function is primarily for side-effects, it seems--looking at
;; =defun= and =defadvice=--that the convention is to return the name of the
;; function defined by the macro.  I wasn't sure what to do in this case because
;; this macro can potentially define more than one function.  I chose to have it
;; return a the name of the defined advice if only one is defined; and a list of
;; advices if multiple are defined.  I'm unsure if this is the best
;; solution--perhaps always just returning a list is simpler and more
;; consistent.  We'll see.

;; Note that I allow the macro to take key-value pairs as an alist just before the
;; body as well as a plist (like =cl-defun='s =&key=) in the arglist.  This is not a
;; new idea--some evil macros allow this.  The reason for it is that the arglist can
;; easily become really long; and it looks bad if we indent it.
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
    (pushing! forms `(oo-add-advice ',symbol ,(oo-args-to-keyword place) ,lambda ,@kargs)))
  (macroexp-progn (nreverse forms)))

(provide 'oo-base-advice-utils)
