(require 'log4e)
(require 'pcase)
(require 'oo-modification-macros)
(require 'oo-base-utils)
(require 'oo-autoload)
(require 'oo-set)

(adding-to-list! log4e-log-level-alist '(hook . 3))
(log4e--def-level-logger "oo" "log-hook" 'hook)
(defun oo-log-hook (_ function &rest _)
  (oo--log-hook "%s" function))

(defun! oo-generate-hook-name (hook function args)
  "Return a hook name based on HOOK, FUNCTION and ARGS."
  (let! prefix hook)
  (alet (pcase function
	      ((pred listp)
	       (gensym "anonymous-hook-"))
	      ((guard (aand args (-all-p (-orfn #'stringp #'symbolp #'numberp) args)))
	       ;; Combine function and args into a name if it's reasonable to do so.
	       (string-join (mapcar #'oo-args-to-string (cons function args)) "-"))
	      (_
	       function))
    (intern (format "%s&%s" prefix it))))

(defun oo-add-hook (symbols function &rest arglist)
  "Add new hooks generated from FUNCTIONS to SYMBOLS.
Unlike `add-hook', SYMBOLS and FUNCTIONS can be single items or lists.  EXPIRE is
the same as in `oo-hook-create'.  When EXPIRE is non-nil, each
function will remove itself from the hook it is in after it is run once.  If
EXPIRE is a function, call it on the return value in order to determine
whether to remove a function from the hook."
  (dolist (symbol (-list symbols))
    (apply #'oo-create-hook symbol function arglist)))

;; (defalias 'oo-generate-hook 'oo-create-hook)
;; (defalias 'oo-gen-hook 'oo-create-hook)

(defmacro! defhook! (&rest rest)
  "Define a hook and add it to SYMBOL and SYMBOLS.
DOCSTRING and BODY are the docstring and body (respectively) of the defined
hook.  Optionally, this macro takes key-value pairs passed in as either.

Optional keyword arguments:

- `:args' the arguments of the hook, `\(&rest)' by default.
- `:depth' - same as `depth' in `add-hook'.
- `:append' - same as `:depth'.
- `:local' - same as `local' in `add-hook'.
- `:expire' - a function that returns non-nil when the advice should be removed.

\(fn NAME (SYMBOL [SYMBOLS] [KEY VALUE]...) [DOCSTRING] [[KEY VALUE]...] [BODY])"
  (declare (indent defun) (doc-string 3))
  (let! (name symbols metadata plist body) (oo-destructure-defun-plus rest))
  (dolist (symbol symbols)
    (let! args (or (plist-get plist :args) '(&rest _)))
    (let! lambda `(lambda ,args ,@metadata (block! nil ,@body)))
    (let! fname (intern (format "%s+" name)))
    (collecting! forms `(oo-add-hook ',symbol #',lambda ,@plist :name ',fname)))
  (macroexp-progn forms))

(defun! oo-create-hook (symbol fn &rest arglist)
  "Add a hook at SYMBOL that behaves like FUNCTION.
DEPTH and LOCAL are the same as in `add-hook'.  When EXPIRE is non-nil, each
the new hook will remove itself from SYMBOL after it is run once.  If
EXPIRE is a function, call it on the return value of the new hook (which is will
be the same as the return value for FUNCTION) in order to determine whether to
remove a function from the hook."
  (let! args (take! (-not #'keywordp) arglist))
  (let! (&plist :append :depth :local :expire) arglist)
  (let! hook (oo-generate-hook-name symbol fn args))
  (fset hook
	    `(lambda (&rest _)
	       ,@(when (symbolp fn) `((oo-try-load-feature #',fn)))
	       (oo-log-hook nil ',hook)
	       (prog1 (condition-case-unless-debug err
		              (apply #',fn ',args)
		            (error (oo-log-error "hook `%s' failed because %s -> %s" ',hook (car err) (cdr err))))
	         (when ,expire (remove-hook ',symbol ',hook)))))
  (add-hook symbol hook (or depth append) local))

(provide 'oo-base-hook)
