(require 'oo-base-block-definers)
(require 'oo-base-log)

;; I have been dissatisfied with using excessive =,= and =,@= syntax with backquotes for building a
;; form.  Often the body can get confusing, large, and difficult to understand at a glance.  For
;; example, look at [[][this version]] of the macro. This
;; function allows me to do such form building incrementally and with normal lisp syntax as opposed to
;; backquote syntax.  Compare [[][]].
(defun! oo--subst (tree substs delimiter)
  "Replace nodes in TREE that match DELIMITER with SUBSTS."
  (let! zipper (treepy-list-zip tree))
  (while (and substs (not (treepy-end-p zipper)))
    (when (equal (treepy-node zipper) delimiter)
      (setq zipper (treepy-replace zipper (pop substs))))
    (setq zipper (treepy-next zipper)))
  (treepy-root zipper))

;; This function is the "font-end" to [[][]].  I split it into two because I wanted two things.  One is I
;; wanted a function with a small number of argument to [[][format]].  But I also didn't want to forgoe
;; the potential uses of different "delimiters".  For now I used the symbol =$= but.  I specifically
;; use the dollar sign (=$=) as a delimiter because I almost never see it used in emacs or external
;; packages.  The one time I saw it was.
(defun oo-subst (tree &rest substs)
  (oo--subst tree substs '*))

;; Adding hooks here I strive for the following goals.
;; 1 - all hook should follow the naming convention of =hook&function=
;; For example, a function symbol =foo= added to =emacs-lisp-mode-hook= would result in
;; =emacs-lisp-mode-hook&foo=.
;; 2 - hooks should try to load hook function if it is not bound
;; Hooks provide the perfect opportunity to autoload features.
;; 3 - if the hook function raises an error, suppress it but log the error
;; If a function raises an error while running a hook, the hook "short circuits".
;; By which I mean the following hooks are not run.  So if you have functions =a=, =b=, =c=,
;; and =d= in =foo-mode-hook= and =b= raises an error when the hook is run then functions =c=
;; and =b= won't be called.
;; 4 - provide a way to add a hook that expires
;; This is a feature popularized by =DOOM= and honestly I think it's only relevant
;; for those who are really try-harding to have a very fast and performant Emacs
;; configuration (like me).
;; 5 - log the occurrence of the hook
;; Hooks provide a great opportunity for knowing what is happening in your Emacs
;; configuration because every mode has.  You easily note whether a hook that
;; should be called isn't.  And you can see any errors in hooks.
(defun! oo-add-hook (hook function &key name args depth append local when mode expire no-log)
  "Generate a function that behaves like FUNCTION and add it to HOOK. 
DEPTH and LOCAL are the same as in `add-hook'.  When EXPIRE is non-nil, each
the generated function will remove itself from SYMBOL after it is run once.  If
EXPIRE is a function it should take no arguments and return non-nil when the hook should be
removed.  If NO-LOG, don't log the hook.  If NO-AUTOLOAD don't try to autoload FUNCTION.  ARGS
is be a list of arguments that should be passed in to FUNCTION when called"
  (let! name (or name (oo-args-to-symbol hook '& (if (symbolp function) function (gensym "anonymous-hook-")))))
  (let! body `(apply (oo-log-error-fn (oo-autoload-function #',function)) args))
  (when expire
    (setq body (oo-subst `(prog1 * *) body `(when ,expire (remove-hook ',hook ',name)))))
  (unless no-log
    (setq body (oo-subst `(progn * *) `(lgr-info oo-lgr "Running hook `%s'" ',name) body)))
  (when when
    (setq body (oo-subst `(when (funcall #',when) *) body)))
  (when mode
    (setq body `(when (bound-and-true-p ,mode) ,body)))
  (when args
    (setq body (oo-subst `(progn (ignore args) (let ((args (append ',args args))) *)) body)))
  (fset name `(lambda (&rest args) ,body))
  (add-hook hook name (or depth append) local))

;; This is a convenience macro for creating hooks.  It is the counterpart to
;; =defadvice!=.  Like =defadvice!=, it allows me to define hooks in a concise,
;; easily-readable, and familiar way.
(defmacro! defhook! (&rest rest)
  "Define a hook and add it to SYMBOL and SYMBOLS.
DOCSTRING and BODY are the docstring and body (respectively) of the defined
hook.  Optionally, this macro takes key-value pairs passed in as either.

Optional keyword arguments:

- `:args' the arguments of the hook, `(&rest)' by default.
- `:depth' - same as `depth' in `add-hook'.
- `:append' - same as `:depth'.
- `:local' - same as `local' in `add-hook'.
- `:expire' - a function that returns non-nil when the advice should be removed.

(fn NAME (SYMBOL [SYMBOLS] [KEY VALUE]...) [DOCSTRING] [[KEY VALUE]...] [BODY])"
  (declare (indent defun) (doc-string 3))
  (let! (name symbols metadata plist body) (oo-destructure-defun-plus rest))
  (dolist (symbol symbols)
    (let! args (or (plist-get plist :args) '(&rest _)))
    (let! lambda `(lambda ,args ,@metadata (block! nil ,@body)))
    (let! fname (intern (format "%s&%s+" symbol name)))
    (collecting! forms `(oo-add-hook ',symbol #',lambda ,@plist :name ',fname)))
  (macroexp-progn forms))

(provide 'oo-base-hook-utils)
