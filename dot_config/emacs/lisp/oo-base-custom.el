(require 'oo-base-lib)
;;;; custom 
(defvar oo-old-values-alist nil
  "An alist that contains symbols I want to \"reset\" after startup.
Each element is of the form (SYMBOL OLD-VALUE SETTER).  SYMBOL is the
symbol whose value should be reset to OLD-VALUE by calling SETTER with
SYMBOL and OLD-VALUE.")

(defmacro startup-set! (symbol value &optional setter)
  "Set VAR to VALUE using SETTER.
At the end of `emacs-statup-hook' set VAR back to its original VALUE."
  `(progn (setf (alist-get ',symbol oo-old-values-alist) (list ,symbol #',setter))
          (funcall (or #',setter #'set) ',symbol ,value)))
;; ;;;; advices
;; ;; There are two ways that I name an advice.  One is =SYMBOL@FUNCTION=.  The other is
;; ;; =SYMBOL@A-NAME+=.  The former is when I generate an advice based on an existing
;; ;; function.  While the latter is when I create a completely new advice.  The reason
;; ;; I create this function is because I want to create dynamically named advices and
;; ;; macros such as =defadvice!= are not good for this because their arguments are
;; ;; unevaluated.
;; (defun! oo-add-advice (symbol place function &key name expire props when)
;;   "Return a function that advises SYMBOL at PLACE.
;; PROPS is the same as in `advice-add'.  When EXPIRE is non-nil, each function will
;; remove itself as advice after it is run once.  If EXPIRE is a function, call it
;; on the return value in order to determine whether to remove a function as advice."
;;   (set! name (or name (if (symbolp function) function (gensym "anonymous-advice-"))))
;;   (set! new-advice (oo-args-to-symbol symbol '@ name))
;;   (fset new-advice
;;         `(lambda (&rest args)
;;            (aprog1 (apply (oo-autoload-function #',function) args)
;;              (when (and ',expire (or (not (functionp ',expire)) (funcall ',expire it)))
;;                (advice-remove ',symbol ',new-advice)))))
;;   (advice-add symbol place new-advice props)
;;   new-advice)

;; ;; This macro lets me define advices in a concise, declarative and easy to read
;; ;; syntax.  Additionally, I can name the advices with a meaningful name that can be
;; ;; distinguished from other advices.  Note that these advices are named in the
;; ;; format.

;; ;; Although this function is primarily for side-effects, it seems--looking at
;; ;; =defun= and =defadvice=--that the convention is to return the name of the
;; ;; function defined by the macro.  I wasn't sure what to do in this case because
;; ;; this macro can potentially define more than one function.  I chose to have it
;; ;; return a the name of the defined advice if only one is defined; and a list of
;; ;; advices if multiple are defined.  I'm unsure if this is the best
;; ;; solution--perhaps always just returning a list is simpler and more
;; ;; consistent.  We'll see.

;; ;; Note that I allow the macro to take key-value pairs as an alist just before the
;; ;; body as well as a plist (like =cl-defun='s =&key=) in the arglist.  This is not a
;; ;; new idea--some evil macros allow this.  The reason for it is that the arglist can
;; ;; easily become really long; and it looks bad if we indent it.
;; (defmacro! defadvice! (&rest args)
;;   "Define an advice for FUNCTION called FUNCTION@NAME+.
;; DOCSTRING and BODY are the docstring and body of the defined advice.  PLACE is
;; the same as in `add-function' except it is a symbol instead of a keyword.  FLAGS
;; is set of key value pairs.  Optionally,

;; Optional keyword arguments:
;; - `:args' - the arguments for the advice, \(&rest _) by default.
;; - `:props' - the same as in `advice-add'.
;; - `:expire' - a function that returns non-nil when the advice should be removed.

;; \(fn NAME (PLACE FUNCTION [OTHER-FUNCTIONS] [KEY VAL]...) [DOCSTRING] [interactive-form] [[KEY VAL]...]
;; BODY...)"
;;   (declare (indent defun) (doc-string 3))
;;   (let! (name (place . symbols) metadata plist body) (oo-destructure-defun-plus args))
;;   (dolist (symbol symbols)
;;     (let! lambda `(lambda ,(or (plist-get plist :args) '(&rest _)) ,@metadata ,@body))
;;     (let! kargs (map-delete (plist-put plist :name (macroexp-quote (oo-args-to-symbol name '+))) :args))
;;     (pushing! forms `(oo-add-advice ',symbol ,(oo-args-to-keyword place) ,lambda ,@kargs)))
;;   (macroexp-progn (nreverse forms)))
;; ;;;; hooks
;; ;; I have been dissatisfied with using excessive =,= and =,@= syntax with backquotes for building a
;; ;; form.  Often the body can get confusing, large, and difficult to understand at a glance.  For
;; ;; example, look at [[][this version]] of the macro. This
;; ;; function allows me to do such form building incrementally and with normal lisp syntax as opposed to
;; ;; backquote syntax.  Compare [[][]].
;; (defun! oo--subst (tree substs delimiter)
;;   "Replace nodes in TREE that match DELIMITER with SUBSTS."
;;   (let! zipper (treepy-list-zip tree))
;;   (while (and substs (not (treepy-end-p zipper)))
;;     (when (equal (treepy-node zipper) delimiter)
;;       (setq zipper (treepy-replace zipper (pop substs))))
;;     (setq zipper (treepy-next zipper)))
;;   (treepy-root zipper))

;; ;; This function is the "font-end" to [[][]].  I split it into two because I wanted two things.  One is I
;; ;; wanted a function with a small number of argument to [[][format]].  But I also didn't want to forgoe
;; ;; the potential uses of different "delimiters".  For now I used the symbol =$= but.  I specifically
;; ;; use the dollar sign (=$=) as a delimiter because I almost never see it used in emacs or external
;; ;; packages.  The one time I saw it was.
;; (defun oo-subst (tree &rest substs)
;;   (oo--subst tree substs '*))

;; ;; Adding hooks here I strive for the following goals.
;; ;; 1 - all hook should follow the naming convention of =hook&function=
;; ;; For example, a function symbol =foo= added to =emacs-lisp-mode-hook= would result in
;; ;; =emacs-lisp-mode-hook&foo=.
;; ;; 2 - hooks should try to load hook function if it is not bound
;; ;; Hooks provide the perfect opportunity to autoload features.
;; ;; 3 - if the hook function raises an error, suppress it but log the error
;; ;; If a function raises an error while running a hook, the hook "short circuits".
;; ;; By which I mean the following hooks are not run.  So if you have functions =a=, =b=, =c=,
;; ;; and =d= in =foo-mode-hook= and =b= raises an error when the hook is run then functions =c=
;; ;; and =b= won't be called.
;; ;; 4 - provide a way to add a hook that expires
;; ;; This is a feature popularized by =DOOM= and honestly I think it's only relevant
;; ;; for those who are really try-harding to have a very fast and performant Emacs
;; ;; configuration (like me).
;; ;; 5 - log the occurrence of the hook
;; ;; Hooks provide a great opportunity for knowing what is happening in your Emacs
;; ;; configuration because every mode has.  You easily note whether a hook that
;; ;; should be called isn't.  And you can see any errors in hooks.
;; (defun! oo-add-hook (hook function &key name args depth append local when mode expire no-log)
;;   "Generate a function that behaves like FUNCTION and add it to HOOK.
;; DEPTH and LOCAL are the same as in `add-hook'.  When EXPIRE is non-nil, each
;; the generated function will remove itself from SYMBOL after it is run once.  If
;; EXPIRE is a function it should take no arguments and return non-nil when the hook should be
;; removed.  If NO-LOG, don't log the hook.  If NO-AUTOLOAD don't try to autoload FUNCTION.  ARGS
;; is be a list of arguments that should be passed in to FUNCTION when called"
;;   (let! name (or name (oo-args-to-symbol hook '& (if (symbolp function) function (gensym "anonymous-hook-")))))
;;   (let! body `(apply (oo-log-error-fn (oo-autoload-function #',function)) args))
;;   (when expire
;;     (setq body (oo-subst `(prog1 * *) body `(when ,expire (remove-hook ',hook ',name)))))
;;   (unless no-log
;;     (setq body (oo-subst `(progn * *) `(lgr-info oo-lgr "Running hook `%s'" ',name) body)))
;;   (when when
;;     (setq body (oo-subst `(when (funcall #',when) *) body)))
;;   (when mode
;;     (setq body `(when (bound-and-true-p ,mode) ,body)))
;;   (when args
;;     (setq body (oo-subst `(progn (ignore args) (let ((args (append ',args args))) *)) body)))
;;   (fset name `(lambda (&rest args) ,body))
;;   (add-hook hook name (or depth append) local))

;; ;; This is a convenience macro for creating hooks.  It is the counterpart to
;; ;; =defadvice!=.  Like =defadvice!=, it allows me to define hooks in a concise,
;; ;; easily-readable, and familiar way.
;; (defmacro! defhook! (&rest rest)
;;   "Define a hook and add it to SYMBOL and SYMBOLS.
;; DOCSTRING and BODY are the docstring and body (respectively) of the defined
;; hook.  Optionally, this macro takes key-value pairs passed in as either.

;; Optional keyword arguments:

;; - `:args' the arguments of the hook, `(&rest)' by default.
;; - `:depth' - same as `depth' in `add-hook'.
;; - `:append' - same as `:depth'.
;; - `:local' - same as `local' in `add-hook'.
;; - `:expire' - a function that returns non-nil when the advice should be removed.

;; (fn NAME (SYMBOL [SYMBOLS] [KEY VALUE]...) [DOCSTRING] [[KEY VALUE]...] [BODY])"
;;   (declare (indent defun) (doc-string 3))
;;   (let! (name symbols metadata plist body) (oo-destructure-defun-plus rest))
;;   (dolist (symbol symbols)
;;     (let! args (or (plist-get plist :args) '(&rest _)))
;;     (let! lambda `(lambda ,args ,@metadata (block! nil ,@body)))
;;     (let! fname (intern (format "%s&%s+" symbol name)))
;;     (collecting! forms `(oo-add-hook ',symbol #',lambda ,@plist :name ',fname)))
;;   (macroexp-progn forms))
;; ;;;; package deferment
;; ;;;; oo-call-after-load
;; (defvar oo-unbound-symbol-alist nil
;;   "An alist mapping an unbound symbol to an expression.
;; This alist is checked by the hook `after-load-functions&set-bound-symbols' for
;; any symbols that are now bound.")

;; ;; I'll note that I push all the forms into a list and evaluate them all in the
;; ;; body of one lambda as opposed to evaluating one lambda per form.  This is
;; ;; important because lambda calls have an overhead that adds up.  It is far less
;; ;; costly to invoke one lambda over N lambdas.
;; (defun! after-load-functions&set-bound-symbols (&rest _)
;;   "Set symbols that have been bound to the result of their corresponding expr.
;; Check each symbol in `oo-unbound-symbol-alist', removing those that have already been
;; bound and setting them to the result of evaluating expr."
;;   (for! ((elt &as symbol . expr) oo-unbound-symbol-alist)
;;     (cond ((boundp symbol)
;;            (pushing! exprs `(set! ,symbol ,expr)))
;;           (t
;;            (pushing! updated elt))))
;;   (setq oo-unbound-symbol-alist (nreverse updated))
;;   (when exprs (funcall `(lambda () ,@exprs))))

;; (add-hook 'after-load-functions #'after-load-functions&set-bound-symbols)

;; ;; (defun! oo-try-load-feature (fn)
;; ;;   "Try to load feature that could contain FN."
;; ;;   (unless (fboundp fn)
;; ;;     (dolist (feature (oo-candidate-features fn))
;; ;;       (require feature)
;; ;;       (when (fboundp fn)
;; ;;         (return! feature)))))

;; (defun oo--call-after-load (condition fn &rest args)
;;   "Call FN with ARGS after CONDITION is met.
;; For what CONDITION is see `oo-call-after-load'."
;;   (pcase condition
;;     (`(:or . ,conditions)
;;      (--each conditions (apply #'oo--call-after-load it fn args)))
;;     (`(:and . ,conditions)
;;      (apply #'oo--call-after-load conditions fn args))
;;     ((or (pred null) (and (pred symbolp) (pred featurep)))
;;      (apply fn args))
;;     (`(,condition . ,conditions)
;;      (apply #'oo--call-after-load condition #'oo--call-after-load conditions fn args))
;;     ((and feature (pred symbolp))
;;      (eval-after-load feature `(funcall #',fn ,@(mapcar #'macroexp-quote args))))
;;     (_
;;      (error "invalid condition `%S'" condition))))

;; ;; An error in a =eval-after-load= form interferes with any subsequent
;; ;; =eval-after-load= forms.  Furthermore, if features depend on the feature you're
;; ;; trying to load then the error interferes with loading them (its similar to
;; ;; having an error in a hook).  This is why I ignore errors here (eventually I don't
;; ;; want to completely ignore them, but for now this is what I do).

;; ;; Admittedly, I didn't encounter any problems with this happening using the
;; ;; built-in =eval-after-load= because most if not all of the forms I have written
;; ;; are not dangerous to re-evaluate.  I know that when I use =eval-after-load= I
;; ;; mean to have forms evaluated only once.  I want to be formal about the matter.

;; ;; When something goes wrong I don't want to wonder what's going on.  I want to know
;; ;; what is happening and when.  To this end, I ensure the function logs to the log
;; ;; buffer.
;; (defun! oo-call-after-load-fn (fn args)
;;   "Return function to be used for `oo-call-after-load' based on FN.
;; The returned function calls FN with args ignoring any resulting errors.
;; Additionally, it does nothing and returns nil on any calls after its first
;; call."
;;   (let! fname (gensym "eval-after-load-fn-"))
;;   (let! closure (eval `(let ((first-call-p t))
;;                          (lambda (&rest _)
;;                            (when first-call-p
;;                              (setq first-call-p nil)
;;                              (lgr-info oo-lgr (s-truncate 50 (format "Calling %s with %S" ',fn ',args)))
;;                              (ignore-errors (apply #',fn ',args)))))
;;                       t))
;;   (fset fname closure)
;;   fname)

;; ;; This macro is designed with the following goals in mind.
;; ;; 1 - use one generic macro for most binding needs
;; ;; 2 - log the variables I set and when they are being set
;; ;; You'll get a warning when trying to bind a symbol that hasn't been defined yet.
;; ;; So it's best to bind a package symbol only after the package has been loaded.
;; ;; 3 - stop worrying about variables that haven't been bound
;; ;; 4 - stop worrying about whether a variable is a custom variable or not
;; ;; Some variables are custom variables.  Meaning they have some function that.
;; (defun oo-call-after-load (condition fn &rest args)
;;   "Call FN with ARGS after CONDITION resolves.
;; CONDITION can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
;; either `:or' or `:and' and whose CDR is a list of CONDITIONS.  If CONDITION is a
;; feature, call FN with ARGS if feature has already been provided; otherwise,
;; behave similarly to `eval-after-load'.  If CONDITION is a list of
;; CONDITIONS, call FN with ARGS only after all CONDITIONS have been met.  If
;; CONDITION is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
;; If CONDITION is a list whose CAR is `:or', call FN with ARGS after any of
;; CONDITIONS in (CDR CONDITION) is met."
;;   (oo--call-after-load condition (oo-call-after-load-fn fn args)))

;; ;; This is a convenince macro for =oo-call-after-load=.  The latter is concise when
;; ;; there is an existing function and possibly arguments that I can add.  However,
;; ;; =after!= is more convenient the case when I want a custom body and I don't want
;; ;; to define a function.
;; (defmacro! after! (condition &rest body)
;;   "Eval BODY after CONDITION is met.
;; For what CONDITION is see `oo-call-after-load'."
;;   (declare (indent 1))
;;   `(oo-call-after-load ',condition (lambda () ,@body)))

;; ;; I want to encourage named after blocks.  The name helps provide a
;; ;; description of what I'm intending to accomplish with said after
;; ;; block.  Although I don't have to define a named function to evaluate
;; ;; the code of the after block, I do for two reasons.  One is that I more
;; ;; easily ensure that the after block forms are run only once.  The other
;; ;; is that I can rerun or examine a given `defafter!` form when
;; ;; debugging.
;; (defmacro! defafter! (&rest args)
;;   "A wrapper around `after!' With same syntax as `defun'."
;;   (declare (indent defun))
;;   (let! (_ args _ body) (oo-defun-components args))
;;   `(after! ,args ,@body))
;; ;;;; cset!
;; (defmacro! cset! (symbol value)
;;   "A \"do-it-all\" setter for configuring variables."
;;   (let! value-var (make-symbol "value"))
;;   `(if (not (boundp ',symbol))
;;        (push (cons ',symbol ',value) oo-unbound-symbol-alist)
;;      (let ((,value-var ,value))
;;        (message "Set %s to %S" ',symbol ,value-var)
;;        (aif! (get ',symbol 'custom-set)
;;              (funcall it ',symbol ,value-var)
;;              (with-no-warnings (setq ,symbol ,value-var))))))
;; ;;;; autoloading
;; ;; ***** guess possible features from a function symbol
;; ;; With the [[][autoload]] function elispers usually provide.  however, it is very
;; ;; possible to.  There are a few packages that have abnormally named functions, but
;; ;; these are by and large uncommon.
;; ;; #+begin_src emacs-lisp
;; (defun oo-candidate-features (fn)
;;   "Return a list of candidate features for FN.
;; FN is a function symbol.  Look in the load path for names that match features."
;;   (let ((candidates nil)
;;         (base nil)
;;         (fname (symbol-name fn)))
;;     (for! (path load-path)
;;       (setq base (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
;;       (when (s-prefix-p base fname)
;;         (collecting! candidates (intern base))))
;;     (seq-sort-by (-compose #'length #'symbol-name) #'> candidates)))

;; ;; generating my own custom autoload function
;; ;; I'd like to avoid relying on autoloads from an autoload file.  Binding keys
;; ;; provides a distinct opportunity to autoload commands considering we already name
;; ;; the commands in the binding.
;; (defun oo-autoload-fn (fn &optional feature)
;;   "If FN is bound return FN, otherwise return an interactive lambda."
;;   (unless (and (symbolp fn) (fboundp fn))
;;     (alet `(lambda (&rest _)
;;              (interactive)
;;              (if-let (feature (or ',feature (car (oo-candidate-features #',fn))))
;;                  (progn (fmakunbound #',fn)
;;                         (message "Autoloading %s from %s" #',fn feature)
;;                         (require feature)
;;                         (cond ((fboundp #',fn)
;;                                (alet (symbol-function #',fn)
;;                                  (if (keymapp it)
;;                                      (set-transient-map it)
;;                                    (call-interactively #',fn))))
;;                               (t
;;                                (error "Not able to load %s from %s." #',fn feature))))
;;                (error "Not able to find feature for %s." #',fn)))
;;       (fset fn it)))
;;   fn)

(provide 'oo-base-custom)
