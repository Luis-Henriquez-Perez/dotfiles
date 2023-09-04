(require 'oo-base-utils)
(require 'oo-modification-macros)
(require 'oo-base-definers)
(require 'oo-set)

(adding-to-list! log4e-log-level-alist '(after-load . 3))
(log4e--def-level-logger "oo" "log-after-load" 'after-load)
(defalias 'oo-log-after-load 'oo--log-after-load)
(defun oo-log-after-load (fn args)
  "Log an `after-load!' form."
  (cond ((symbolp fn)
	     (oo--log-after-load (s-truncate 70 (format "%S" (cons fn args)))))
	    (t
	     (oo--log-after-load "(anonymous-after-block)"))))

(defun oo--call-after-load (condition fn &rest args)
  "Call FN with ARGS after CONDITION is met.
For what CONDITION is see `oo-call-after-load'."
  (pcase condition
    (`(:or . ,conditions)
     (--each conditions (apply #'oo--call-after-load it fn args)))
    (`(:and . ,conditions)
     (apply #'oo--call-after-load conditions fn args))
    ((or (pred null) (and (pred symbolp) (pred featurep)))
     (apply fn args))
    (`(,condition . ,conditions)
     (apply #'oo--call-after-load condition #'oo--call-after-load conditions fn args))
    ((and feature (pred symbolp))
     (eval-after-load feature `(funcall #',fn ,@(mapcar #'macroexp-quote args))))
    (_
     (error "invalid condition `%S'" condition))))

(defun! oo-call-after-load-fn (fn args)
  "Return function to be used for `oo-call-after-load' based on FN.
The returned function calls FN with args ignoring any resulting errors.
Additionally, it does nothing and returns nil on any calls after its first call.
Finally, logs into the log buffer using `oo-log-after-load'."
  (let! fname (gensym "eval-after-load-fn-"))
  (let! closure (eval `(let ((first-call-p t))
			             (lambda (&rest _)
			               (when first-call-p
			                 (setq first-call-p nil)
			                 (oo-log-after-load #',fn ',args)
			                 (ignore-errors (apply #',fn ',args)))))
		              t))
  (fset fname closure)
  fname)

(defun oo-call-after-load (condition fn &rest args)
  "Call FN with ARGS after CONDITION resolves.
CONDITION can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of CONDITIONS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-after-load'.  If CONDITION is a list of
CONDITIONS, call FN with ARGS only after all CONDITIONS have been met.  If
CONDITION is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If CONDITION is a list whose CAR is `:or', call FN with ARGS after any of
CONDITIONS in (CDR CONDITION) is met."
  (oo--call-after-load condition (oo-call-after-load-fn fn args)))

(defmacro! after! (condition &rest body)
  "Eval BODY after CONDITION is met.
For what CONDITION is see `oo-call-after-load'."
  (declare (indent 1))
  `(oo-call-after-load ',condition (lambda () ,@body)))

(defmacro! defafter! (&rest args)
  "A wrapper around `after!' With same syntax as `defun'."
  (declare (indent defun))
  (let! (_ args _ body) (oo-defun-components args))
  `(after! ,args ,@body))

(provide 'oo-after-load)
