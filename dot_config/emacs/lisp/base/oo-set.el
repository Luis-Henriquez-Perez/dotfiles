(require 'oo-base-definers)

(add-to-list 'log4e-log-level-alist '(setting . 3))
(log4e--def-level-logger "oo" "log-setting" 'setting)
(defalias 'oo-log-setting 'oo--log-advice)
(defun! oo-log-setting (var value)
  "Log a setting."
  (let! svalue (s-truncate 40 (format "%S" value)))
  (oo--log-setting "%s -> %s" var svalue))

(defvar oo-unbound-symbol-alist nil
  "An alist mapping an unbound symbol to an expression.
This alist is checked by the hook `after-load-functions&set-bound-symbols' for
any symbols that are now bound.")

(defmacro! set! (symbol value)
  "A \"do-it-all\" setter for configuring variables."
  (let! value-var (make-symbol "value"))
  `(if (not (boundp ',symbol))
       (push (cons ',symbol ',value) oo-unbound-symbol-alist)
     (let ((,value-var ,value))
       (oo-log-setting ',symbol ,value-var)
       (aif (get ',symbol 'custom-set)
	       (funcall it ',symbol ,value-var)
	     (setq ,symbol ,value-var)))))

(defun! after-load-functions&set-bound-symbols (&rest _)
  "Set symbols that have been bound to the result of their corresponding expr.
Check each symbol in `oo-unbound-symbol-alist', removing those that have already been
bound and setting them to the result of evaluating expr."
  (for! ((elt &as symbol . expr) oo-unbound-symbol-alist)
    (cond ((boundp symbol)
	       (pushing! exprs `(set! ,symbol ,expr)))
	      (t
	       (pushing! updated elt))))
  (setq oo-unbound-symbol-alist (nreverse updated))
  (when exprs (funcall `(lambda () ,@exprs))))

(add-hook 'after-load-functions #'after-load-functions&set-bound-symbols)

(provide 'oo-set)
