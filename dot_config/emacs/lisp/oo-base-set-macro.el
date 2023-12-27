(require 'lgr)
(require 'oo-base-log)
(require 'anaphora)
;; This file also uses the variable =oo-unbound-symbol-alist= which is defined
;; in =oo-call-after-load=.
(require 'oo-call-after-load)

(defmacro! set! (symbol value)
  "A \"do-it-all\" setter for configuring variables."
  (let! value-var (make-symbol "value"))
  `(if (not (boundp ',symbol))
       (push (cons ',symbol ',value) oo-unbound-symbol-alist)
     (let ((,value-var ,value))
       (lgr-info oo-lgr "Set %s to %S" ',symbol ,value-var)
       (aif (get ',symbol 'custom-set)
           (funcall it ',symbol ,value-var)
         (setq ,symbol ,value-var)))))

(provide 'oo-base-set-macro)
