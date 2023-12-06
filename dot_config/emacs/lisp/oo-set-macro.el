;; **** set! - the "hakuna matata" for setting variables
;; :PROPERTIES:
;; :ID:       20230802T093702.542659
;; :END:
;; This macro is designed with the following goals in mind.
;; 1 - use one generic macro for most binding needs
;; 2 - log the variables I set and when they are being set
;; You'll get a warning when trying to bind a symbol that hasn't been defined yet.
;; So it's best to bind a package symbol only after the package has been loaded.
;; 3 - stop worrying about variables that haven't been bound
;; 4 - stop worrying about whether a variable is a custom variable or not
;; Some variables are custom variables.  Meaning they have some function that.
;; ***** define an alist to store unbound variables
;; :PROPERTIES:
;; :ID:       20230802T122637.504086
;; :END:
(defvar oo-unbound-symbol-alist nil
  "An alist mapping an unbound symbol to an expression.
This alist is checked by the hook `after-load-functions&set-bound-symbols' for
any symbols that are now bound.")
;; ***** define a "do-it-all" variable setter macro - set!
;; :PROPERTIES:
;; :ID:       20230801T060030.522883
;; :END:
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

(provide 'oo-set-macro)
