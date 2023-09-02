(require 'dash)
(require 'mmt)
(require 'shut-up)

(defun oo-ampersand-symbol-p (obj)
  "Return non-nil of OBJ is an ampersand symbol.
An ampersand symbol is a symbol that starts with `&'."
  (and (symbolp obj) (string-match-p "\\`&" (symbol-name obj))))

(defsubst oo-sharp-quoted-p (obj)
  "Return non-nil if OBJ is sharp quoted."
  (equal (car-safe obj) 'function))

(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of lisp forms.  WRAPPER are a list of forms."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (-snoc wrapper forms)))
  forms)

(defun oo-non-keyword-symbol-p (object)
  "Return t if OBJECT is a symbol but not a keyword."
  (declare (pure t) (side-effect-free t))
  (and (symbolp object) (not (keywordp object))))

(defun oo-args-to-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defun oo-args-to-symbol (&rest args)
  "Return an interned symbol from args."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-args-to-string args)))

(defun oo-args-to-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-args-to-symbol ":" args))

(defun @silence (orig-fn &rest args)
  "Advice that silences ORIG-FN."
  (shut-up (apply orig-fn args)))

(defun oo-silence (fn &rest fns)
  "Add advice to FN and FNS that silences their output."
  (dolist (fun (cons fn fns))
    (oo-add-advice fun :around '@silence)))

(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (alet `(,regexp
	      (display-buffer-at-bottom)
	      (side bottom)
	      (slot 1)
	      (window-height 0.5)
	      (window-parameters ((no-other-window t))))
    (push it display-buffer-alist)))

(defmacro loop! (pred &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
This is the same as `dolist' except argument is MATCH-FORM.  match-form can be a
symbol as in `dolist', but.  LIST can be a sequence."
  (declare (indent 1))
  (pcase pred
    ;; ((pred oo-sharp-quoted-p)
    ;;  `(while (funcall ,pred) ,@body))
    ((or `(repeat ,n) (and n (pred integerp)))
     (mmt-once-only (n)
       `(if (integerp ,n)
	        (dotimes (_ ,n) ,@body)
	      (error "Wrong type argument integerp: %S" ,n))))
    (`(,(and match-form (pred sequencep)) ,list)
     (alet (make-symbol "var")
       `(for! (,it ,list)
	      (-let [,match-form ,it]
	        ,@body))))
    (`(,(and var (pred symbolp)) ,list)
     (mmt-once-only (list)
       `(cond ((listp ,list)
	           (dolist (,var ,list) ,@body))
	          ((sequencep ,list)
	           (seq-doseq (,var ,list) ,@body))
	          ((integerp ,list)
	           (loop! ,list ,@body))
	          (t
	           (error "Unknown list predicate: %S" ',pred)))))))

(defalias 'for! 'loop!)

(defmacro let-thread-last! (var &rest forms)
  "Bind VAR to the result of threading FORMS with `thread-last'."
  (declare (indent defun))
  `(-setq ,var (->> ,@forms)))

(defalias 'let->>! 'let-thread-last!)

(defun oo-tree-map-nodes (pred fun tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fun tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fun (car tree))
               (oo-tree-map-nodes pred fun (cdr tree))))
        (t
         tree)))

(provide 'oo-base-utils)
