(require 'dash)
(require 'dash-functional)
(require 'subr-x)

;; This function is more for helping me write macros than for anything else.  It's
;; easy to wrap one form around a macro.  But this function automates the process of
;; wrapping =N= wrappers around a set of forms.
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

;; Being able to distinguish between a non-keyword symbol is useful enough to merit
;; its own function.
(defun oo-non-keyword-symbol-p (object)
  "Return t if OBJECT is a symbol but not a keyword."
  (declare (pure t) (side-effect-free t))
  (and (symbolp object) (not (keywordp object))))

(defun oo-args-to-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

;; Simple symbols, using this function can save me having to provide a string for
;; =format=.
(defun oo-args-to-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-args-to-string args)))

;; Sometimes I want to create a keyword by interning a string or a symbol.  This
;; commands saves me having to add the colon at the beginning before interning.
(defun oo-args-to-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-args-to-symbol ":" args))

(defun oo-funcall-silently (function &rest args)
  "Call function silently.
Don't produce any output to the *Messages* buffer when calling function."
  (shut-up (apply function args)))

(defun oo-symbols (regexp tree)
  "Return symbols that match REGEXP in TREE."
  (thread-last (flatten-list tree)
               (--select (and (symbolp it) (oo-symbol-match-p regexp it)))
               (-uniq)))

;; I find myself using the idiom ~(string-match-p regexp (symbol-name symbol))~ enough times.
(defsubst oo-symbol-match-p (regexp symbol)
  "Return non-nil if SYMBOL matches REGEXP."
  (string-match-p regexp (symbol-name symbol)))

;; For I want my advices, hooks and bindings to be dynamic in the sense that
;; they only take effect when it makes sense to do so.  For example [[id:20230801T175939.021467][this
;; headline]] I append =evil-append-line= to org-insert-heading-hook=.  But
;; obviously I don't want this to happen if for whatever reason I'm not in
;; =evil-mode=.
(defun oo-bound-and-true-fn (symbol)
  "Return a function that checks if SYMBOL is bound and non-nil."
  `(lambda () (bound-and-true-p ,symbol)))

;; This generic looping macro with predicate clauses inspired by =loopy=.  The
;; goal is to provide a unified syntax to cover all of my looping needs.  It
;; should "do-what-I-mean" whenever possible.

;; An implementation note: You might wonder why I check whether its a list if
;; =seq-doseq= already deals with the case that the sequence is a list.  The
;; reason is that =seq-doseq= is a wrapper around =seq-do= which works by
;; wrapping the iteration body in a lambda and calling it on every single
;; iteration.  The lambda adds an overhead.  So the answer is I do this for
;; better performance when dealing with lists.
(defmacro loop! (pred &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
This is the same as `dolist' except argument is MATCH-FORM.  match-form can be a
symbol as in `dolist', but.  LIST can be a sequence."
  (declare (indent 1))
  (pcase pred
    ((or `(repeat ,n) (and n (pred integerp)))
     (cl-once-only (n)
       `(if (integerp ,n)
	        (dotimes (_ ,n) ,@body)
	      (error "Wrong type argument integerp: %S" ,n))))
    (`(,(and match-form (pred sequencep)) ,list)
     (alet (make-symbol "var")
       `(for! (,it ,list)
	      (-let [,match-form ,it]
	        ,@body))))
    (`(,(and var (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
	           (dolist (,var ,list) ,@body))
	          ((sequencep ,list)
	           (seq-doseq (,var ,list) ,@body))
	          ((integerp ,list)
	           (loop! ,list ,@body))
	          (t
	           (error "Unknown list predicate: %S" ',pred)))))))

(defalias 'for! 'loop!)

;; The threading macro [[][->>]] can be particularly useful when you want to.  I've
;; often had cases in my code where I've.
(defmacro let-thread-last! (var &rest forms)
  "Bind VAR to the result of threading FORMS with `thread-last'."
  (declare (indent defun))
  `(-setq ,var (->> ,@forms)))

(defalias 'let->>! 'let-thread-last!)

(provide 'oo-base-utils)
