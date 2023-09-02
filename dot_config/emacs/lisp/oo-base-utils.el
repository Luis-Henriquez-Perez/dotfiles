(require 'dash)
(require 'mmt)

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
  (--each (cons fn fns)
    (oo-add-advice it :around '@silence)))

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

(defmacro updating! (place fn &rest args)
  "Set PLACE to the result of `(apply FN PLACE args)'."
  `(setf ,place (apply ,fn ,place ,args)))

(defmacro updating-it! (place expr)
  "Set PLACE to the value of EXPR.
PLACE is bound to `it' for the duration of EXPR."
  `(setf ,place (alet ,place ,expr)))

(defalias 'it-updating! 'updating-it!)

(defmacro appending! (place list)
  "Append LIST to the end of PLACE."
  (declare (indent 1))
  `(setf ,place (append ,place ,list)))

(defalias 'incrementing! 'cl-incf)

(defmacro collecting! (place item)
  "Affix ITEM to the end of PLACE."
  (declare (indent defun))
  `(setf ,place (append ,place (list ,item))))

(defalias 'snocing! 'collecting!)
(defalias 'affixing! 'collecting!)

(defmacro finding! (place expr)
  "Set VAR to the result of \(or VAR EXPR\)."
  `(setf ,place (or ,place ,expr)))

(defmacro counting! (place &optional x)
  "Increment PLACE by 1 if FORM is non-nil."
  `(cl-incf ,place (or ,x 1)))

(defmacro prepending! (place list)
  "Prepend LIST to beginning of PLACE."
  `(setf ,place (append ,list ,place)))

(defmacro maxing! (var form)
  "Set VAR to."
  (let ((form-var (gensym "form-var-")))
    `(let ((,form-var ,form))
       (setq ,var (if (> ,form-var ,var) ,form-var ,var)))))

(defalias 'decrementing! 'cl-decf)

(defmacro concating! (place string &optional sep)
  "Concat PLACE and STRING with optional separator, SEP."
  `(setf ,place (string-join (list ,place ,string) ,sep)))

(defmacro unioning! (place form &optional test)
  "Set PLACE to the union of PLACE and FORM."
  `(updating! ,place #'cl-union ,form :test ,(or test '#'equal)))

(defmacro updating! (place fn &rest args)
  "Set PLACE to the result of `(apply FN PLACE args)'."
  `(cl-callf2 funcall ,fn ,place ,@args))

(defmacro take! (pred place)
  (mmt-with-gensyms (taken)
    (mmt-once-only (pred)
      `(let (,taken)
	     (pcase ,pred
	       ((pred integerp)
	        (dotimes (_ ,pred)
	          (collecting! ,taken (pop ,place))))
	       (_
	        (while (and ,place (funcall ,pred (car ,place)))
	          (collecting! ,taken (pop ,place)))))
	     ,taken))))

(defmacro adjoining! (place item &optional test)
  "Set PLACE to ITEM consed onto the front of PLACE only if it's not already there."
  `(setf ,place (cl-adjoin ,item ,place :test ,(or test '#'equal))))

(defmacro pushing! (place form)
  "Same as `push', except the arguments are switched.
Designed to be used in `block!'."
  (declare (indent 1))
  `(push ,form ,place))

(defmacro adding-to-list! (var value &optional append compare-fn)
  `(set! ,var (add-to-list ',var ,value ,append ,compare-fn)))

(defun oo-candidate-features (fn)
  "Return a list of candidate features for FN.
FN is a function symbol.  Look in the load path for names that match features."
  (let ((candidates nil)
        (base nil)
        (fname (symbol-name fn)))
    (for! (path load-path)
      (setq base (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
      (when (s-prefix-p base fname)
        (collecting! candidates (intern base))))
    (seq-sort-by (-compose #'length #'symbol-name) #'> candidates)))

(defun oo-autoload-fn (fn &optional feature)
  "If FN is bound return FN, otherwise return an interactive lambda."
  (unless (and (symbolp fn) (fboundp fn))
    (alet `(lambda (&rest _)
	         (interactive)
	         (if-let (feature (or ',feature (car (oo-possible-features #',fn))))
		         (progn (fmakunbound #',fn)
			            (message "Autoloading %s from %s" #',fn feature)
			            (require feature)
			            (cond ((fboundp #',fn)
			                   (alet (symbol-function #',fn)
				                 (if (keymapp it)
				                     (set-transient-map it)
				                   (call-interactively #',fn))))
			                  (t
			                   (error "Not able to load %s from %s." #',fn feature))))
	           (error "Not able to find feature for %s." #',fn)))
      (fset fn it)))
  fn)

(defmacro catch-autoloads! (&rest body)
  "Try to autoload any unbound functions."
  (let ((err (gensym "error"))
        (void-fn (gensym "void-function")))
    `(condition-case ,err
         (progn ,@body)
       (void-function
        (let ((,void-fn (cadr ,err)))
          (mapc #'oo-try-load-feature (-select #'symbolp (flatten-tree ',body)))
          (progn ,@body))))))

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
