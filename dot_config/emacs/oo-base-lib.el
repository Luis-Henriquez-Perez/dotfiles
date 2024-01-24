;;; Base Library
;; This library should not require an external dependencies.  The main reason is
;; so that it can be loaded from scripts without having to think about package
;; management.
;; This is an initiative to remove external dependencies from the base functions
;; and macros that I use for my configuration.  Let me explain my reasoning.
;; First, I do not think I generally needed the convinience functions of dash,
;; f, and s.  With just a bit more work, I could do without them.  But also, I
;; want this base library to be a separate file that I can load for any elisp
;; scripts I write.  Relying on external libraries when loading scripts can be
;; complicated because you have to be sure you're packages are all set; if
;; they're not your script could take much longer if it tries to install the
;; packages before running its code, or it could even fail.  And it is painful
;; to write scripts without the functions and macros I painstakingly wrote to
;; help me.  Another advantage is I can use my library of helpers anywhere even
;; for =oo-install-packages-with-package-dot-el.el=.

;;;; Require built-in packages
(require 'pcase)
(require 'subr-x)
(require 'cl-lib)
;;;; aliases
;; Make symbols with consistent naming conventions to elisp predicate functions.
(defalias 'oo-all-p 'cl-every)
(defalias 'oo-every-p 'cl-every)
(defalias 'oo-none-p 'cl-notany)
(defalias 'oo-some-p 'cl-notevery)
;;;; helpers
(defun oo-cons-cell-p (obj)
  "Return t only if OBJ is a cons-cell."
  (declare (pure t) (side-effect-free t))
  (and (consp obj)
       (not (listp (cdr obj)))))

(defun oo-proper-list-p (obj)
  "Return t only if OBJ is a proper list."
  (declare (pure t) (side-effect-free t))
  (and (listp obj)
       (or (null obj)
           (null (cdr-safe (last obj))))))

(defun oo-improper-list-p (obj)
  "Return t only if OBJ is not a proper list"
  (declare (pure t) (side-effect-free t))
  (not (oo-proper-list-p obj)))

(defun oo-snoc (list elt)
  "Append ELT to the end of LIST."
  (declare (pure t) (side-effect-free t))
  (append list (list elt)))

(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of lisp forms.  WRAPPER are a list of forms."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (oo-snoc wrapper forms)))
  forms)

;; Being able to distinguish between a non-keyword symbol is useful enough to merit
;; its own function.
;; (defun oo-non-keyword-symbol-p (object)
;;   "Return t if OBJECT is a symbol but not a keyword."
;;   (declare (pure t) (side-effect-free t))
;;   (and (symbolp object) (not (keywordp object))))

;; (defun oo-args-to-string (&rest args)
;;   "Return ARGS as a string."
;;   (declare (pure t) (side-effect-free t))
;;   (with-output-to-string (mapc #'princ args)))

;; Simple symbols, using this function can save me having to provide a string for
;; =format=.
;; (defun oo-args-to-symbol (&rest args)
;;   "Return an interned symbol from ARGS."
;;   (declare (pure t) (side-effect-free t))
;;   (intern (apply #'oo-args-to-string args)))
;;;; function transformers 
;; (defun oo-notfn (fn)
;;   (lambda (&rest args) (not (apply fn args))))

;; (defun oo-andfn (fn &rest fns)
;;   )
;;;; anaphoric macros
;; I used the [[][anaphora]] package for these macros, but in reality they are
;; trivial to write on my own.
(defmacro alet! (expr &rest body)
  "Like `let', but the result of evaluating FORM is bound to `it'.
FORM and BODY are otherwise as documented for `let'."
  (declare (indent 1))
  `(let ((it ,expr))
     ,@body))

(defmacro aif! (expr then &rest else)
  "Like `if', but the result of evaluating COND is bound to `it'.
The variable `it' is available within THEN and ELSE.
COND, THEN, and ELSE are otherwise as documented for `if'."
  (declare (indent 2))
  `(alet! ,expr (if it ,then ,@else)))

(defmacro awhen! (cond &rest body)
  "Like `when', but the result of evaluating COND is bound to `it'.
The variable `it' is available within BODY.
COND and BODY are otherwise as documented for `when'."
  (declare (indent 1))
  `(alet! ,cond (when it ,@body)))

;; For brevity I do not want to add the =!= for this macro.
(defmacro -- (&rest body)
  `(lambda (it) ,@body))
;;;; looping
(defmacro for! (pred &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
This is the same as `dolist' except argument is MATCH-FORM.  match-form can be a
symbol as in `dolist', but.  LIST can be a sequence."
  (declare (indent 1))
  (pcase pred
    ((or `(repeat ,n) (and n (pred integerp)))
     `(dotimes (_ ,n) ,@body))
    (`(,(and match-form (pred sequencep)) ,list)
     (cl-with-gensyms (elt)
       `(for! (,elt ,list)
          (seq-let ((,match-form ,elt))
              ,@body))))
    (`(,(and elt (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
               (dolist (,elt ,list) ,@body))
              ((sequencep ,list)
               (seq-doseq (,elt ,list) ,@body))
              ((integerp ,list)
               (dotimes (,elt ,list) ,@body))
              (t
               (error "Unknown list predicate: %S" ',pred)))))))
;; ;;;; setting
;; (cl-defmacro appending! (place list &key (setter 'setf))
;;   "Append LIST to the end of PLACE.
;; SETTER is the symbol of the macro or function used to do the setting."
;;   `(,setter ,place (append ,place ,list)))

;; ;; Important to note that this macro is not as efficient as pushing because it's adding to the end
;; ;; of the list.  So this macro should be used only in non-performance-intensive code.  In
;; ;; performance-intensive code we need the =push-nreverse= idiom.
;; (cl-defmacro collecting! (place item &key (setter 'setf))
;;   "Affix ITEM to the end of PLACE.
;; SETTER is the same as in `appending!'."
;;   `(,setter ,place (oo-snoc ,place ,item)))

;; (defalias 'snocing! 'collecting!)
;; (defalias 'affixing! 'collecting!)

;; ;; You might be wondering why I didn't create a macro with a setter for these.
;; ;; Well I haven't had a case yet when I've wanted to increment or decrement the
;; ;; value symbol used in customization.
;; (defalias 'incrementing! 'cl-incf)
;; (defalias 'counting! 'cl-incf)
;; (defalias 'decrementing! 'cl-decf)

;; (cl-defmacro prepending! (place list &key (setter 'setf))
;;   "Prepend LIST to beginning of PLACE.
;; SETTER is the same as in `appending!'."
;;   `(,setter ,place (append ,list ,place)))

;; (cl-defmacro maxing! (place form &key (setter 'setf) (comparator '>))
;;   "Set PLACE to the greater of PLACE and FORM.
;; SETTER is the same as in `appending!'."
;;   (cl-with-gensyms (value1 value2)
;;     `(,setter ,place (let ((,value1 ,form)
;;                            (,value2 ,place))
;;                        (if (,comparator ,value1 ,value2) ,value1 ,value2)))))

;; (cl-defmacro minning! (place form &key (setter 'setf) (comparator '<))
;;   "Set PLACE to the lesser of PLACE and FORM.
;; SETTER is the same as in `appending!'. COMPARATOR is the same as in `maxing!'."
;;   `(maxing! ,place ,form :setter ,setter :comparator ,comparator))

;; (cl-defmacro concating! (place string &key (setter 'setf) separator)
;;   "Concat PLACE and STRING with SEPARATOR.
;; SETTER is the same as in `appending!'"
;;   `(,setter ,place (string-join (list ,place ,string) ,separator)))

;; (cl-defmacro adjoining! (place item &key test test-not key (setter 'setf))
;;   "Set PLACE to the value of `cl-adjoin'.
;; SETTER is the same as in `appending!'."
;;   `(,setter ,place (cl-adjoin ,item ,place :test ,test :test-not ,test-not :key ,key)))

;; ;; I know =push= already exists.  But I want a variant of push that can be used
;; ;; with the =block!= macro.
;; (cl-defmacro pushing! (place item &key (setter 'setf))
;;   "Cons ITEM to PLACE.
;; SETTER is the same as in `appending!'."
;;   `(,setter ,place (cons ,item ,place)))

;; ;; To configure variables I don't use the standard =setq=--at least not directly.
;; ;; Instead, I use =set!=.  Adjoining is one of the most common operations done to
;; ;; lisp symbols when configuring Emacs.

;; ;; Something I was always confused about was why adjoin instead of just using
;; ;; =push=.  The latter is more performant; however I don't think that's.  The best reason I could
;; ;; think of is that sometimes you want to re-evaluate parts of your configuration
;; ;; and in that case it is more convenient to have =adjoin= over =push=.
;; (cl-defmacro unioning! (place list &key test test-not key (setter 'setf))
;;   "Set PLACE to the union of PLACE and FORM.
;; SETTER is the same as in `appending!'."
;;   `(,setter ,place (cl-union ,place ,list :test ,test :test-not ,test-not :key ,key)))

;; ;; To configure variables I don't use the standard =setq=--at least not directly.
;; ;; Instead, I use =set!=.  Adjoining is one of the most common operations done to
;; ;; lisp symbols when configuring Emacs.

;; ;; Something I was always confused about was why adjoin instead of just using
;; ;; =push=.  The latter is more performant; however I don't think that's.  The best reason I could
;; ;; think of is that sometimes you want to re-evaluate parts of your configuration
;; ;; and in that case it is more convenient to have =adjoin= over =push=.
;; (cl-defmacro adjoin! (place value &key test key test-not)
;;   "Adjoin value to place.
;; Same as `adjoining!' but use `set!' as the setter.  Meant to be used for
;; customizing variables."
;;   `(adjoining! ,place ,value :test ,test :key ,key :test-not ,test-not :setter set!))
;; ;;;; block!
;; (defvar oo-block-alist '((stub! . cl-flet)
;;                          (label! . cl-labels)))

;; (defun oo-block-interpret-tree (data tree)
;;   "Return new TREE and DATA."
;;   (pcase tree
;;     (`(,(and loop (or 'for! 'while 'dolist! 'dolist)) ,pred . ,body)
;;      (pcase-let* ((`(,data1 ,tree) (oo-block-interpret-tree nil body)))
;;        (list (map-merge 'plist data data1)
;;              `(catch 'break! (,loop ,pred (catch 'continue ,@tree))))))
;;     (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
;;      (alet! (cl-case name
;;               ((maxing! maximizing!) most-negative-fixnum)
;;               ((minning! minimizing!) most-positive-fixnum)
;;               (counting! 0))
;;        (adjoining! (plist-get data :let) (list symbol it) :test #'equal :key #'car))
;;      (list data tree))
;;     (`(,(or 'without! 'excluding!) . ,(and symbols (guard (oo-all-p #'symbolp symbols))))
;;      (appending! (plist-get data :no-let) symbols)
;;      (list data nil))
;;     (`(,(and macro (guard (member macro '(let!)))) ,match-form ,_ . ,(and plist (guard t)))
;;      (alet! (or (car (member match-form '(gc-cons-threshold gc-cons-percentage)))
;;                 (plist-get plist :init))
;;        (adjoining! (plist-get data :let) (list match-form it) :test #'equal :key #'car))
;;      (list data tree))
;;     (`((,(and name (guard (assoc name oo-block-alist))) ,symbol ,args . ,body) . ,rest)
;;      (pcase-let ((`(,data1 ,rest) (oo-block-interpret-tree nil rest))
;;                  (`(,data2 ,body) (oo-block-interpret-tree nil body)))
;;        (list (map-merge 'plist data data1 data2)
;;              `((cl-flet ((,symbol ,args ,@body)) ,@rest)))))
;;     ((and (pred (listp)) (pred (not oo-cons-cell-p)) (pred (not null)))
;;      (pcase-let ((`(,data1 ,tree1) (oo-block-interpret-tree nil (car tree)))
;;                  (`(,data2 ,tree2) (oo-block-interpret-tree nil (cdr tree))))
;;        (list (map-merge 'plist data1 data2)
;;              (cons tree1 tree2))))
;;     (_
;;      (list data tree))))

;; (defmacro block! (name &rest body)
;;   "Define a lexically-scoped block named NAME.
;; Name may be any symbol.  Code inside body can call `return!'."
;;   (declare (indent 1))
;;   (pcase-let* ((`(,data ,tree) (oo-block-interpret-tree nil body))
;;                (lets (plist-get data :let))
;;                (nolets (plist-get data :nolet))
;;                (bindings (cl-remove-if (-- (assoc it lets) nolets) lets)))
;;     `(let ,bindings ,@tree)))
;; ;; ;;;; quiet!
;; ;; (defmacro quiet! (&rest body)
;; ;;   )
;; ;; ;;;; flet!
;; ;; (defmacro! flet! (bindings &rest body)
;; ;;   ""
;; ;;   (stub! )
;; ;;   (pcase-dolist (`(,symbol ,args . ,body) bindings)
;; ;;     (collecting! originals `(,(cl-gensym "original-fn") ,symbol))
;; ;;     (collecting! sets `(fset ))
;; ;;     (collecting! resets `()))
;; ;;   `(let ,originals
;; ;;      (unwind-protect (progn ,@sets ,@body)
;; ;;        ,@resets)))
;; ;; ;;;; defmacro! and defun!
;; ;; (defun oo-destructure-defun-plus (arglist)
;; ;;   "Return list (name args (docstring declarations) plist body) from DEFUN-ARGS."
;; ;;   (block! nil
;; ;;     (let! (name args (docstring declarations) body) (oo-defun-components arglist))
;; ;;     (let! (args plist) (-split-with (-not #'keywordp) args))
;; ;;     (let! (raw body) (-split-with (-compose #'keywordp #'car-safe) body))
;; ;;     (for! ((elt &as k v) raw)
;; ;;       (let! (k . v) elt)
;; ;;       (pushing! alist (cons k (if (cdr v) v (car v)))))
;; ;;     (let! new (if (and plist alist)
;; ;;                   (map-merge 'plist plist alist)
;; ;;                 (map-into (or alist plist) 'plist)))
;; ;;     (list name args (list docstring declarations) new body)))

;; ;; (defmacro defmacro! (&rest args)
;; ;;   "Wrapper around `defmacro!'."
;; ;;   (declare (indent defun) (doc-string 3))
;; ;;   (pcase-let ((`(,name ,arglist ,metadata ,body) (oo-defun-components args)))
;; ;;     `(cl-defmacro ,name ,arglist
;; ;;        ,@(cl-remove-if #'null metadata)
;; ;;        (block! ,name
;; ;;          (excluding! ,@(-remove #'oo-ampersand-symbol-p (flatten-list arglist)))
;; ;;          ,@body))))

;; ;; (defmacro! defun! (&rest args)
;; ;;   "Wrapper around `defun'."
;; ;;   (declare (indent defun) (doc-string 3))
;; ;;   (pcase-let ((`(,name ,arglist ,metadata ,body) (oo-defun-components args)))
;; ;;     `(cl-defun ,name ,arglist
;; ;;        ,@(cl-remove-if #'null metadata)
;; ;;        (block! ,name
;; ;;          (excluding! ,@(cl-remove-if #'oo-ampersand-symbol-p (flatten-list arglist)))
;; ;;          ,@body))))
;; ;; ;;;; set!
;; ;; (defmacro! set! (symbol value)
;; ;;   "A \"do-it-all\" setter for configuring variables."
;; ;;   (let! value-var (make-symbol "value"))
;; ;;   `(if (not (boundp ',symbol))
;; ;;        (push (cons ',symbol ',value) oo-unbound-symbol-alist)
;; ;;      (let ((,value-var ,value))
;; ;;        (message "Set %s to %S" ',symbol ,value-var)
;; ;;        (aif (get ',symbol 'custom-set)
;; ;;            (funcall it ',symbol ,value-var)
;; ;;          (with-no-warnings (setq ,symbol ,value-var))))))
