;;; Base Library
;; This library should not require an external dependencies.  The main reason is
;; so that it can be loaded from scripts without having to think about package
;; management.
(require 'pcase)
(require 'subr-x)
(require 'cl)
;;;; helpers
(defun oo-snoc (list elt)
  "Add item to the end of list."
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
;;;; destructuring 
(defun oo-destructure ()
  ())

(defmacro let! (patterns &rest body)
  `(let ,patterns ,@body))
;;;; looping
(defmacro for! (pred &rest body)
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
               (for! ,list ,@body))
              (t
               (error "Unknown list predicate: %S" ',pred)))))))
;;;; setting
(cl-defmacro appending! (place list &key (setter 'setf))
  "Append LIST to the end of PLACE.
SETTER is the symbol of the macro or function used to do the setting."
  `(,setter ,place (append ,place ,list)))

;; Important to note that this macro is not as efficient as pushing because it's adding to the end
;; of the list.  So this macro should be used only in non-performance-intensive code.  In
;; performance-intensive code we need the =push-nreverse= idiom.
(cl-defmacro collecting! (place item &key (setter 'setf))
  "Affix ITEM to the end of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (-snoc ,place ,item)))

(defalias 'snocing! 'collecting!)
(defalias 'affixing! 'collecting!)

;; You might be wondering why I didn't create a macro with a setter for these.
;; Well I haven't had a case yet when I've wanted to increment or decrement the
;; value symbol used in customization.
(defalias 'incrementing! 'cl-incf)
(defalias 'counting! 'cl-incf)
(defalias 'decrementing! 'cl-decf)

(cl-defmacro prepending! (place list &key (setter 'setf))
  "Prepend LIST to beginning of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (append ,list ,place)))

(cl-defmacro maxing! (place form &key (setter 'setf) (comparator '>))
  "Set PLACE to the greater of PLACE and FORM.
SETTER is the same as in `appending!'."
  (cl-with-gensyms (value1 value2)
    `(,setter ,place (let ((,value1 ,form)
                           (,value2 ,place))
                       (if (,comparator ,value1 ,value2) ,value1 ,value2)))))

(cl-defmacro minning! (place form &key (setter 'setf) (comparator '<))
  "Set PLACE to the lesser of PLACE and FORM.
SETTER is the same as in `appending!'. COMPARATOR is the same as in `maxing!'."
  `(maxing! ,place ,form :setter ,setter :comparator ,comparator))

(cl-defmacro concating! (place string &key (setter 'setf) separator)
  "Concat PLACE and STRING with SEPARATOR.
SETTER is the same as in `appending!'"
  `(,setter ,place (string-join (list ,place ,string) ,separator)))

(cl-defmacro adjoining! (place item &key test test-not key (setter 'setf))
  "Set PLACE to the value of `cl-adjoin'.
SETTER is the same as in `appending!'."
  `(,setter ,place (cl-adjoin ,item ,place :test ,test :test-not ,test-not :key ,key)))

;; I know =push= already exists.  But I want a variant of push that can be used
;; with the =block!= macro.
(cl-defmacro pushing! (place item &key (setter 'setf))
  "Cons ITEM to PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (cons ,item ,place)))

;; To configure variables I don't use the standard =setq=--at least not directly.
;; Instead, I use =set!=.  Adjoining is one of the most common operations done to
;; lisp symbols when configuring Emacs.

;; Something I was always confused about was why adjoin instead of just using
;; =push=.  The latter is more performant; however I don't think that's.  The best reason I could
;; think of is that sometimes you want to re-evaluate parts of your configuration
;; and in that case it is more convenient to have =adjoin= over =push=.
(cl-defmacro unioning! (place list &key test test-not key (setter 'setf))
  "Set PLACE to the union of PLACE and FORM.
SETTER is the same as in `appending!'."
  `(,setter ,place (cl-union ,place ,list :test ,test :test-not ,test-not :key ,key)))

;; To configure variables I don't use the standard =setq=--at least not directly.
;; Instead, I use =set!=.  Adjoining is one of the most common operations done to
;; lisp symbols when configuring Emacs.

;; Something I was always confused about was why adjoin instead of just using
;; =push=.  The latter is more performant; however I don't think that's.  The best reason I could
;; think of is that sometimes you want to re-evaluate parts of your configuration
;; and in that case it is more convenient to have =adjoin= over =push=.
(cl-defmacro adjoin! (place value &key test key test-not)
  "Adjoin value to place.
Same as `adjoining!' but use `set!' as the setter.  Meant to be used for
customizing variables."
  `(adjoining! ,place ,value :test ,test :key ,key :test-not ,test-not :setter set!))
;;;; quiet!
(defmacro quiet! (&rest forms)
  )
;;;; flet!
(defmacro flet! (&rest forms)
  )
;;;; block!
(defun oo-tree-map-nodes (pred fun tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fun tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fun (car tree))
               (oo-tree-map-nodes pred fun (cdr tree))))
        (t
         tree)))

(defun oo-block-interpret-tree (tree &optional data)
  (pcase tree
    (`(,(and loop (or 'for! 'while 'dolist! 'dolist)) ,pred . ,body)
     (oo-block-interpret-tree body)
     (list `(catch 'break! (,loop ,pred (catch 'continue ,@body))) data))
    (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
     (setf (plist-get data :let) (list symbol nil))
     (cl-case name
       ((maxing! maximizing!) most-negative-fixnum)
       ((minning! minimizing!) most-positive-fixnum)
       (counting! 0))
     (adjoining! (plist-get data :let) (list symbol it) :test #'equal :key #'car))
    ;; (`(,(or 'let! 'let->>! 'let-->!) ,match-form ,_ . ,(and plist (guard t)))
    ;;  (alet (or (car (member match-form '(gc-cons-threshold gc-cons-percentage)))
    ;;            (plist-get plist :init))
    ;;    (adjoining! (plist-get data :let) (list match-form it) :test #'equal :key #'car))
    ;;  (setq zipper (treepy-next zipper)))
    (_ tree)))

(defmacro block! (name &rest body)
  "Define a lexically-scoped block named NAME.
Name may be any symbol.  Code inside body can call `return!'."
  (declare (indent 1))
  (pcase-let ((`(,tree ,data) (oo-block-interpret-tree body)))
    (seq-let ,(oo-block-let-bindings let nolet) ,@body)))
