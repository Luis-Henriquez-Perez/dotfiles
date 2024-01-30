(defun oo-tree-map-nodes (pred fun tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fun tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fun (car tree))
               (oo-tree-map-nodes pred fun (cdr tree))))
        (t
         tree)))

(defsubst oo-non-nil-symbol-p (obj)
  (and obj (symbolp obj)))

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

(defmacro dowhile! (condition &rest body)
  (declare (indent 2))
  `(when ,condition
     ,(car body)
     ,@(cdr body)
     (while ,condition ,@(cdr body))))

(defmacro take-while! (condition list)
  (cl-with-gensyms (old new)
    `(let ((,new nil))
       (while (and ,list ,condition)
         (push (pop ,list) ,new))
       (setq ,new (nreverse ,new)))))

;; This is an attempt to use seq-let to make a pcase-let variant but with better
;; syntax.  Note this is *NOT* a very efficient way of doing this.  But it is
;; not like I am binding hundreds of bindings.  Dash's =-let= and =-let*= in
;; contrast are written to be very efficient.  But it must be said, this is
;; quite a beautiful way of composing macros I have written before.
(defmacro let! (bindings &rest body)
  (let (wrappers)
    (while bindings
      (while (and bindings (sequencep (car-safe (car-safe bindings))))
        (push `(seq-let ,@(pop bindings)) wrappers))
      (awhen! (take-while! (oo-non-nil-symbol-p (car-safe (car-safe bindings))) bindings)
        (push `(let* ,it) wrappers)))
    (oo-wrap-forms (reverse wrappers) body)))
;; Simple symbols, using this function can save me having to provide a string for
;; =format=.
;; (defun oo-args-to-symbol (&rest args)
;;   "Return an interned symbol from ARGS."
;;   (declare (pure t) (side-effect-free t))
;;   (intern (apply #'oo-args-to-string args)))

;; Convert patterns to pcase patterns
;; Essentially, I am just mapping.
;; (defun oo-seq-map-nodes (pred fn tree)
;;   "Same as `-tree-map-nodes', but works for improper lists."
;;   (cond ((funcall pred tree)
;;          (funcall fn tree))
;;         ((consp tree)
;;          (cons (oo-tree-map-nodes pred fn (car tree))
;;                (oo-tree-map-nodes pred fn (cdr tree))))
;;         ((vectorp tree)
;;          (seq-into (oo-tree-map-nodes pred fn (seq-into tree 'list)) 'vector))
;;         (t
;;          tree)))

;;;; function transformers
;; (defun oo-notfn (fn)
;;   (lambda (&rest args) (not (apply fn args))))

;; (defun oo-andfn (fn &rest fns)
;;   )

;;;; advices
;; A general advice to message the.
;; (defun oo-message-arguments (fn &rest args)
;;   ())

(defmacro add! (key value)
  "Add VALUE to the list associated with KEY in ALIST."
  (cl-with-gensyms (existing)
    `(setq alist
           (let ((,existing (assoc ,key alist)))
             (if ,existing
                 (setcdr existing (cons ,value (cdr ,existing)))
               (push (list ,key ,value) alist))))))

(let ((alist '((a 1) (b 2))))
  (add! 'a 3)
  (should (equal '((a 1 3) (b 2)) alist))
  (add! 'b 9)
  (should (equal '((b 9) (a 1 3) (b 2)) alist))
  (add! 'b 8)
  (should (equal '((b 9 8) (a 1 3) (b 2)) alist))
  (add! 'c 0)
  (should (equal '((c 0) (b 9 8) (a 1 3) (b 2)) alist)))

;; For brevity I do not want to add the =!= for this macro.
(defmacro -- (&rest body)
  `(lambda (it) ,@body))

;; Does pcase destructuring but without pcase syntax.
(defmacro let! (bindings &rest body)
  "Like `pcase-let*' but all."
  (declare (indent 1))
  (let (wrappers bind)
    (while bindings
      (setq bind (car-safe (car-safe bindings)))
      (let (pcase-binds)
        (while (and bind (sequencep bind) (not (stringp bind)))
          (setf (caar bindings) (oo-pcase-pattern (caar bindings)))
          (push (pop bindings) pcase-binds)
          (setq bind (car-safe (car-safe bindings))))
        (when pcase-binds
          (push `(pcase-let* ,(reverse pcase-binds)) wrappers)))
      (let (let-binds)
        (while (and bind (symbolp bind))
          (push (pop bindings) let-binds)
          (setq bind (car-safe (car-safe bindings))))
        (when let-binds
          (push `(let* ,(reverse let-binds)) wrappers))))
    (oo-wrap-forms (reverse wrappers) body)))

;; (ert-deftest take-while! ()
;;   (should (let ((foo '(a b 1 2 3))) (list (take-while! (symbolp foo) foo) foo))
;;           '((a b) (1 2 3)))
;;   ;; (should ())
;;   )

;; (ert-deftest oo-non-keyword-symbol-p ()
;;   (should (oo-non-keyword-symbol-p 'foo))
;;   (should-not (oo-non-keyword-symbol-p :foo))
;;   (should-not (oo-non-keyword-symbol-p "foo"))
;;   (should-not (oo-non-keyword-symbol-p 1)))

;; (ert-deftest oo-args-to-string ()
;;   (should (equal "foo" (oo-args-to-string 'foo)))
;;   (should (equal ":foo" (oo-args-to-string :foo)))
;;   (should (equal "foo" (oo-args-to-string "foo")))
;;   (should (equal "1" (oo-args-to-string 1))))

;; (ert-deftest oo-args-to-symbol ()
;;   (should (equal 'foo (oo-args-to-symbol 'foo)))
;;   (should (equal :foo (oo-args-to-symbol :foo)))
;;   (should (equal 'foo (oo-args-to-symbol "foo")))
;;   (should (equal '1 (oo-args-to-symbol 1))))

(defmacro lambda! (match-form &rest body)
  "Same as lambda but provides destructuring."
  (if (oo-all-p #'symbolp match-form)
      `(lambda ,match-form ,@body)
    (cl-with-gensyms (args)
      `(lambda (&rest ,args)
         (let! ((,match-form ,args))
           ,@body)))))

(defalias 'l! 'lambda!)

(ert-deftest lambda! ()
  ;; How many lists need to nest can be a bit confusing, but I think that this
  ;; needs to be.
  ;; (should (equal '((1 . 2) 1 2) (funcall (lambda! ((&as foo (a . b))) (list foo a b)) (cons 1 2))))
  (should (= 10 (funcall (lambda! (a (b c) d) (+ a b c d)) 1 '(2 3) 4)))
  (should (= 3 (funcall (lambda! (a b) (+ a b)) 1 2))))

;;;; flet!
;; I have to say that I am not a fan of the =cl-flet= syntax where you specify
;; the function bodies in the bindings.  I am more O.K. with the seemingly less
;; convenient syntax of =cl-letf= because it is more idiomatic in my opinion.
;; And you do not have to deal with bad indentation.  Only problem is =cl-letf=
;; does not provide access to the original function and in my experience, most
;; of the time, that is the function you want to define.  Sometimes I want this kind
;; of thing because stubbing in =block!= does not provide access to the original
;; function and neither does letf!.
;; However this does require thought from the macro perspective on how.
;; (defmacro! flet! (bindings &rest body)
;;   ""
;;   (stub! )
;;   (for! ((symbol args . body) bindings)
;;     (collecting! originals `(,(cl-gensym "original-fn") ,symbol))
;;     (collecting! sets `(fset ))
;;     (collecting! resets `()))
;;   `(let ,originals
;;      (unwind-protect (progn ,@sets ,@body)
;;        ,@resets)))
