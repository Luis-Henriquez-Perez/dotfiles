;;; oo-base-lib.el --- my library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is the primary set of functions and macro I use to.  They are designed
;; to only rely on built-in emacs packages so that they can be loaded into any
;; script.
;;
;;; Base Library
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
;;;; require built-in packages
;; I need it to help me with pattern matching.
(require 'pcase)
;; It has several useful functions and macros.
(require 'subr-x)
(require 'cl-lib)
;;;; aliases
;; Make symbols with consistent naming conventions to elisp predicate functions.
;; Most elisp predicate functions end in by "-p" to denote that they return
;; non-nil if a condition is true.  Many of the predicate functions provided by
;; =cl-lib= do not follow this convention.
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
  (and (listp obj)
       (cdr-safe (last obj))))

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
;;;; advices
;; A general advice to message the.
;; (defun oo-message-arguments (fn &rest args)
;;   ())
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
  `(,setter ,place (oo-snoc ,place ,item)))

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
;;;; block!
;; Convert patterns to pcase patterns
;; Essentially, I am just mapping.
(defun oo-seq-map-nodes (pred fn tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fn tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fn (car tree))
               (oo-tree-map-nodes pred fn (cdr tree))))
        ((vectorp tree)
         (seq-into (oo-tree-map-nodes pred fn (seq-into tree 'list)) 'vector))
        (t
         tree)))

(defun oo-pcase-pattern (pattern)
  "Return a pcase pattern from a tree of symbols."
  (list '\` (oo-seq-map-nodes #'oo-non-nil-symbol-p (-- (list '\, it)) pattern)))

(defun oo-tree-map-nodes (pred fun tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fun tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fun (car tree))
               (oo-tree-map-nodes pred fun (cdr tree))))
        (t
         tree)))

(defvar oo-block-alist '((stub! . cl-flet)
                         (label! . cl-labels)))

;; These are just placeholder macros.
(defmacro label! (&rest args) (declare (indent defun)))
;; This is a question too. Should I make these the same as flet!, probably not huh?
(defalias 'fflet! 'label!)

(defun oo-block-interpret-tree (data tree)
  "Return new TREE and DATA."
  (pcase tree
    (`(,(and loop (or 'for! 'while 'dolist! 'dolist)) ,pred . ,body)
     (pcase-let* ((`(,data1 ,tree) (oo-block-interpret-tree nil body)))
       (list (map-merge 'plist data data1)
             `(catch 'break! (,loop ,pred (catch 'continue ,@tree))))))
    (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
     (alet! (cl-case name
              ((maxing! maximizing!) most-negative-fixnum)
              ((minning! minimizing!) most-positive-fixnum)
              (counting! 0))
       (adjoining! (plist-get data :let) (list symbol it) :test #'equal :key #'car))
     (list data tree))
    (`(,(or 'without! 'excluding!) . ,(and symbols (guard (oo-all-p #'symbolp symbols))))
     (appending! (plist-get data :no-let) symbols)
     (list data nil))
    (`(,(and macro (guard (member macro '(let!)))) ,match-form ,_ . ,(and plist (guard t)))
     (alet! (or (car (member match-form '(gc-cons-threshold gc-cons-percentage)))
                (plist-get plist :init))
       (adjoining! (plist-get data :let) (list match-form it) :test #'equal :key #'car))
     (list data (cons 'setq (cdr tree))))
    (`((,(and name (guard (assoc name oo-block-alist))) ,symbol ,args . ,body) . ,rest)
     (pcase-let ((`(,data1 ,rest) (oo-block-interpret-tree nil rest))
                 (`(,data2 ,body) (oo-block-interpret-tree nil body)))
       (list (map-merge 'plist data data1 data2)
             `((cl-flet ((,symbol ,args ,@body)) ,@rest)))))
    ((and (pred (listp)) (pred (not oo-cons-cell-p)) (pred (not null)))
     (pcase-let ((`(,data1 ,tree1) (oo-block-interpret-tree nil (car tree)))
                 (`(,data2 ,tree2) (oo-block-interpret-tree nil (cdr tree))))
       (list (map-merge 'plist data1 data2)
             (cons tree1 tree2))))
    (_
     (list data tree))))

(defmacro block! (name &rest body)
  "Define a lexically-scoped block named NAME.
Name may be any symbol.  Code inside body can call `return!'."
  (declare (indent 1))
  (pcase-let* ((`(,data ,tree) (oo-block-interpret-tree nil body))
               (lets (plist-get data :let))
               (nolets (plist-get data :nolet))
               (bindings (cl-remove-if (-- (assoc it lets) nolets) lets)))
    `(let ,bindings ,@tree)))
;;;; quiet!
(defmacro quiet! (&rest body)
  )
;;;; flet!
(defmacro! flet! (bindings &rest body)
  ""
  (stub! )
  (pcase-dolist (`(,symbol ,args . ,body) bindings)
    (collecting! originals `(,(cl-gensym "original-fn") ,symbol))
    (collecting! sets `(fset ))
    (collecting! resets `()))
  `(let ,originals
     (unwind-protect (progn ,@sets ,@body)
       ,@resets)))

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
;;;; cset!
;; (defmacro! cset! (symbol value)
;;   "A \"do-it-all\" setter for configuring variables."
;;   (let! value-var (make-symbol "value"))
;;   `(if (not (boundp ',symbol))
;;        (push (cons ',symbol ',value) oo-unbound-symbol-alist)
;;      (let ((,value-var ,value))
;;        (message "Set %s to %S" ',symbol ,value-var)
;;        (aif (get ',symbol 'custom-set)
;;            (funcall it ',symbol ,value-var)
;;          (with-no-warnings (setq ,symbol ,value-var))))))
;;;; let! 
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

(defsubst oo-non-nil-symbol-p (obj)
  (and obj (symbolp obj)))

;; This is an attempt to use seq-let to make a pcase-let variant but with better
;; syntax.  Note this is *NOT* a very efficient way of doing this.  But it is
;; not like I am binding hundreds of bindings.  Dash's =-let= and =-let*= in
;; contrast are written to be very efficient.  But it must be said, this is
;; quite a beautiful way of composing macros I have written before.
;; (defmacro let! (bindings &rest body)
;;   (let (wrappers)
;;     (while bindings
;;       (while (and bindings (sequencep (car-safe (car-safe bindings))))
;;         (push `(seq-let ,@(pop bindings)) wrappers))
;;       (awhen! (take-while! (oo-non-nil-symbol-p (car-safe (car-safe bindings))) bindings)
;;         (push `(let* ,it) wrappers)))
;;     (oo-wrap-forms (reverse wrappers) body)))

(defmacro let! (bindings &rest body)
  (let (wrappers)
    (while bindings
      (while (and bindings (sequencep (car-safe (car-safe bindings))))
        (push `(pcase-let* (pop bindings)) wrappers))
      (awhen! (take-while! (oo-non-nil-symbol-p (car-safe (car-safe bindings))) bindings)
        (push `(let* ,it) wrappers)))
    (oo-wrap-forms (reverse wrappers) body)))
;;;; Provide feature
(provide 'oo-base-lib)
