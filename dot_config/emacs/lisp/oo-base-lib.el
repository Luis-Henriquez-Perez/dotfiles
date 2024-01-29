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
          (let! ((,match-form ,elt))
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
;;;; oo-pcase-pattern 
(defun oo-pcase-pattern (pat)
  "Return a pcase pattern from a tree of symbols."
  (cl-labels ((pcase-pat (pat)
                (cl-typecase pat
                  (null nil)
                  (symbol (list '\, pat))
                  (list (cons (pcase-pat (car pat)) (pcase-pat (cdr pat))))
                  (vector (append `[,@(mapcar #'pcase-pat pat)])))))
    (when pat (list '\` (pcase-pat pat)))))
;;;; with-map!
;; (defun oo-bang-symbol-p (obj)
;;   "Return non-nil if OBJ is a bang symbol."
;;   (and obj
;;        (symbolp obj)
;;        (string-match-p regexp (symbol-name obj))))

;; (defalias 'oo-select #'cl-remove-if-not)
;; (defmacro with-map! (map &rest body)
;;   (let!)
;;   (cl-remove-duplicates (cl-remove-if-not #'oo-bang-symbol-p (flatten-list body))))
;;;; let! 
;; I do not think that sef extensions are crazy useful for `cl-letf' except for
;; `(symbol-function)'.  In terms of implementation I want to create a macro
;; that ties together all the "letters"--cl-letf, cl-flet, cl-labels,
;; cl-macrolet, etc.
(defmacro let! (bindings &rest body)
  "Like `pcase-let*' but all."
  (declare (indent 1))
  (let (wrappers)
    (dolist (bind bindings)
      (pcase bind
        ((pred symbolp)
         (pushing! wrappers `(let* (,bind))))
        (`(,(pred symbolp) ,_)
         (pushing! wrappers `(let* (,bind))))
        (`((&as ,alias ,match-form) ,expr)
         (pushing! wrappers `(let! ((,alias ,expr) (,match-form ,alias)))))
        ;; (`(,(or &plist &alist &hash) ,map)
        ;;  (pushing! wrappers `(with-map! ,map)))
        ;; This is like `cl-letf' except the syntax is different and the
        ;; function will take the original function as its first argument.
        ;; (`(#',(and fn (pred symbolp)) ,lambda)
        ;;  (pushing! wrappers `(cl-letf ((symbol-function #',fn)
        ;;                                (apply-partially ,lambda (symbol-function #',fn))))))
        ;; (`((,(or label labels) ,_) ,_)
        ;;  (pushing! wrappers `(cl-labels (,bind))))
        ;; (`((,(or 'mlet 'macrolet) ,_) ,_)
        ;;  (pushing! wrappers `(cl-macrolet (,bind))))
        (`(,(and match-form (or (pred listp) (pred vectorp))) ,value)
         (alet! `(pcase-let* ((,(oo-pcase-pattern match-form) ,value)))
           (pushing! wrappers it)))
        (_
         (error "Unknown predicate %S."))))
    (oo-wrap-forms (reverse wrappers) body)))
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

(defvar oo-block-alist '((stub! . cl-flet)
                         (label! . cl-labels)))

(defmacro label! (&rest args) (declare (indent defun)))
(defalias 'flet! 'label!)

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
  (let! (((data tree) (oo-block-interpret-tree nil body))
         (lets (plist-get data :let))
         (nolets (plist-get data :nolet))
         ;; (bindings (cl-remove-if (lambda (x) (assoc x lets) nolets) lets))
         )
    `(let ,bindings ,@tree)))
;;;; defmacro! and defun!
(defun oo-defun-components (arglist)
  "Return the components of defun.
ARGLIST is the arglist of `defun' or similar macro.
The components returned are in the form of (name args (docstring declaration interactive-form) body)."
  (let! (((name args . body) arglist)
         (docstring (when (stringp (car-safe body)) (pop body)))
         (decls (when (equal 'declare (car-safe (car-safe body))) (pop body)))
         (iform (when (equal 'interactive (car-safe (car-safe body))) (pop body))))
    (list name args (list docstring decls iform) body)))
;; I wanted to hard-code this, but I have a feeling I will be using this
;; function later.
(defun oo-list-marker-p (obj)
  "Return non-nil if OBJ is a list marker.
Examples are `&rest' and `&optional'."
  (char-equal ?& (seq-first (symbol-name obj))))

(defmacro defmacro! (&rest args)
  "Wrapper around `defmacro!'."
  (declare (indent defun) (doc-string 3))
  (let! (((name arglist metadata body) (oo-defun-components args))
         (args (cl-remove-if #'oo-list-marker-p (flatten-list arglist))))
    `(cl-defmacro ,name ,arglist
       ,@(cl-remove-if #'null metadata)
       (block! ,name (excluding! ,args) ,@body))))

(defmacro defun! (&rest args)
  "Wrapper around `defun'."
  (declare (indent defun) (doc-string 3))
  (let! (((name arglist metadata body) (oo-defun-components args))
         (args (cl-remove-if #'oo-list-marker-p (flatten-list arglist))))
    `(cl-defun ,name ,arglist
       ,@(cl-remove-if #'null metadata)
       (block! ,name (excluding! ,@args) ,@body))))
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
(defmacro! flet! (bindings &rest body)
  ""
  (stub! )
  (for! ((symbol args . body) bindings)
    (collecting! originals `(,(cl-gensym "original-fn") ,symbol))
    (collecting! sets `(fset ))
    (collecting! resets `()))
  `(let ,originals
     (unwind-protect (progn ,@sets ,@body)
       ,@resets)))
;;;; quiet!
;; I copied much of the bod of this from the =shut-up= package.  I really wanted
;; to just use that package but the problem is that I need this macro
;; beforehand, specifically for package installation with =package.el=.  The
;; =shut-up= package does a bit more because it puts the messages in a different
;; buffer, but I won't go into that yet--not when and until I think I need it.
;; (defmacro quiet! (&rest body)
;;   "Don't allow any output to be messaged."
;;   ;; Override `standard-output', for `print' and friends, and
;;   ;; monkey-patch `message'
;;   `(let! ((standard-output #'ignore)
;;           (#'message #'ignore)
;;           (#'write-region
;;            ;; Wish there was a way not to have to specify all the arguments
;;            ;; twice.  Well see if I find one or one day thing of one.
;;            ;; complicating things is that some of the arguments are optional.
;;            (lambda (fn start end filename &optional append visit lockname mustbenew)
;;              (unless visit (setq visit 'no-message))
;;              (funcall fn start end filename append visit lockname mustbenew)))
;;           (#'load (lambda (fn file noerror nomsg nosuffix must-suffix)
;;                     (funcall fn file noerror t nosuffix must-suffix))))
;;      ,@body))
;;;; cset!
;; (defmacro! cset! (symbol value)
;;   "A \"do-it-all\" setter for configuring variables."
;;   (let! value-var (make-symbol "value"))
;;   `(if (not (boundp ',symbol))
;;        (push (cons ',symbol ',value) oo-unbound-symbol-alist)
;;      (let ((,value-var ,value))
;;        (message "Set %s to %S" ',symbol ,value-var)
;;        (aif! (get ',symbol 'custom-set)
;;            (funcall it ',symbol ,value-var)
;;          (with-no-warnings (setq ,symbol ,value-var))))))
;;;; Provide feature
(provide 'oo-base-lib)
