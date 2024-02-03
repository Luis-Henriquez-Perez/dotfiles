;;; oo-base-library.el --- main library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 02 Feb 2024
;;
;; URL:
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
;;; Code:
;; This is the primary set of functions and macro I use to.  They are designed
;; to only rely on built-in Emacs packages so that they can be loaded into any
;; script and also to make.
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
(defalias 'oo-always-p 'cl-every)
(defalias 'oo-none-p 'cl-notany)
(defalias 'oo-never-p 'cl-notany)
(defalias 'oo-some-p 'cl-notevery)
(defalias 'oo-select #'cl-remove-if-not)
(defalias 'oo-filter #'cl-remove-if-not)
(defalias 'oo-true #'always)
;; (defalias 'oo-always #'always)
(defalias 'oo-false #'ignore)
;; (defalias 'oo-never #'ignore)
;;;; helpers
(defun oo-cons-cell-p (obj)
  "Return non-nil only if OBJ is a cons-cell."
  (declare (pure t) (side-effect-free t))
  (and (consp obj)
       (not (listp (cdr obj)))))

(defun oo-proper-list-p (obj)
  "Return non-nil only if OBJ is a proper list."
  (declare (pure t) (side-effect-free t))
  (and (listp obj)
       (or (null obj)
           (null (cdr-safe (last obj))))))

(defun oo-improper-list-p (obj)
  "Return non-nil only if OBJ is not a proper list."
  (declare (pure t) (side-effect-free t))
  (and (listp obj)
       (cdr-safe (last obj))))

(defun oo-snoc (list elt &rest elts)
  "Append ELT to the end of LIST."
  (declare (pure t) (side-effect-free t))
  (append list (list elt) elts))

(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms.  WRAPPER are a list of forms."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (oo-snoc wrapper forms)))
  forms)

(defun oo-into-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defun oo-into-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-into-string args)))

(defun oo-into-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-into-symbol ":" args))
;;;; special symbols
(defun oo-list-marker-p (obj)
  "Return non-nil if OBJ is a list marker.
Examples are `&rest' and `&optional'."
  (char-equal ?& (seq-first (symbol-name obj))))
;;;; anaphoric macros
;; I used the [[][anaphora]] package for these macros, but in reality they are
;; trivial to write on my own.
(defmacro alet! (expr &rest body)
  "Bind the result of EXPR to `it' during BODY."
  (declare (indent 1))
  `(let ((it ,expr)) ,@body))

(defmacro aif! (cond then &rest else)
  "Like `if', but COND is bound to `it' during THEN and ELSE."
  (declare (indent 2))
  `(alet! ,cond (if it ,then ,@else)))

(defmacro awhen! (cond &rest body)
  "Like `when', but the result of COND is bound to `it' during BODY."
  (declare (indent 1))
  `(alet! ,cond (when it ,@body)))
;;;; functional
(defalias 'oo-partial 'apply-partially)

;; Copied/inspired from dash's =-rpartial=.
(defun oo-rpartial (fn &rest args)
  "Return a function with fewer arguments than FN."
  (lambda (&rest args-before) (apply fn (append args-before args))))

;; A replacement for dash's =-const=
(defun oo-const (c)
  "Return a function that returns C ignoring any arugments."
  (lambda (&rest _) c))

;;;; setting
(cl-defmacro appending! (place list &key (setter 'setf))
  "Append LIST to the end of PLACE.
SETTER is the symbol of the macro or function used to do the setting."
  `(,setter ,place (append ,place ,list)))

;; Important to note that this macro is not as efficient as pushing because it's
;; adding to the end of the list.  So this macro should be used only in
;; non-performance-intensive code.  In performance-intensive code we need the
;; =push-nreverse= idiom.
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
  (alet! (keys '(:test ,test :test-not ,test-not :key ,key))
    `(,setter ,place (cl-adjoin ,item ,place ,@keys))))

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
;; =push=.  The latter is more performant; however I don't think that's.  The
;; best reason I could think of is that sometimes you want to re-evaluate parts
;; of your configuration and in that case it is more convenient to have =adjoin=
;; over =push=.
(cl-defmacro unioning! (place list &key test test-not key (setter 'setf))
  "Set PLACE to the union of PLACE and FORM.
SETTER is the same as in `appending!'."
  `(,setter ,place (cl-union ,place ,list :test ,test :test-not ,test-not :key ,key)))

;; This is really messy lol.  I am sure there is a better way to do this.
;; Basically, I am saying add a comma to all the symbols, but replace certain
;; patterns with and record these patterns for me.  I would rather do this with
;; an iterator.
(defun oo--match-form-wrappers (match-form)
  (cl-labels ((fn (wrappers tree)
                (pcase tree
                  ((pred null)
                   (list wrappers nil))
                  ((and sym (pred symbolp))
                   (list wrappers (list '\, sym)))
                  (`(&as ,whole ,parts)
                   (alet! (cl-gensym "&as")
                          (list `((let! ((,whole ,it)
                                         (,parts ,it))))
                                (list '\, it))))
                  (`(,(or '&map '&alist '&plist '&hash) ,map)
                   (alet! (cl-gensym "&map")
                          (list `((let ((,it ,map))) (with-map! ,it))
                                (list '\, it))))
                  ((pred listp)
                   (pcase-let* ((`(,wrappers1 ,tree1) (fn nil (car tree)))
                                (`(,wrappers2 ,tree2) (fn nil (cdr tree))))
                     (list (append wrappers wrappers1 wrappers2)
                           (cons tree1 tree2))))
                  ((pred vectorp)
                   (alet! (mapcar (oo-partial #'fn wrappers) (append tree))
                          (list (apply #'append (mapcar #'car it))
                                (vconcat (mapcan #'cdr it))))))))
    (pcase-let* ((`(,wrappers ,pcase-mf) (fn nil match-form)))
      (pcase pcase-mf
        (`(,(and comma (guard (equal '\, comma))) ,(and symbol (pred symbolp)))
         (list wrappers symbol))
        (_
         (list wrappers (list '\` pcase-mf)))))))

(defun oo--let-bind (bind)
  "Return wrappers."
  (pcase bind
    ((pred symbolp)
     `((let* (,bind))))
    (`(,(pred symbolp) ,_)
     `((let* (,bind))))
    (`(#',(and fn (pred symbolp)) ,lambda)
     `((lef! ((,fn ,lambda)))))
    (`(,(and mf (or (pred listp) (pred vectorp))) ,value)
     (pcase-let ((`(,wrappers ,pcase-mf) (oo--match-form-wrappers mf)))
       `((pcase-let* ((,pcase-mf ,value))) ,@wrappers)))
    (_
     (error "Unknown predicate %S." bind))))

;; I do not think that sef extensions are crazy useful for `cl-letf' except for
;; `(symbol-function)'.  In terms of implementation I want to create a macro
;; that ties together all the "letters"--cl-letf, cl-flet, cl-labels,
;; cl-macrolet, etc.
(defmacro let! (bindings &rest body)
  "Like `pcase-let*' but all."
  (declare (indent 1))
  (oo-wrap-forms (mapcan #'oo--let-bind bindings) body))
;;;; block!
(defun oo--interpret-block (data tree)
  "Return new TREE and DATA."
  (pcase tree
    (`(,(and loop (or 'for! 'while 'dolist! 'dolist)) ,pred . ,body)
     (pcase-let* ((`(,data1 ,tree) (oo--interpret-block nil body)))
       (list (map-merge 'plist data data1)
             `(catch 'break! (,loop ,pred (catch 'continue ,@tree))))))
    (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
     (alet! (cl-case name
              ((maxing! maximizing!) most-negative-fixnum)
              ((minning! minimizing!) most-positive-fixnum)
              (counting! 0))
            (adjoining! (plist-get data :let) (list symbol it) :test #'equal :key #'car))
     (list data tree))
    (`((,(or 'with! 'wrap!) . ,(and wrappers (pred listp))) . ,rest)
     (appending! (plist-get data :wrappers) wrappers)
     (list data rest))
    (`((,(or 'without! 'excluding!) . ,(and symbols (guard (oo-all-p #'symbolp symbols)))) . ,rest)
     (appending! (plist-get data :no-let) symbols)
     (list data rest))
    (`((gensym! ,(and name (pred symbolp))) . ,rest)
     (adjoining! (plist-get data :let) (list name (cl-gensym (symbol-name name))))
     (list data rest))
    (`(,(and macro (guard (member macro '(let! set!)))) ,match-form ,_ . ,(and plist (guard t)))
     (alet! (plist-get plist :init)
            (adjoining! (plist-get data :let) (list match-form it) :test #'equal :key #'car))
     (list data (cons 'setq (cdr tree))))
    ;; Typically I will use these when I am.
    (`((,(or 'stub! 'flet!)  ,name ,fn) . ,rest)
     (let! (((data1 rest) (oo--interpret-block nil rest)))
       (list (map-merge 'plist data data1) `((cl-flet ((,name ,fn)) ,@rest)))))
    (`((,(or 'stub! 'flet!) ,name ,args . ,body) . ,rest)
     (let! (((data1 rest) (oo--interpret-block nil rest))
            ((data2 body) (oo--interpret-block nil body)))
       (list (map-merge 'plist data data1 data2)
             `((cl-flet ((,name ,args ,@body)) ,@rest)))))
    ;; This is my own variant that takes the original function.  I name it.
    (`((,(or 'nflet! 'noflet!) ,name ,args . ,body) . ,rest)
     (let! (((data1 rest) (oo--interpret-block nil rest))
            ((data2 body) (oo--interpret-block nil body)))
       (list (map-merge 'plist data data1 data2)
             `((lef! ((,name (lambda ,args ,@body))) ,@rest)))))
    ((and (pred (listp)) (pred (not oo-cons-cell-p)) (pred (not null)))
     (pcase-let ((`(,data1 ,tree1) (oo--interpret-block nil (car tree)))
                 (`(,data2 ,tree2) (oo--interpret-block nil (cdr tree))))
       (list (map-merge 'plist data1 data2)
             (cons tree1 tree2))))
    (_
     (list data tree))))

;; I am conflicted between the name =this-fn= and =orig-fn=.  I think all else
;; being equal =orig-fn= is a better name than =this-fn=.  But I know that
;; =this= and =it= are used more commonly anaphoric names.
(defmacro lef! (bindings &rest body)
  "Similar to `cl-letf' but."
  (let (binds orig-fn (args (cl-gensym "args")))
    (pcase-dolist (`(,sym ,fn) bindings)
      (setq orig-fn (gensym "orig-fn"))
      (pushing! binds `(,orig-fn (if (fboundp #',sym) (symbol-function #',sym) nil)))
      (pushing! binds `((symbol-function #',sym)
                        (lambda (&rest ,args)
                          (let ((this-fn ,orig-fn))
                            (apply ,fn ,args))))))
    `(cl-letf* ,(nreverse binds) ,@body)))

(defmacro block! (name &rest body)
  "Define a lexically-scoped block named NAME.
Name may be any symbol.  Code inside body can call `return!'."
  (declare (indent 1))
  (let! (((data body) (oo--interpret-block nil body))
         ;; lets is an alist.
         (lets (plist-get data :let))
         ;; nolets is a list of symbols.
         (nolets (plist-get data :nolet))
         (binds (cl-remove-if (lambda (bind) (member (car bind) nolets)) lets))
         (wrappers `((cl-block ,name) (let ,binds) ,@(plist-get data :wrappers))))
    ;; Yea this wrap forms function is paying dividens for me.
    (oo-wrap-forms wrappers body)))
;;;; defmacro! and defun!
(defun oo-defun-components (arglist)
  "Return the components of defun.
ARGLIST is the arglist of `defun' or similar macro.
The components returned are in the form of (name args (docstring declaration
interactive-form) body)."
  (let! (((name args . body) arglist)
         (docstring (when (stringp (car-safe body)) (pop body)))
         (decls (when (equal 'declare (car-safe (car-safe body))) (pop body)))
         (iform (when (equal 'interactive (car-safe (car-safe body))) (pop body))))
    (list name args (list docstring decls iform) body)))

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
;;;; looping
;; There is a huge question of whether to automatically wrap loops with
;; =block!=, but I decided to.
(defmacro for! (pred &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
This is the same as `dolist' except argument is MATCH-FORM.  match-form can be a
symbol as in `dolist', but.  LIST can be a sequence."
  (declare (indent 1))
  (pcase pred
    ((or `(repeat ,n) (and n (pred integerp)))
     `(dotimes (_ ,n) ,@body))
    (`(,(and match-form (or (pred listp) (pred vectorp))) ,list)
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
               (for! ,list ,@body))
              (t
               (error "Unknown list predicate: %S" ',pred)))))))
(defalias 'dolist! 'for!)
(defalias 'loop! 'for!)
;;;; quiet!
;; I copied much of the bod of this from the =shut-up= package.  I really wanted
;; to just use that package but the problem is that I need this macro
;; beforehand, specifically for package installation with =package.el=.  The
;; =shut-up= package does a bit more because it puts the messages in a different
;; buffer, but I won't go into that yet--not when and until I think I need it.
(defmacro quiet! (&rest body)
  "Don't allow any output to be messaged."
  ;; Override `standard-output', for `print' and friends, and
  ;; monkey-patch `message'
  `(let! ((standard-output #'ignore)
          (#'message #'ignore)
          (#'write-region
           ;; Wish there was a way not to have to specify all the arguments
           ;; twice.  Well see if I find one or one day thing of one.
           ;; complicating things is that some of the arguments are optional.
           (lambda (start end filename &optional append visit lockname mustbenew)
             (unless visit (setq visit 'no-message))
             (funcall this-fn start end filename append visit lockname mustbenew)))
          (#'load (lambda (fn file noerror nomsg nosuffix must-suffix)
                    (funcall this-fn file noerror t nosuffix must-suffix))))
     ,@body))
;;;; buffer-contents
(defun oo-buffer-contents (buffer)
  (with-current-buffer (get-buffer buffer)
    (buffer-string)))

(provide 'oo-base-library)
;;; oo-base-library.el ends here
