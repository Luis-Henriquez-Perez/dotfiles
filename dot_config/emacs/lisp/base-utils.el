;;; base-utils.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/
;;
;; This file is not part of GNU Emacs.
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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file contains utility functions.
;;
;;; Code:
;;;; requirements
(require 'cl-lib)
;;;; converting types
;; These functions try to "do what I mean" when converting from one type to another.
(defun oo-into-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))
(defalias 'oo-to-string 'oo-into-string)

(defun oo-into-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-into-string args)))
(defalias 'oo-to-symbol 'oo-into-symbol)

(defun oo-into-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-into-symbol ":" args))
(defalias 'oo-to-keyword 'oo-into-keyword)
;;;; quoting
(defun oo-quoted-p (form)
  "Return non-nil if FORM is quoted."
  (declare (pure t) (side-effect-free t))
  (equal (car-safe form) 'quote))

(defun oo-sharpquoted-p (form)
  "Return non-nil if form is sharpquoted."
  (declare (pure t) (side-effect-free t))
  (equal (car-safe form) 'function))

(defun oo-ensure-quote (form)
  "Return quoted form unquoted, otherwise return form."
  (declare (pure t) (side-effect-free t))
  (if (oo-quoted-p form) form (macroexp-quote form)))
;;;; miscellaneous
(defun oo-hook-symbol-p (obj)
  "Return non-nil if SYMBOL is a hook symbol."
  (declare (pure t) (side-effect-free t))
  (and (symbolp obj)
       (string-match-p "[^[:space:]]+-hook\\'" (symbol-name obj))))

(defun oo-true-list-p (object)
  "Return non-nil if OBJECT is a true list.
A \"true list\" is a list whose CDR is also a list."
  (declare (pure t) (side-effect-free error-free))
  (and (listp object) (listp (cdr-safe object))))

(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms to be wrapped.  WRAPPERS are a list of forms
representing the wrappers to apply.  If WRAPPERS is empty, `progn' is added to
ensure the result is syntactically valid."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (append wrapper (list forms))))
  forms)
;;;; oo-condition-case-fn
;; One thing is the fact that because it is a function it can be composed and
;; chained.  Another is I can swap in and out the =condition-case= body and handlers
;; without having to write out the whole =condition-case= form.  Even though the
;; =condition-case= form is relatively simple I have admittedly had trouble
;; remembering its components off the top of my head.

;; I'd like to have the signature be something like ~(function &rest args &key ...)~;
;; that way it would be truly analogous to =funcall=. However, then the signature
;; would ambiguous if =function= has arguments that are the same as keys specified by
;; =&key=.
(defun oo-condition-case-fn (fn action &optional handlers)
  "Return a function that calls ACTION when errors matching HANDLERS are raised.
ACTION is a function with three arguments the error object, FN and the list of
arguments FN will be called with."
  ;; To be honest I'm not sure if I need to make a gensym for the variable
  ;; `err'.  I do it just in case.
  (cl-callf or handlers 'error)
  (cl-callf or action #'ignore)
  (cl-with-gensyms (err)
    `(lambda (&rest args)
       (condition-case ,err
           (apply #',fn args)
         (,handlers (funcall #',action ,err #',fn args))))))
(defalias 'oo-cond-case-fn 'oo-condition-case-fn)
;;;; oo-in-string-or-comment-p
;; This function is used by captain and abbrev.
(defun oo-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment.
Specifically, return the symbol `string' if point is in a string, the symbol
`comment' if in a comment and nil otherwise."
  (declare (pure t) (side-effect-free t))
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))
;;;; if-not!
;; More often than not when I am using `if', the default else clause is simpler than
;; the then clause.  And in that case I end up having to wrap the then clause in
;; a `progn'. I want to invert the else clause and the if clause so I do not
;; need to include the extra `progn' in that case.  I also considered just
;; writing a macro that expands to an `if' with the then and else reversed, but
;; I think it might be confusing.
(defmacro if-not! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond) ,then ,@else))

(defalias 'nif! 'if-not!)
;;;; oo-funcall-silently
(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (shut-up (apply fn args)))
;; With lexical binding you can actually store the values of let-bound variables
;; in a function by creating a closure.  But it might be useful to.
;;;; oo-first-success
;; This function is very similar to dash's [[file:snapshots/_helpful_function__-first_.png][-first]] or cl-lib's [[file:snapshots/_helpful_function__cl-find-if_.png][cl-find-if]].
;; These functions take a predicate and a list and they return the first element of
;; the list for which ~(pred element)~ returns non-nil.  The function =oo-first-success= also takes a
;; predicate and the list, but instead it returns the first non-nil return value of
;; ~(pred element)~.  For example, ~(oo-first-sucess 'numberp '(a t 0))~ would return
;; =t= instead of =0= as it would for =-first= or =cl-find-if= because ~(numberp 0)~ evaluates
;; to =t=. The name of this function is inspired by a similar function designed for
;; hooks [[file:snapshots/_helpful_function__run-hooks-with-args-until-success_.png][run-hook-with-args-until-success]].
(defun oo-first-success (fn list)
  "Return the first non-nil result of applying FN to an element in LIST."
  (declare (pure t) (side-effect-free t))
  (let (success)
    (--each-while list (not (setq success (funcall fn it))))
    success))
;;;; numbers
(defsubst oo-negative-p (number)
  "Return non-nil if NUMBER is less than zero."
  (< number 0))

(defsubst oo-positive-p (number)
  "Return non-nil if NUMBER is greater than zero."
  (> number 0))
;;;; other
(defun oo-alist (&rest args)
  "Create an alist from ARGS.
Odd elements of ARGS are the keys, even elements are the values."
  (let (alist)
    (while args
      (push (cons (pop args) (pop args)) alist))
    (setq alist (nreverse alist))))
;;; provide
(provide 'base-utils)
;;; base-utils.el ends here
