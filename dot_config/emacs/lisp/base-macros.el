;;; base-macros.el --- Initialize base-macros -*- lexical-binding: t; -*-
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
;; Initialize base-macros.
;;
;;; Code:
(require 'base-utils)
(require 'base-macros-setters)
(require 'base-macros-with-map)
(require 'base-macros-autolet)
(require 'base-macros-for)
(require 'base-macros-bind)
;;;; macros
;;;;; nif!
;; More often than not when I am using `if', the default else clause is simpler than
;; the then clause.  And in that case I end up having to wrap the then clause in
;; a `progn'. I want to invert the else clause and the if clause so I do not
;; need to include the extra `progn' in that case.  I also considered just
;; writing a macro that expands to an `if' with the then and else reversed, but
;; I think it might be confusing.
(defmacro nif! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond) ,then ,@else))
;;;;; anaphoric macros
(defmacro alet! (form &rest body)
  "Bind the result FORM to `it' for the duration of BODY."
  (declare (debug let) (indent 1))
  `(let ((it ,form))
     ,@body))

(defmacro aand! (&rest conditions)
  "Like `and' but bind the result of first condition to `it'."
  `(alet! ,(car conditions)
     (and it ,@(cdr conditions))))

(defmacro aif! (cond then &rest else)
  "Like `if' but bind the result of COND to `it' for duration of THEN and ELSE."
  (declare (debug t) (indent 2))
  `(alet! ,cond (if it ,then ,@else)))

(defmacro awhen! (cond &rest body)
  "Like `when' but the result of COND is bound to `it'."
  (declare (debug when) (indent 1))
  `(aif! ,cond (progn ,@body) nil))

(defmacro aprog1! (form &rest body)
  "Like `prog1' but bind first form to `it'."
  (declare (debug when) (indent 1))
  `(alet! ,form (prog1 it ,@body)))
;;;;; quietly!
(defmacro quietly! (&rest forms)
  "Run FORMS without generating any output.
Silence calls to `message', `load', `write-region' and anything that
writes to `standard-output'."
  `(let ((inhibit-message t)
         (save-silently t))
     (cl-letf ((standard-output #'ignore)
               ((symbol-function 'message) #'ignore)
               ((symbol-function 'load)
                (lambda (file &optional noerror nomessage nosuffix must-suffix)
                  (funcall #'load file noerror t nosuffix must-suffix)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (funcall write-region start end filename append visit lockname mustbenew))))
       ,@forms)))
;;;;; opt!
(defmacro! opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let ((value-var (gensym "value")))
    `(if (not (boundp ',symbol))
         ;; This quote on he lambda is needed to avoid infinite recursion.
         (push '(lambda () (opt! ,symbol ,value))
               (gethash ',symbol oo-after-load-hash-table))
       (let ((,value-var (with-demoted-errors "Error: %S" (with-no-warnings ,value))))
         (aif! (get ',symbol 'custom-set)
             (funcall it ',symbol ,value-var)
           (with-no-warnings (setq ,symbol ,value-var)))))))
;;;;; defhook!
(defmacro hook! (hook function &rest args)
  "Configuration wrapper around `oo-add-hook'."
  `(progn (declare-function ,function nil)
          (oo-add-hook ',hook #',function ,@args)))

(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME."
  (declare (indent defun))
  (while (aand! (car args) (symbolp it) (not (keywordp it)))
    (collecting! hooks (pop args)))
  (dolist (hook hooks)
    (collecting! hook-forms `(oo-add-hook ',hook ',name ,@args)))
  (when (stringp (car body))
    (collecting! metadata (pop body)))
  (when (equal 'declare (car-safe (car body)))
    (collecting! metadata (pop body)))
  `(progn
     (defun! ,name nil ,@metadata ,@body)
     ,@hook-forms))
;;;;; after!
;; I made the decision to add a hook function to a hook regardless of whether
;; the hook has already has been run.  But if the hook has been run the hook
;; function is called individually.  The idea is that I do not want to just
;; evaluate the body and have no record of it being evaluated other than it is
;; side-effects.
(defmacro defafter! (name expr &rest body)
  "Evaluate BODY after EXPR is satisfied."
  (declare (indent defun))
  `(progn
     (defun! ,name nil ,@body)
     (oo-call-after-load ',expr #',name)))
;;;;; setq-hook
(defmacro! setq-hook! (hooks symbol value)
  "Add function to hook that sets the local value of SYMBOL to VALUE."
  (dolist (hook (ensure-list hooks))
    (set! name (intern (format "oo--%s--set-local-var--%s" hook symbol)))
    (set! docstring (format "Set local variable `%S' to `%S'." ',symbol ',value))
    (set! lambda `(lambda (&rest _)
                    (info! "HOOK: %s -> %s" ',hook ',name)
                    (condition-case err
                        (setq-local ,symbol ,value)
                      (error
                       (error! "%s error in local hook %s because of %s"
                               (car err)
                               ',hook
                               (cdr err))))))
    (appending! forms `((fset ',name ,lambda) (add-hook ',hook #',name nil nil))))
  (macroexp-progn forms))
;;;;; lef!
(defmacro lef! (bindings &rest body)
  "Bind each symbol in BINDINGS to its corresponding function during BODY.
BINDINGS is a list of either (SYMBOL FUNCTION), where symbol is the symbol to be
bound and FUNCTION is the function to bind it to; or (SYMBOL ARGS BODY).  In
each of BINDINGS if the symbol is an existing function symbol let-bind the
original function to `this-fn', otherwise bind `this-fn' to nil."
  (declare (indent 1))
  (let (binds orig-fn)
    (pcase-dolist (`(,sym . ,rest) bindings)
      (setq orig-fn (gensym "orig-fn"))
      (push `(,orig-fn (when (fboundp ',sym) (symbol-function ',sym))) binds)
      (push (list `(symbol-function ',sym)
                  (pcase rest
                    (`(,fn . nil)
                     `(lambda (&rest args)
                        (let ((this-fn ,orig-fn)
                              (this-function ,orig-fn))
                          (ignore this-fn this-function)
                          (apply ,fn args))))
                    (`(,args . ,function-body)
                     `(lambda ,args
                        (let ((this-fn ,orig-fn)
                              (this-function ,orig-fn))
                          (ignore this-fn this-function)
                          ,@function-body)))))
            binds))
    `(cl-letf* ,(nreverse binds) ,@body)))
;;; provide
(provide 'base-macros)
;;; base-macros.el ends here
