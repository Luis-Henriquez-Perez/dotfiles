;;; oo-base-macros.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; The purpose of this file is simply to require all of the macros I am using.
;; This file is meant to be compiled away--as in `(eval-when-compile (require
;; 'oo-macros))'.  I am putting the macros in this file like this so that it is
;; easy to compile them away as opposed to intermingling them with functions.
;;
;;; Code:
(require 'oo-base-utils)
(require 'oo-base-macros-ing)
(require 'oo-base-macros-let)
(require 'oo-base-macros-loop)
(require 'oo-base-macros-map)
(require 'oo-base-macros-progn)
(require 'oo-base-macros-definers)
;;;;; if-not!
;; More often than not when I am using `if', the default else clause is simpler than
;; the then clause.  And in that case I end up having to wrap the then clause in
;; a `progn'. I want to invert the else clause and the if clause so I do not
;; need to include the extra `progn' in that case.  I also considered just
;; writing a macro that expands to an `if' with the then and else reversed, but
;; I think it might be confusing.
(defmacro if-not! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond) ,then ,@else))
;;;;; defhook!
(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME.
NAME should be a hook symbol."
  (declare (indent defun))
  (set! hook (oo-hook name))
  (cl-assert hook t "%s is not a hook symbol" hook)
  (when (vectorp (car body))
    (alet (append (pop body) nil)
      (set! params (list (or (map-elt it :depth) (map-elt it :append))
                         (map-elt it :local)))))
  `(prog1 ',name
     (fset ',name (lambda ,args (progn! ,@body)))
     (add-hook ',hook ',name ,@params)))
;;;;; defadvice!
(defmacro! defadvice! (name args &rest body)
  "Define an advice."
  (declare (indent defun))
  (set! (symbol how-name _) (oo-advice-components name))
  (set! how (cdr (assoc how-name oo-advice-how-alist)))
  `(progn
     (fset ',name `(lambda ,args (progn! ,@body)))
     (advice-add ',symbol ,how ',name)))
;;;;; defafter!
(defmacro! defafter! (name expr &rest body)
  "Define a function that is called after EXPR is resolved.
EXPR is the same as in `oo-call-after-load'.  BODY is enclosed in
`progn!'."
  (declare (indent defun))
  `(progn!
     (set! fn (intern (format "oo-after-load%%s" ',name)))
     (fset fn (oo-report-error-fn (lambda! () ,@body)))
     (oo-call-after-load ',expr ',fn)))
;;;;; opt!
;; The reason this needs to be a macro is because `value' might not be evaluated
;; immediately.
;; TODO: need better error handling for when value producess an error.
(defmacro! opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let ((value-var (gensym "value")))
    `(if (not (boundp ',symbol))
         (push (cons ',symbol ',value) oo-unbound-symbol-alist)
       (let ((,value-var (with-demoted-errors "Error: %S" ,value)))
         (aif (get ',symbol 'custom-set)
             (funcall it ',symbol ,value-var)
           (with-no-warnings (setq ,symbol ,value-var)))))))
;;;;; threading
;; I have written several functions whose last arguments are =FN= and =&rest args= such
;; as [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]] and [[file:snapshots/_helpful_function__oo-call-after-keymap_.png][oo-call-after-keymap]].  And when I was trying to compose
;; these functions together in the body of multiple =oo-bind= functions I realized
;; that composing more than two of such functions produced a very long line.  The
;; line would have several sharp quoted functions. And I found it difficult to read
;; and understand what's going on.  Usually, I try to imagine what I want and when
;; I did that I found myself writing a [[file:snapshots/_helpful_macro__thread-last_.png][thread-last]] form. This is similar to
;; [[file:snapshots/_helpful_macro__thread-last_.png][thread-last]] and [[file:snapshots/_helpful_macro__->>_.png][->>]].  Except it is designed for functions that need a function
;; symbol and function arguments as their arguments such as [[file:snapshots/_helpful_function__funcall_.png][funcall]] and
;; [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]].
(defmacro! thread-partial! (&rest body)
  (flet! sharpquote (obj) `(function ,obj))
  (flet! sharpquote-first (obj) (cons (sharpquote (car obj)) (cdr obj)))
  (append (-last-item body)
          (apply #'append (reverse (mapcar #'sharpquote-first (-butlast body))))))

(defalias '-partial-> 'thread-partial!)
(defalias '-p-> 'thread-partial!)
;;; provide
(provide 'oo-base-macros)
;;; oo-base-macros.el ends here
