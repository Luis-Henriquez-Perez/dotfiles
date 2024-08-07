;;; oo-base-macros.el -*- lexical-binding: t; -*-
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
(require 'oo-base-macros-for-bang)
(require 'oo-base-macros-let-bang)
(require 'oo-base-macros-lef-bang)
(require 'oo-base-macros-progn-bang)
(require 'oo-base-macros-with-map-bang)
(require 'oo-base-macros-definers)
;;;; oo--defhook-arguments
(defun! oo--defhook-arguments (args)
  (set! name (pop args))
  (set! arglist (pop args))
  (while (oo-hook-symbol-p (car arglist))
    (collecting! hooks (pop arglist)))
  (when (stringp args)
    (set! docstring (pop args)))
  (when (vectorp (car args))
    (alet (append (pop args) nil)
      (set! depth (or (map-elt it :depth) (map-elt it :append)))
      (set! local (map-elt it :local))))
  (set! body args)
  (list name arglist hooks body depth local))
;;;; defhook!
(defmacro! defhook! (&rest args)
  "Add function to hook as specified by NAME.
NAME should be a hook symbol."
  (declare (indent defun))
  (set! (suffix arglist hooks body depth local) (oo--defhook-arguments args))
  (macroexp-progn (--map `(oo-generate-hook ',it ',suffix (lambda ,arglist ,@body) ,depth ,local) hooks)))
;;;; defadvice!
(defmacro! defadvice! (name args &rest body)
  "Define an advice."
  (declare (indent defun))
  (set! (symbol how-name _) (oo-advice-components name))
  (set! how (cdr (assoc how-name oo-advice-how-alist)))
  `(progn
     (fset ',name (lambda ,args (progn! ,@body)))
     (advice-add ',symbol ,how ',name)))
;;;; opt!
;; The reason this needs to be a macro is because `value' might not be evaluated
;; immediately.
;; TODO: need better error handling for when value producess an error.
(defmacro! opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let ((value-var (gensym "value")))
    `(if (not (boundp ',symbol))
         (push '(lambda () (opt! ,symbol ,value))
               (gethash ',symbol oo-after-load-hash-table))
       (let ((,value-var (with-demoted-errors "Error: %S" ,value)))
         (aif (get ',symbol 'custom-set)
             (funcall it ',symbol ,value-var)
           (with-no-warnings (setq ,symbol ,value-var)))))))
;;; provide
(provide 'oo-base-macros)
;;; oo-base-macros.el ends here
