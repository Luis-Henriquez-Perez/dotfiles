;;; base-macros.el -*- lexical-binding: t; -*-
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
(require 'base-macros-ing)
(require 'base-macros-advice)
(require 'base-macros-hook)
(require 'base-macros-bind)
(require 'base-macros-loop)
(require 'base-macros-let)
(require 'base-macros-lef)
(require 'base-macros-progn)
(require 'base-macros-with-map)
(require 'base-macros-definers)
;;;;; opt!
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
;;;;; alt!
(defmacro alt! (old new feature)
  `(progn (push (lambda (&rest _) (when (or (featurep ',feature) (require ',feature nil t)) ',new))
                (gethash ',old oo-alternate-commands))
          (define-key global-map [remap ,old] '(menu-item "" ,old :filter oo-alternate-command-choose-fn))));;; provide

(provide 'base-macros)
;;; base-macros.el ends here
