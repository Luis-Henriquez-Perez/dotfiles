;;; base-macros-after-load.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
;;;; requirements
(require 'base-macros-definers)
;;;; after!
(defmacro! after! (expr &rest body)
  "Similar to `with-eval-after-load'."
  (declare (indent 1))
  `(oo-call-after-load ',expr (lambda () (with-no-warnings ,@body))))

;; I want to enforce named after blocks as opposed to forms.  If you use a
;; named function, you can always advice the function or override it before it
;; is evaluated.
(defmacro! defafter! (name expr &rest body)
  "Define a function to be called when EXPR is satisfied."
  (declare (indent defun))
  `(progn
     (defun ,name () ,@body)
     (oo-call-after-load ',expr #',name)))

(defmacro! require! (&rest args)
  "Require FEATURE after EXPR is met.
If no EXPR is provided and FEATURE is a configuration file derive expression
from feature name."
  (flet! config-feature (symbol)
    (alet (symbol-name symbol)
      (string-match "\\`config-\\([^[:space:]]+\\)\\'" it)
      (intern (match-string 1 it))))
  (cond ((null (nthcdr 1 args))
         (set! config-file (car args))
         (set! expr (config-feature config-file))
         (set! feature config-file))
        ((null (nthcdr 2 args))
         (set! (expr feature) args))
        (t
         (error "Invalid arguments")))
  (set! name (intern (format "oo--require-%S" feature)))
  `(progn (unless (fboundp ',name)
            (defun ,name ()
              (require ',feature nil t nil)))
          (oo-call-after-load ',expr #',name)))

(defmacro! setq-hook! (hooks symbol value)
  "Set the local value of hook."
  (set! name (intern (format "set-%s" symbol)))
  `(defhook! ,name ,(ensure-list hooks)
     ,(format "Set local variable %S locally." symbol value)
     (setq-local ,symbol ,value)))
;;; provide
(provide 'base-macros-after-load)
;;; base-macros-after-load.el ends here
