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
(require 'base-lib)
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
;;; provide
(provide 'base-macros-after-load)
;;; base-macros-after-load.el ends here
