;;; base-macros-advice.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; defadvice!
(defmacro advice! (symbol where fn)
  `(progn (defun name ()
            ())
          (advice-add)))

(defmacro! defadvice! (name args &rest body)
  "Define an advice."
  (declare (indent defun))
  (set! (symbol how-name _) (oo-advice-components name))
  (set! how (cdr (assoc how-name oo-advice-how-alist)))
  `(progn
     (fset ',name (lambda ,args (progn! ,@body)))
     (advice-add ',symbol ,how ',name)))
;;; provide
(provide 'base-macros-advice)
;;; base-macros-advice.el ends here
