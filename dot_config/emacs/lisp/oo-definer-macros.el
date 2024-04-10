;;; oo-definer-macros.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-progn-macro)
;;;; defmacro! and defun!
(defun oo-defun-components (body &optional show-nils)
  "Divide defun body, BODY, into its components.
The return value should be of the form ((docstring declarations interactive)
body)."
  (let (doc decls int)
    (setq doc (when (stringp (car-safe body)) (pop body)))
    (setq decls (when (equal 'declare (car-safe (car-safe body))) (pop body)))
    (setq int (when (equal 'interactive (car-safe (car-safe body))) (pop body)))
    (list (cl-remove-if (if show-nils #'ignore #'null) (list doc decls int)) body)))

;; It is easier to tests functions that return a value rather than macros.  So I
;; prefer writing a helper that returns the macro body as data as opposed to
;; doing the expansion directly in the macro.
(defun oo--definer-body (definer definer-args)
  "Return the form for DEFINER.
Meant to be used in `defmacro!' and `defun!'."
  (let! (((name arglist . body) definer-args)
         ((metadata body) (oo-defun-components body))
         (symbols (cl-remove-if #'oo-list-marker-p (flatten-tree arglist))))
    (oo-wrap-forms `((,definer ,name ,arglist ,@metadata)
                     (block! (exclude! ,@symbols)))
                   body)))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `block!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (oo--definer-body 'defmacro args))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `block!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (oo--definer-body 'defun args))
;;; provide
(provide 'oo-definer-macros)
;;; oo-definer-macros.el ends here
