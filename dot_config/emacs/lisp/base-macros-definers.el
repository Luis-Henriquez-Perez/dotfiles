;;; base-macros-definers.el --- defining macros -*- lexical-binding: t; -*-
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
;; Define definer macros.
;;
;;; Code:
(require 'base-macros-autolet)

(defun oo--arglist-symbols (arglist)
  "Return non-nil if symbol is an argument key."
  (let (symbols)
    (dolist (item (flatten-list arglist))
      (when (and (symbolp item) (not (string-match "^&" (symbol-name item))))
        (push item symbols)))
    (nreverse symbols)))

(defun oo--definer-components (args)
  ;; "Return (name arglist metadata body) from ORIGINAL-BODY."
  (let ((name (pop args))
        (arglist (pop args))
        (doc (and (stringp (car args)) (pop args)))
        (decl (and (equal 'declare (car-safe (car args))) (pop args)))
        (inte (and (equal 'interactive (car-safe (car args))) (pop args))))
    (list name arglist (cl-remove-if #'null (list doc decl inte)) args)))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `autolet!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (cl-destructuring-bind (name arglist metadata body) (oo--definer-components args)
    `(defmacro ,name ,arglist
       ,@metadata
       (autolet! :noinit ,(oo--arglist-symbols arglist)
                 ,@body))))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `autolet!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (cl-destructuring-bind (name arglist metadata body) (oo--definer-components args)
    `(defun ,name ,arglist
       ,@metadata
       (autolet! :noinit ,(oo--arglist-symbols arglist)
                 ,@body))))
;;; provide
(provide 'base-macros-definers)
;;; base-macros-definers.el ends here
