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
(require 'base-macros-with-map)
(require 'base-macros-block)

(defun oo--arglist (arglist)
  "Return the list of argument symbols in ARGLIST."
  (cl-remove-if #'oo-list-marker-p (flatten-tree arglist)))

(defun oo--declaration-p (form)
  "Return non-nil if FORM is a declaration form."
  (equal 'declare (car-safe form)))

(defun oo--interactive-p (form)
  "Return non-nil if FORM is an interactive form."
  (equal 'interactive (car-safe form)))

(defun oo--definer-components-1 (body)
  "Return (documentation decl interactive-form body) from ORIGINAL-BODY."
  (pcase body
    (`(,(and (pred stringp) doc)
       ,(and (pred oo--declaration-p) decl)
       ,(and (pred oo--interactive-p) int) . ,rest)
     (list doc decl int rest))
    (`(,(and (pred stringp) doc)
       ,(and (pred oo--declaration-p) decl) . ,rest)
     (list doc decl nil rest))
    (`(,(and (pred stringp) doc)
       ,(and (pred oo--interactive-p) int) . ,rest)
     (list doc nil int rest))
    (`(,(and (pred stringp) doc) . ,rest)
     (list doc nil nil rest))
    (`(,(and (pred oo--declaration-p) decl)
       ,(and (pred oo--interactive-p) int) . ,rest)
     (list nil decl int rest))
    (`(,(and (pred oo--declaration-p) decl) . ,rest)
     (list nil decl nil rest))
    (`(,(and (pred oo--interactive-p) int) . ,rest)
     (list nil nil int rest))
    (_
     (list nil nil nil body))))

(defun oo--definer-components (args)
  "Return a plist corresponding to the components of `defun'."
  (let* ((name (pop args))
         (arglist (pop args))
         (body args)
         (keys (list :docstring :declaration :interactive :body)))
    (append (list :name name :arglist arglist)
            (-interleave keys (oo--definer-components-1 body)))))

(defun oo--prognify-components (components)
  "Return the form for DEFINER.
Meant to be used in `defmacro!' and `defun!'."
  (let! ((body (map-elt components :body))
         (arglist (map-elt components :arglist)))
    (setf (map-elt components :body)
          (list (oo--generate-block-body body nil (oo--arglist arglist))))
    components))

(defun oo--finalize-components (components)
  "Return the values of."
  (with-map-keywords! components
    (let ((metadata (-non-nil (list !docstring !declaration !interactive))))
      `(,!name ,!arglist ,@metadata ,@!body))))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `block!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  `(defmacro ,@(thread-last (oo--definer-components args)
                            (oo--prognify-components)
                            (oo--finalize-components))))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `block!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  `(defun ,@(thread-last (oo--definer-components args)
                         (oo--prognify-components)
                         (oo--finalize-components))))
;;; provide
(provide 'base-macros-definers)
;;; base-macros-definers.el ends here
