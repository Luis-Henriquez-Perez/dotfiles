;;; base-utils.el -*- lexical-binding: t; -*-
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
;; This file contains utility functions.
;;
;;; Code:
;;;; requirements
(require 'cl-lib)
(require 'subr)

(defsubst oo-true-symbol-p (object)
  "Return non-nil if OBJECT is a non-keyword symbol."
  (declare (pure t) (side-effect-free error-free))
  (and object (symbolp object) (not (keywordp object))))

(defsubst oo-cons-cell-p (object)
  "Return non-nil if OBJECT is a cons-cell but not a proper list."
  (declare (pure t) (side-effect-free error-free))
  (and (listp object) (not (listp (cdr-safe object)))))

(defsubst oo-true-list-p (object)
  "Return non-nil if OBJECT is a non-nil proper-list.
This means it is non-nil."
  (declare (pure t) (side-effect-free error-free))
  (and object (listp object) (listp (cdr-safe object))))

(defsubst oo-negative-p (number)
  "Return non-nil if NUMBER is less than zero."
  (declare (pure t) (side-effect-free error-free))
  (< number 0))

(defsubst oo-positive-p (number)
  "Return non-nil if NUMBER is greater than zero."
  (declare (pure t) (side-effect-free error-free))
  (> number 0))

(defsubst oo-contains-all-p (list1 list2)
  "Return non-nil if"
  (declare (pure t) (side-effect-free error-free))
  (null (cl-set-difference list1 list2)))

(defsubst oo-same-items-as-p (list1 list2)
  "Return non-nil if LIST1 has the same items as LIST2"
  (declare (pure t) (side-effect-free error-free))
  (and (null (cl-set-difference list1 list2))
       (null (cl-set-difference list2 list1))))

(defun oo-into-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defun oo-into-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-into-string args)))

(defun oo-into-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-into-symbol ":" args))

(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms to be wrapped.  WRAPPERS are a list of forms
representing the wrappers to apply.  If WRAPPERS is empty, `progn' is added to
ensure the result is syntactically valid."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (append wrapper (list forms))))
  forms)

(defun oo-quoted-p (form)
  "Return non-nil if FORM is quoted."
  (declare (pure t) (side-effect-free t))
  (equal (car-safe form) 'quote))

(defun oo-sharpquoted-p (form)
  "Return non-nil if form is sharpquoted."
  (declare (pure t) (side-effect-free t))
  (equal (car-safe form) 'function))

(defun oo-ensure-quote (form)
  "Return quoted form unquoted, otherwise return form."
  (declare (pure t) (side-effect-free t))
  (if (oo-quoted-p form) form (macroexp-quote form)))

;; This function is used by captain and abbrev.
(defun oo-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment.
Specifically, return the symbol `string' if point is in a string, the symbol
`comment' if in a comment and nil otherwise."
  (declare (pure t) (side-effect-free t))
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))

(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (quietly! (apply fn args)))
;;; provide
(provide 'base-utils)
;;; base-utils.el ends here
