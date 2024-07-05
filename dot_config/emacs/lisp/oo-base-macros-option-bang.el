;;; oo-base-macros-option-bang.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; The purpose of this macro is multifold.  First, I do not want to have to
;; think about custom variables anymore.  If the variable I am setting is a
;; custom variable, then call the appropriate setter automatically.  Second,
;; record the variable I am binding, the time I am binding it as well as the
;; value it is being bound to.  This is useful for debugging purposes.  Finally,
;; I want to gracefully handle any error that is raised in the evaluation of the
;; value.  For me this means logging the error (if I am not running Emacs in
;; debug-mode or during init time).  I allow errors in session because it is
;; much easier to deal with those than.
;;
;;; Code:
;;;; requirements
(require 'oo-base-requirements)
;;;; helpers
(defun oo--let-bind (symbols values forms)
  "Return FORMS."
  `((let ,(cl-mapcar #'list symbols values) ,@forms)))

(defun! oo--option-bang-main-body (symbol-var value-var)
  "Return the form"
  (set! custom-setter (gensym "custom-setter"))
  `((let (,custom-setter)
      (cond ((setq ,custom-setter (get ,symbol-var 'custom-set))
             (info! "Custom set `%s' to %S" ,symbol-var ,value-var)
             ,(oo--option-condition-case-form `((funcall ,custom-setter ,symbol-var ,value-var))))
            (t
             (info! "Set `%s' to %S" ,symbol-var ,value-var)
             ,(oo--option-condition-case-form `((set ,symbol-var ,value-var))))))))

(defun oo--option-condition-case-form (forms)
  "Wrap form in `condition-case'."
  `(condition-case err
       (progn ,@forms)
     (error
      (if (or oo-debug-p after-init-time)
          (signal (car err) (cdr err))
        (error! "Failed to set %s to %s because of %S." (car err))
        (push err oo-errors)))))

(defmacro! option! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (set! symbol-var (gensym "symbol"))
  (set! value-var (gensym "value"))
  (set! body (oo--option-bang-main-body symbol-var value-var))
  `(let ((,symbol-var ',symbol))
     (if (not (boundp ,symbol-var))
         (push '(lambda () ,@(oo--let-bind (list symbol-var value-var) (list symbol value) body))
               (gethash ,symbol-var oo-after-load-hash-table))
       (let ((,value-var ,value))
         ,@body))))
;;; provide
(provide 'oo-base-macros-option-bang)
;;; oo-base-macros-option-bang.el ends here
