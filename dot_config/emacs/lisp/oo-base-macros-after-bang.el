;;; oo-base-macros-after-bang.el --- `with-eval-after-load' replacement -*- lexical-binding: t; -*-
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
;; This file provides a macro `after!' that is similar to `eval-after-load' with
;; just a few differences.  It ensures that is body is evaluated only once.
;; Moreover, it catches any errors in its body and adds the body into
;; `oo-errors'.  This is so that I can invoke the error later for debugging.
;; This only works with lexical-binding.
;;
;;; Code:
(require 'oo-base-lib)

(defun oo--after-bang-error (body)
  "Return form that produces error."
  (let ((eval-after-load-error (gensym "oo--eval-after-load-error-")))
    `((condition-case err
          ,(macroexp-progn body)
        (error
         (if oo-debug-p
             (signal (car err) (cdr err))
           (error! "Error %s in after block because of %s." (car err) (cdr err))
           (defun ,eval-after-load-error ()
             ,(macroexp-progn body))
           (push ',eval-after-load-error oo-errors)))))))
;;;; once-only-function
(defun oo--once-only-lambda (body)
  "Return a function that evaluates BODY only the first time it is called.
This function works only if `lexical-binding' is enabled."
  (let ((oo--first-call-p (gensym "oo--first-call-p")))
    `(let ((,oo--first-call-p t))
       (lambda ()
         (when ,oo--first-call-p
           (setq ,oo--first-call-p nil)
           ,(macroexp-progn body))))))
;;;; oo--after-bang-forms
(defun oo--after-bang-forms (expr body)
  "Return a form."
  (pcase expr
    ((pred null)
     body)
    ((and feature (pred symbolp))
     `((if (featurep ',feature)
           ,(macroexp-progn body)
         (with-eval-after-load ',feature ,(macroexp-progn body)))))
    (`(:or . ,exprs)
     (mapcan (-rpartial #'oo--after-bang-forms body) exprs))
    (`(:and . ,exprs)
     (oo--after-bang-forms exprs body))
    (`(,expr . ,exprs)
     (oo--after-bang-forms expr (oo--after-bang-forms exprs body)))
    (_
     (error "invalid expression `%S'" expr))))
;;;; after!
(defmacro after! (expr &rest body)
  "Evaluate BODY after EXPR is resolved.
If EXPR is null evaluate BODY immediately.  If it is a feature (symbol), evaluate BODY
after feature is loaded.  BODY is at most evaluated one time.  If body raises an
error and `oo-debug-p' is nil, log the error and add a function to `oo-errors'."
  (declare (indent defun))
  (cl-with-gensyms (once-only-lambda)
    `(let ((,once-only-lambda ,(oo--once-only-lambda (oo--after-bang-error body))))
       ,(macroexp-progn (oo--after-bang-forms expr `((funcall ,once-only-lambda)))))))
;;; provide
(provide 'oo-base-macros-after-bang)
;;; oo-base-macros-after-bang.el ends here
