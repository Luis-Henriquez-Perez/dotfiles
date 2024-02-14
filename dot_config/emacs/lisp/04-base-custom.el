;;; 04-base-custom.el --- Tools to config features -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 02 Jan 2024
;;
;; URL: https://github.com/Luis-Henriquez-Perez/dotfiles
;;
;; License: GPLv3
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;
;;; Code:
(require '02-base-lib)
(require 'anaphora)
(require 'dash)
(require 'lgr)
;;; logging
(defmacro info! (msg &rest meta)
  `(lgr-info oo-lgr ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-lgr ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-lgr ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-lgr ,msg ,@meta))
;;; hooks
;;;; reporting errors
(defun oo-report-error (fn error)
  "Register ERROR and FN in `oo-errors'."
  (error! "%s raised an %s error because of %s" fn (car error) (cdr error))
  (cl-pushnew oo-errors (cons fn error) :key #'car))

(defun oo-report-error-fn (symbol function)
  "Return a function that will report error instead of raising an error."
  (oo-condition-case-fn fn :action (apply-partially #'oo-report-error fn)))
;;;; oo-add-hook
;; No anonymous hooks allowed.
(defun oo-add-hook (hook fsym &optional depth local)
  "Generate a function from FSYM and add it to HOOK.
Unlike `add-hook'."
  (aprog1 (oo-into-symbol hook '& fsym)
    (fset it (oo-report-error-fn fsym))
    (add-hook hook it depth local)))
;;;; remove hook
(defun oo-remove-hook (fsym &optional hook)
  "Remove FSYM from HOOK."
  (if (and function hook)
      (remove-hook hook fsym)
    (remove-hook (oo-get-hook fsym) fsym)))
;;;; oo-hook
(defun oo-hook (fsym)
  "Return the hook symbol specified by FSYM."
  (declare (pure t) (side-effect-free t))
  (alet (symbol-name fsym)
    (string-match "\\(.+\\)&.+" it)
    (intern (match-string 1 it))))
;;;; oo-hook-p
(defalias 'oo-hook-p 'oo-hook "Return non-nil if FSYM is a hook symbol.")
;;;; defhook!
;; (defmacro defhook! (name args &rest body)
;;   "Add function to hook as specified by NAME.
;; NAME should be a hook symbol."
;;   (let! hook (oo-hook name))
;;   (cl-assert hook t "%s is not a hook symbol" hook)
;;   `(prog1 name
;;      (fset name ,)
;;      (add-hook hook name)))
;; (require 'lgr)
;; ;;; require
;; ;; This macro is to satisfy the compiler but also not have to list all the files
;; ;; I want.
;; ;; (defun oo--require-symbols (regexp dir)
;; ;;   (mapcar (-compose #'intern #'file-name-sans-extension)
;; ;;           (directory-files dir nil regexp)))

;; ;; (defmacro! require! (regexp)
;; ;;   (flet! files (directory-files dir nil regexp))
;; ;;   (let! body nil)
;; ;;   (let! symbols (funcall (-rpartial #'oo--require-symbols dir) regexp))
;; ;;   (for! (feature symbols)
;; ;;     (collecting! body `(require ',feature)))
;; ;;   (macroexp-progn body))
;; ;;; logging
;; (defvar oo-lgr (lgr-add-appender (lgr-get-logger "oo") (lgr-appender-buffer :buffer oo-log-buffer))
;;   "Object used for logging.")
;; ;;;; advice
;; ;;;; expire
;; (defun oo-set-expire (fsym &optional when-fn)
;;   "Set hook to expire."
;;   (pcase symbol
;;     ((pred oo-advice)
;;      (fset fsym (oo-after-fn advice #'oo-remove-advice fsym)))
;;     ((pred oo-hook)
;;      (fset fsym (oo-after-fn hook #'oo-remove-hook fsym)))
;;     (_ (error "%s fsym is neither a hook nor a an advice"))))
;; ;;;; generate advice function
;; (defun oo-get-advised (fsym)
;;   "Return advised.")

;; (defalias 'oo-advised 'oo-get-advised)
;; (defalias 'oo-advice-p 'oo-get-advice)

;; (defun oo-remove-advice ())

;; (defun oo-add-advice ()
;;   "Add "
;;   )
;; ;;; opt!
;; ;;;; set unbound symbols
;; (defvar oo-unbound-symbol-alist nil
;;   "An alist mapping an unbound symbol to an expression.
;; This alist is checked by the hook `after-load-functions&set-bound-symbols' for
;; any symbols that are now bound.")

;; ;; I'll note that I push all the forms into a list and evaluate them all in the
;; ;; body of one lambda as opposed to evaluating one lambda per form.  This is
;; ;; important because lambda calls have an overhead that adds up.  It is far less
;; ;; costly to invoke one lambda over N lambdas.
;; (defun! after-load-functions&set-bound-symbols (&rest _)
;;   "Set symbols that have been bound to the result of their corresponding expr.
;; Check each symbol in `oo-unbound-symbol-alist', removing those that have already been
;; bound and setting them to the result of evaluating expr."
;;   (for! ((&as elt (symbol . expr)) oo-unbound-symbol-alist)
;;     (cond ((boundp symbol)
;;            (pushing! exprs `(set! ,symbol ,expr)))
;;           (t
;;            (pushing! updated elt))))
;;   (setq oo-unbound-symbol-alist (nreverse updated))
;;   (when exprs (funcall `(lambda () ,@exprs))))

;; (add-hook 'after-load-functions #'after-load-functions&set-bound-symbols)
;; ;;;; opt!
;; (defmacro! opt! (symbol value)
;;   "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
;; This is like `setq' but it is meant for configuring variables."
;;   (let! value-var (make-symbol "value"))
;;   `(if (not (boundp ',symbol))
;;        (push (cons ',symbol ',value) oo-unbound-symbol-alist)
;;      (let ((,value-var ,value))
;;        (aif (get ',symbol 'custom-set)
;;            (funcall it ',symbol ,value-var)
;;          (with-no-warnings (setq ,symbol ,value-var))))))
;;; provide
(provide '04-base-custom)
;;; 04-base-custom.el ends here
