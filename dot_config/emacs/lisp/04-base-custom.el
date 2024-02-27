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
;;; oo-first-success
;; This function is very similar to dash's [[file:snapshots/_helpful_function__-first_.png][-first]] or cl-lib's [[file:snapshots/_helpful_function__cl-find-if_.png][cl-find-if]].
;; These functions take a predicate and a list and they return the first element of
;; the list for which ~(pred element)~ returns non-nil.  The function =oo-first-success= also takes a
;; predicate and the list, but instead it returns the first non-nil return value of
;; ~(pred element)~.  For example, ~(oo-first-sucess 'numberp '(a t 0))~ would return
;; =t= instead of =0= as it would for =-first= or =cl-find-if= because ~(numberp 0)~ evaluates
;; to =t=. The name of this function is inspired by a similar function designed for
;; hooks [[file:snapshots/_helpful_function__run-hooks-with-args-until-success_.png][run-hook-with-args-until-success]].
(defun! oo-first-success (fn list)
  "Return the first non-nil (fn x) in LIST, else nil."
  (--each-while list (not (set! success (funcall fn it))))
  success)
;;; logging
(defvar oo-lgr (lgr-add-appender (lgr-get-logger "oo") (lgr-appender-buffer :buffer "*Messages"))
  "Object used for logging.")

(defmacro info! (msg &rest meta)
  `(lgr-info oo-lgr ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-lgr ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-lgr ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-lgr ,msg ,@meta))
;;; autoloading
;; I tried making this as a macro called [[][catch-autoloads!]] but I ran in to
;; some issues.  First, my initial implementation ran into infinite recursion
;; during macroexpansion.  And then after I fixed that I had problems with lexical
;; binding.
(defun! oo-autoload-function (fn)
  "Call FN with ARGS trying to load features of any undefined symbols.
If an void-function or void-variable error is raised try to guess the parent
feature."
  (oo-condition-case-fn fn :handlers '(void-function void-variable) :action #'oo-autoload-action-function))

(defun! oo-autoload-action-function (error function args)
  "Try guess if feature is bound.
ERROR is either a void-variable or void-function error."
  (set! (type symbol . rest) error)
  (cl-assert (member type '(void-variable void-function)))
  (set! bound-fn (cl-case type (void-variable #'boundp) (void-function #'fboundp)))
  (set! candidates (oo-candidate-features symbol))
  (for! (feature candidates)
    (require feature nil t)
    (when (funcall bound-fn symbol)
      (return! (apply #'oo-funcall-autoload function args))))
  (signal (car error) (cdr error)))
;;; reporting errors
(defun oo-report-error (fn error)
  "Register ERROR and FN in `oo-errors'."
  (error! "%s raised an %s error because of %s" fn (car error) (cdr error))
  (cl-pushnew (cons fn error) oo-errors :key #'car))

(defun oo-report-error-fn (fn)
  "Return a function that will report error instead of raising an error."
  (oo-condition-case-fn fn :action (lambda (e &rest _) (oo-report-error fn e))))
;;; advices
;; Advices will be named advisee@ADVICE-ABBREVwhat-advice-does.
;;;; oo-advice-how 
(defvar oo-advice-how-alist '((BF . :before)
                              (AF . :after)
                              (AR . :around)
                              (OV . :override)
                              (AU . :after-until)
                              (BU . :before-until)
                              (FA . :filter-args)
                              (FR . :filter-return))
  "An alist whose elements are of the form.")
;;;; oo-advice-components
(defun! oo-advice-components (fsym)
  ;; "Return a list of."
  (set! rx (rx-to-string `(seq (group (one-or-more (not space)))
                               "@"
                               (group (or ,@(mapcar (-compose #'symbol-name #'car) oo-advice-how-alist)))      
                               (group (one-or-more (not space))))))
  (awhen (string-match regexp (symbol-name fsym))
    (list (intern (match-string 1))
          (intern (match-string 2))
          (match-string 3))))
;;;; oo-advised
(defun! oo-advised (fsym)
  "Return the advised function symbol for FSYM."
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp fsym))
  (aset! (symbol-name fsym))
  (when (string-match regexp it)
    (intern (match-string 1 it))))
;;;; add-advice
(defun! oo-add-advice (symbol how fsym &optional props)
  "Generate a new advice."
  (set! how-name (rassoc how oo-advice-how-alist))
  (aprog1 (intern (format "%s@%s%s" symbol how-name fsym))
    (fset it fsym)
    (advice-add symbol how it)))
;;;; defadvice! 
(defmacro! defadvice! (name args &rest body)
  "Define an advice."
  (set! (symbol how-name _) (oo-advice-components name))
  ;; (cl-assert )
  (set! how (assoc how-name oo-advice-how-alist))
  ;; (when (vectorp (car body))
  ;;   (aset! (append (pop body) nil))
  ;;   (set! params (list (map-elt it :depth)
  ;;                      (map-elt it :local))))
  `(progn
     (fset name (lambda ,args ,@body))
     (advice-add symbol how name)))
;;;; silently 
(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (shut-up (apply fn args)))
;;; hooks
;;;; oo-add-hook
;; No anonymous hooks allowed.
(defun! oo-add-hook (hook fsym &optional depth local)
  "Generate a function from FSYM and add it to HOOK.
Unlike `add-hook'."
  (aprog1! (intern (format "%s&%s" hook fsym)))
  (fset it (oo-report-error-fn fsym))
  ;; (fset it fsym)
  (add-hook hook it depth local))
(defalias 'oo-generate-hook 'oo-add-hook)
(defalias 'oo-gen-hook 'oo-add-hook)
;;;; oo-remove-hook
(defun oo-remove-hook (fsym &optional hook)
  "Remove FSYM from HOOK."
  (if (and function hook)
      (remove-hook hook fsym)
    (remove-hook (oo-hook fsym) fsym)))
;;;; oo-hook
(defun! oo-hook (fsym)
  "Return the hook symbol for FSYM."
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp fsym))
  (alet! (symbol-name fsym))
  (when (string-match "\\(.+\\)&.+" it)
    (intern (match-string 1 it))))
;;;; oo-hook-p
(defalias 'oo-hook-p 'oo-hook "Return non-nil if FSYM is a hook symbol.")
;;;; defhook!
(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME.
NAME should be a hook symbol."
  (set! hook (oo-hook name))
  (cl-assert hook t "%s is not a hook symbol" hook)
  (when (vectorp (car body))
    (aset! (append (pop body) nil))
    (set! params (list (map-elt it :depth)
                       (map-elt it :local))))
  `(prog1 ',name
     (fset ',name (lambda ,args (block! ,@body)))
     (add-hook ',hook ',name ,@params)))
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
;; I'll note that I push all the forms into a list and evaluate them all in the
;; body of one lambda as opposed to evaluating one lambda per form.  This is
;; important because lambda calls have an overhead that adds up.  It is far less
;; costly to invoke one lambda over N lambdas.
(defhook! after-load-functions&set-bound-symbols (&rest _)
  "Set symbols that have been bound to the result of their corresponding expr.
Check each symbol in `oo-unbound-symbol-alist', removing those that have already been
bound and setting them to the result of evaluating expr."
  (for! ((&as elt (symbol . expr)) oo-unbound-symbol-alist)
    (cond ((boundp symbol)
           (pushing! exprs `(set! ,symbol ,expr)))
          (t
           (pushing! updated elt))))
  (setq oo-unbound-symbol-alist (nreverse updated))
  (when exprs (funcall `(lambda () ,@exprs))))

;; (add-hook 'after-load-functions #'after-load-functions&set-bound-symbols)
;;; popup
;; I don't yet know where to put this function.  So for now, here it goes.
(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (alet `(,regexp
          (display-buffer-at-bottom)
          (side bottom)
          (slot 1)
          (window-height 0.5)
          (window-parameters ((no-other-window t))))
    (push it display-buffer-alist)))
;;; customize variables
;;;; oo-unbound-symbol-alist
(defvar oo-unbound-symbol-alist nil
  "An alist mapping an unbound symbol to an expression.
This alist is checked by the hook `after-load-functions&set-bound-symbols' for
any symbols that are now bound.")
;;;; opt!
(defmacro! opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (gensym! value-var)
  `(if (not (boundp ',symbol))
       (push (cons ',symbol ',value) oo-unbound-symbol-alist)
     (let ((,value-var ,value))
       (aif (get ',symbol 'custom-set)
           (funcall it ',symbol ,value-var)
         (with-no-warnings (setq ,symbol ,value-var))))))
;;; provide
(provide '04-base-custom)
;;; 04-base-custom.el ends here
