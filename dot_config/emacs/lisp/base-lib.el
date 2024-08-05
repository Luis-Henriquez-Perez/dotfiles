;;; base-lib.el --- external package library -*- lexical-binding: t; -*-
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
;; Compared to `base-utils' this library has functions that on external
;; packages loaded in `base-requirements'.

;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;
;;; Code:
;;;; requirements
(require 'base-utils)
(require 'init-elpaca)
(require 'dash)
(require 'shut-up)
(eval-when-compile (require 'anaphora))
(eval-when-compile (require 'base-macros))
;;;; reporting errors
(defun oo-report-error (fn error)
  "Register ERROR and FN in `oo-errors'."
  (error! "%s raised an %s error because of %s" fn (car error) (cdr error))
  (cl-pushnew (cons fn error) oo-errors :key #'car))

(defun oo-report-error-fn (fn)
  "Return a function that will report error instead of raising it."
  (oo-condition-case-fn fn (lambda (e &rest _) (oo-report-error fn e))))
;;;; silently
(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (shut-up (apply fn args)))
;;;; advices
;; Advices will be named advisee@ADVICE-ABBREVwhat-advice-does.
;;;;; oo-generate-advice
(defun oo-generate-advice (how symbol suffix body-fn &optional props)
  "Generate and add an advice to SYMBOL."
  (set! name (intern (format "%s@%s" symbol suffix)))
  (defalias name
    `(lambda (&rest args)
       (info! "Running advice %s..." ',name)
       (funcall #',body-fn args)))
  (advice-add symbol how name props)
  name)
;;;;; oo-add-advice
(defun! oo-add-advice (symbol how function &optional props)
  "Generate a new advice and add it to SYMBOL. "
  (set! suffix (if (symbolp function) function 'anonymous-advice))
  (oo-generate-advice how symbol suffix function props))
;;;; hooks
;;;;; oo-hook-symbol-p
(defun! oo-hook-symbol-p (symbol)
  "Return non-nil if SYMBOL is a hook symbol."
  (declare (pure t) (side-effect-free t))
  (when (symbolp symbol)
    (set! name (symbol-name symbol))
    (string-match-p (rx (1+ (not white)) "-hook" eos) name)))
;;;;; oo-generate-hook
;; I am hesitant about having the `oo-generate-hook' both generate the fn
;; that produces the hook and add it to the hook, but as of yet I do not see a
;; reason not to have it do this.  In other words, I cannot imagine a case where
;; I would be using this function and not adding a hook.  If that changes I can
;; just change this function.
(defun! oo-generate-hook (hook suffix body-fn depth local)
  "Generate a hook function from HOOK, SUFFIX and BODY-FN."
  (set! name (intern (format "%s&%s" hook suffix)))
  (defalias name
    `(lambda (&rest args)
       (info! "Running hook %s..." ',name)
       (condition-case err
           (funcall #',body-fn args)
         (error (if oo-debug-p
                    (signal (car err) (cdr err))
                  (message "Error calling %s in %s because of %s"
                           ',name
                           (car err)
                           (cdr err)))))))
  (add-hook hook name depth local)
  name)
;;;;; oo-add-hook
;; No anonymous hooks allowed.
(cl-defun oo-add-hook (hook fn &key append depth local)
  "Generate a function from fn and add it to HOOK.
Unlike `add-hook'."
  (oo-generate-hook hook fn fn (or append depth) local))
;;;;; oo-remove-hook
(defun oo-remove-hook (fsym &optional hook)
  "Remove FSYM from HOOK."
  (if (and fsym hook)
      (remove-hook hook fsym)
    (remove-hook (oo-hook fsym) fsym)))
;;;; popup
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
;;; provide
(provide 'base-lib)
;;; base-lib.el ends here
