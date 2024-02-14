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
(require '01-base-lib)
(require 'anaphora)
(require 'dash)
;;; hooks
;;;; reporting errors
(defun oo-report-error (fn error)
  "Register ERROR and FN in `oo-errors'."
  ;; (lgr-info oo-lgr "Recieved error")
  (push (cons fn error) oo-errors))

(defun oo-report-error-fn (symbol function)
  "Return a function that will report error instead of raising an error."
  (oo-condition-case-fn fn :action (apply-partially #'oo-report-error fn)))
;;;; oo-add-hook
;; No anonymous hooks allowed.
(defun oo-add-hook (hook fsym &optional depth local)
  "Generate a hook and add it.
Unlike `add-hook' requires a."
  ;; Conditional based on hook.
  ;; (pcase args
  ;;   (`(,(and (pred symbolp) hook))
  ;;    (add-hook '))
  ;;   (`())
  ;;   (_ ))
  (aprog1 (oo-into-symbol hook '& fsym)
    (fset it fsym)
    (add-hook hook it depth local)))
;;;; remove hook
(defun oo-remove-hook (fsym &optional hook)
  "Remove FSYM from HOOK."
  (if (and function hook)
      (remove-hook hook fsym)
    (remove-hook (oo-get-hook fsym) fsym)))
;;;; oo-hook-p
(defalias 'oo-hook-p 'oo-get-hook "Return non-nil if FSYM is a hook symbol.")
;;;; oo-get-hook
(defun oo-get-hook (fym)
  "Return the hook corresponding to FSYM."
  (alet (symbol-name fsym)
    (string-match "\\(.+\\)&\\(.+\\)" it)
    (match-string 1)))
(defalias 'oo-hook 'oo-get-hook)
;;;; defhook!
(defmacro defhook! (name args &rest body)
  "Add function to hook as specified by NAME.
NAME should be a hook symbol."
  (let! hook (oo-get-hook name))
  (cl-assert hook t "%s is not a hook symbol")
  `(prog1 name
     (fset name ,)
     (add-hook hook name)))
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
;;; provide
(provide '04-base-custom)
;;; 04-base-custom.el ends here
