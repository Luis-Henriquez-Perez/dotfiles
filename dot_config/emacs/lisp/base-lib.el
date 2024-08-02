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
(require 'lgr)
(eval-when-compile (require 'anaphora))
(eval-when-compile (require 'base-macros))

(defvar evil-state-properties)
;;;; logging
;; TODO: figure out how to change the log format
;; I do not really utilize the logging enough yet because I need to understand
;; `lgr' more.  I considered removing the package, but I still got it to work.
;; And logging a little is better than nothing.
(defvar oo-lgr (progn! (set! logger (lgr-get-logger "oo"))
                       (set! log-buffer (get-buffer-create "*lgr*"))
                       (lgr-add-appender logger (lgr-appender-buffer :buffer log-buffer)))
  "Object used for logging.")

(defmacro info! (msg &rest meta)
  `(lgr-info oo-lgr ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-lgr ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-lgr ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-lgr ,msg ,@meta))
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
;;;;; oo-advice-how
(defvar oo-advice-how-alist '((BF . :before)
                              (AF . :after)
                              (AR . :around)
                              (OV . :override)
                              (AU . :after-until)
                              (BU . :before-until)
                              (FA . :filter-args)
                              (FR . :filter-return))
  "An alist of (HOW-ABBREV . HOW).
HOW is the same as in `advice-add'.  HOW-ABBREV is the abbreviation used in
advice names for HOW.")
;;;;; oo-advice-components
(defun! oo-advice-components (fsym)
  "Return a list of."
  (set! rx "\\(?:\\([^[:space:]]+\\)@\\(\\(?:A[FRU]\\|B[FU]\\|F[AR]\\|OV\\)\\)\\([^[:space:]]+\\)\\)")
  (set! name (symbol-name fsym))
  (flet! group (-compose #'intern (-rpartial #'match-string name)))
  (awhen (string-match rx name)
    (mapcar #'group (number-sequence 1 3))))
;;;;; add-advice
(defun! oo-add-advice (symbol how fsym &optional props)
  "Generate a new advice."
  (set! how-name (car (rassoc how oo-advice-how-alist)))
  (aprog1 (intern (format "%s@%s%s" symbol how-name fsym))
    (fset it fsym)
    (advice-add symbol how it props)))
;;;; hooks
;;;;; oo-add-hook
;; No anonymous hooks allowed.
(cl-defun oo-add-hook (hook fsym &key append depth local)
  "Generate a function from FSYM and add it to HOOK.
Unlike `add-hook'."
  (aprog1 (intern (format "%s&%s" hook fsym))
    (fset it (oo-report-error-fn fsym))
    (add-hook hook it (or append depth) local)))
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
