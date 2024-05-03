;;; init.el --- My emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is my personal emacs configuration.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;;;; startup
;;;;; set startup variables
(defvar oo-symbol-values-alist nil)
(defun oo-record-value (symbol)
  (push (cons symbol (symbol-value symbol)) oo-symbol-values-alist))
(defun oo-restore-value (symbol)
  (set symbol (alist-get symbol oo-symbol-values-alist)))
;;;;;; disable garbage collection until I'm done with startup
;; This variable controls how often.  Setting it to =most-positive-fixnum=, a
;; very big number, essentially disables garbage collection.  The garbage
;; collection is later reset to a reasonable value.
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
;; This is the percentage of the heap before.
(setq gc-cons-percentage 0.8)
;;;;;; don't search for whenever a package is loaded
(oo-record-value 'file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
(setq-default mode-line-format nil)
;;;; set load-path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;;;; load requirements
(require '00-base-vars)
(require '01-base-settings)
(require '03-init-elpaca)
(require 'oo-utilities)
(eval-when-compile (require 'oo-macros))
(require 'shut-up)
(require '04-base-custom)
(require '05-base-bind)
(require '98-init-features)
(require '90-keybindings)
;;;; lazy-load after-load files
;; The "after-load" files contain customizations that should be enabled after a
;; package has been loaded.  These files are usually more meaty than the init
;; files.
(defhook! emacs-startup-hook&register-config-files ()
  "Load the code for the lisp files."
  [:depth 10]
  (set! regexp "\\`99-after-load-\\(.+\\)")
  (flet! feature-name (-compose #'file-name-sans-extension #'file-name-nondirectory))
  (flet! feature (-compose #'intern #'feature-name))
  (flet! package (file) (alet (feature-name file) (string-match regexp it) (intern (match-string 1 it))))
  (for! (file (directory-files (locate-user-emacs-file "lisp/") t regexp))
    (set! feature (feature file))
    (set! package (package file))
    ;; TODO: require features only if the have not been required yet to avoid
    ;; loading them twice.  Most of the time it should not matter but better to
    ;; be precise.
    (oo-call-after-load package #'require feature)))
;;;; restore startup values
;; This should be either the last or close to the last thing that happens.
;; Furthermore, its important that this hook is run and therefore that previous
;; hooks dont produce an error.
(defun! oo-lower-garbage-collection ()
  "Lower garbage collection until it reaches default values."
  ;; This is a sanity check to ensure.
  (cl-assert (zerop (% gc-cons-threshold (* 4 1024 1024))))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo-lower-garbage-collection)
    (cl-decf gc-cons-threshold (* 4 1024 1024))
    (cl-decf gc-cons-percentage 0.1)
    (cond ((= gc-cons-threshold (* 8 1024 1024))
           ;; (message "Done...gc-cons-threshold -> 8MB")
           (setq gc-cons-percentage 0.4))
          (t
           (run-with-timer 5 nil #'oo-lower-garbage-collection)))))

(defhook! emacs-startup-hook&restore-startup-values ()
  [:depth 91]
  (oo-restore-value 'file-name-handler-alist)
  (setq gc-cons-threshold (* 32 1024 1024))
  (run-with-timer 5 nil #'oo-lower-garbage-collection)
  (require 'oo-modeline))

;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defhook! minibuffer-setup-hook&increase-garbage-collection ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  [:depth 10]
  (oo-record-value 'gc-cons-threshold)
  (oo-record-value 'gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(defhook! minibuffer-exit-hook&decrease-garbage-collection ()
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  [:depth 90]
  (oo-restore-value 'gc-cons-threshold)
  (oo-restore-value 'gc-cons-percentage))
;;;; load autoloads
(require 'oo-autoloads)
;;; provide init
(provide 'init)
;;; init.el ends here
