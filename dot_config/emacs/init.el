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
;;;;; values
(defvar oo-old-values nil
  "A plist of whose keys are symbols and values are their old.")
;;;;; set startup variables
;;;;;; disable garbage collection until I'm done with startup
;; This variable controls how often.  Setting it to =most-positive-fixnum=, a
;; very big number, essentially disables garbage collection.  The garbage
;; collection is later reset to a reasonable value.
(setf (plist-get oo-old-values 'gc-cons-threshold) gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; This is the percentage of the heap before.
(setf (plist-get oo-old-values 'gc-cons-percentage) gc-cons-percentage)
(setq gc-cons-percentage 0.8)
;;;;;; don't search for whenever a package is loaded
(setf (plist-get oo-old-values 'file-name-handler-alist) file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
(setf (plist-get oo-old-values 'mode-line-format) mode-line-format)
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
(defhook! emacs-startup-hook&restore-startup-values ()
  [:depth 91]
  (setq gc-cons-threshold (plist-get oo-old-values 'gc-cons-threshold))
  (setq gc-cons-percentage (plist-get oo-old-values 'gc-cons-percentage))
  (setq file-name-handler-alist (plist-get oo-old-values 'file-name-handler-alist))
  (setq-default mode-line-format (plist-get oo-old-values 'mode-line-format)))
;;;; load autoloads
(require 'oo-autoloads)
;;; provide init
(provide 'init)
;;; init.el ends here
