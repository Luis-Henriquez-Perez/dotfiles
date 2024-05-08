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
(require 'oo-init)
(require 'oo-after-load)
(require 'oo-autoloads)
;;; provide init
(provide 'init)
;;; init.el ends here
