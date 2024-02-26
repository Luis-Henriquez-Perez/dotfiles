;;; 10-init-gcmh.el --- initialize `gcmh' -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 14 Feb 2024
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
;; This is initialization code for `gcmh'.
;;
;;; Code:
(oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)

(opt! gcmh-idle-delay 'auto)

(opt! gcmh-high-cons-threshold (* 8 1024 1024))

(opt! gcmh-low-cons-threshold (* 4 1024 1024))

;; [[helpvar:minibuffer-setup-hook][minibuffer-setup-hook]] and [[helpvar:minibuffer-exit-hook][minibuffer-exit-hook]] are the hooks run just before
;; entering and exiting the minibuffer (respectively).  In the minibuffer I'll be
;; primarily doing searches for variables and functions.  There are alot of
;; variables and functions so this can certainly get computationally expensive.  To
;; keep things snappy I increase boost the [[helpvar:gc-cons-threshold][gc-cons-threshold]] just before I enter
;; the minibuffer, and restore it to it's original value a few seconds after it's closed.
(defvaralias 'minibuffer-enter-hook 'minibuffer-setup-hook)

(defhook! minibuffer-setup-hook&increase-garbage-collection ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold"
  (require 'gcmh)
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defhook! minibuffer-exit-hook&decrease-garbage-collection ()
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  :append t
  (require 'gcmh)
  (setq gc-cons-threshold gcmh-low-cons-threshold))
;;; provide
(provide '10-init-gcmh)
;;; 10-init-gcmh.el ends here
