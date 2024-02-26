;;; init.el --- Custom configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 02 Feb 2024
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
;; These are my personal Emacs configurations.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;;; emacs
;; For now I put everything in a single file.  The reason I choose to
;; do this is because it is simply easier for me with the knowledge I
;; have about compiling than having to consider loading order and
;;;; setup load-path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;;;; load base libraries
(require '00-base-vars)
(require '01-base-settings)
(require '02-base-lib)
(require '03-init-elpaca)
(require '04-base-custom)
(require '05-base-bind)
(require '06-base-leaders)
(require '10-init-gcmh)
(require '11-init-no-littering)
(require '12-init-benchmark-init)
(require '19-init-super-save)
(require '19-init-vertico)
(require '19-init-which-key)
(require '19-init-evil)
(require '19-init-dashboard)
(require '19-init-recentf)
(require '19-init-smartparens)
(require '19-init-rainbow-delimiters)
;;; load all init files
;; (require! "lisp/[[:digit:]][[:digit:]].+\\.el")
;;; load config files
(oo-call-after-load 'evil #'require '20-config-evil)
(oo-call-after-load 'helm #'require '20-config-helm)
(oo-call-after-load 'abbrev #'require '20-config-abbrev)
(oo-call-after-load 'grugru #'require '20-config-grugru)
(oo-call-after-load 'smartparens #'require '20-config-smartparens)
;; (defhook! emacs-startup-hook&load-config-files ()
;;   "Load the code for the lisp files."
;;   (flet! feature (-compose #'file-name-sans-extension #'file-name-nondirectory))
;;   (for! (file (directory-files oo-config-dir t))
;;     (oo-call-after-load (feature file) file)))
;;; provide init
(provide 'init)
;;; init.el ends here
