;;; init-auto-insert.el --- Initialize auto-insert -*- lexical-binding: t; -*-
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
;; Initialize auto-insert.
;;
;;; Code:
(opt! auto-insert-query nil)
;; (hook! emacs-startup-hook auto-insert-mode)
(hook! on-first-file-hook auto-insert-mode)
(define-auto-insert "\\.el$" #'oo-auto-insert-elisp-template)
(define-auto-insert "\\.html$" #'oo-auto-insert-html-template)
(require 'config-auto-insert)
;; (autoload 'oo-auto-insert-elisp-template "oo-auto-insert-templates")
;;; provide
(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
