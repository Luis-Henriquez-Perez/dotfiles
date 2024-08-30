;;; init-org.el --- initialize org -*- lexical-binding: t; -*-
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
;; Initialize org.
;;
;;; Code:
(require 'base)

;; Add extra todo.
(opt! org-directory (f-full "~/Documents/org/"))
(opt! org-agenda-files (directory-files org-directory t "\\.org\\'"))
(opt! org-todo-keywords '((sequence "TODO" "DONE")
                          (sequence "BUG" "FIXED")
                          (sequence "BINDING" "BOUND")
                          (sequence "OPEN" "CLOSED")
                          (sequence "ADVICE" "DEFINED")
                          (sequence "MACRO" "DEFINED")
                          (sequence "COMMAND" "DEFINED")
                          (sequence "FUNCTION" "DEFINED")
                          (sequence "QUESTION" "ANSWERED")))
(opt! org-src-fontify-natively t)
(opt! org-hide-emphasis-markers t)
;;; provide
(provide 'init-org)
;;; init-org.el ends here
