;;; init-abbrev.el --- initialize abbrev-mode -*- lexical-binding: t; -*-
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
;; Initialize abbrev-mode.
;;
;;; Code:
(require 'base)
;;;; hooks
(hook! prog-mode-hook abbrev-mode)
(hook! text-mode-hook abbrev-mode)
;; Emacs loads abbreviation-mode automatically so instead of using something
;; like `with-eval-after-load' I am loading it in `abbrev-mode-hook'.
(defhook! apply-config (abbrev-mode-hook)
  (require 'config-abbrev))
;;;; bindings
(autoload #'+abbrev-add-new-abbrev "config-abbrev" nil t 'function)
(bind! "C-c j" #'+abbrev-add-new-abbrev)
(bind! "C-c k" #'unexpand-abbrev)
;;;; do not save abbrevs to a file
(oo-add-advice #'read-abbrev-file :around #'ignore)
(oo-add-advice #'write-abbrev-file :around #'ignore)
(oo-add-advice #'abbrev--possibly-save :around #'ignore)
(oo-add-advice #'quietly-read-abbrev-file :around #'ignore)
;;; provide
(provide 'init-abbrev)
;;; init-abbrev.el ends here
