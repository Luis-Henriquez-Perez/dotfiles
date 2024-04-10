;;; 99-after-load-recentf.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
;; TODO: Figure out why this is an error with eldev eval.
;; For some reason this gives an error when I use eldev eval. I have to figure
;; out what eldev is doing here.
(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)

(oo-add-advice #'recentf-cleanup :around #'oo-funcall-silently)
(oo-add-advice #'recentf-save-list :around #'oo-funcall-silently)
(oo-add-advice #'recentf-mode :around #'oo-funcall-silently)

(opt! recentf-filename-handlers '(file-truename))

;; TODO: Add back =adjoin!= if I removed it.
(opt! recentf-filename-handlers (cl-adjoin #'abbreviate-file-name recentf-filename-handlers))

(opt! recentf-filename-handlers (cl-adjoin 'substring-no-properties recentf-filename-handlers))

(opt! recentf-exclude (cl-adjoin (regexp-quote (recentf-expand-file-name oo-config-dir)) recentf-exclude))

(opt! recentf-exclude (cl-adjoin (regexp-quote (recentf-expand-file-name oo-data-dir)) recentf-exclude))
;;; provide
(provide '99-after-load-recentf)
;;; 99-after-load-recentf.el ends here
