;;; 19-savehist-save.el --- Custom configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 14 Feb 2024
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
;;
;;; Code:
(require 'on)

(oo-add-hook 'on-first-input-hook #'savehist-mode)

(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))
;; This should be already taken care of by no-littering.
(opt! savehist-file (expand-file-name "savehist" oo-data-dir))

(opt! savehist-additional-variables (cl-adjoin 'register-alist savehist-additional-variables))

(defadvice! savehist-save@BFremove-kill-ring-properties (&rest _)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))
;;; provide
(provide '19-init-savehist)
;;; 19-init-savehist.el ends here
