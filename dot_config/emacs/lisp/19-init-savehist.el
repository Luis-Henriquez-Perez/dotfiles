;;; 18-savehist-save.el --- Custom configuration -*- lexical-binding: t; -*-
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
;; These are my personal Emacs configurations.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
(oo-add-hook 'on-first-input-hook #'savehist-mode)

(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))
(opt! savehist-file (concat oo-cache-dir "savehist"))

(cl-pushnew 'register-alist savehist-additional-variables)
;; savehist-save@AR@remove-properties-from-kill-ring
;; savehist-save@AW@remove-properties-from-kill-ring
;; savehist-save@BW@remove-properties-from-kill-ring
;; savehist-save@B@remove-properties-from-kill-ring
;; savehist-save@A@remove-properties-from-kill-ring
(defadvice! savehist-save@B@remove-kill-ring-properties ()
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))
;;; provide
(provide '19-init-savehist)
;;; 19-init-savehist.el ends here
