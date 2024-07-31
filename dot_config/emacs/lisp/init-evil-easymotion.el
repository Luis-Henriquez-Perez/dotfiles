;;; init-evil-easymotion.el --- initialize evil-easymotion -*- lexical-binding: t; -*-
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
;; Initialize evil-easymotion.
;;
;;; Code:
(require 'base)

(opt! evilem-style 'at)
(opt! evilem-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

(autoload #'oo-evilem-motion-beginning-of-word "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-WORD "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-end-of-word       "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-end-of-WORD       "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-char              "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-line "oo-evilem-motions" nil t 'function)

(bind! (n v) "w" #'oo-evilem-motion-beginning-of-word)
(bind! (n v) "W" #'oo-evilem-motion-beginning-of-WORD)
(bind! (n v) "e" #'oo-evilem-motion-end-of-word)
(bind! (n v) "E" #'oo-evilem-motion-end-of-WORD)
(bind! (n v o) "f" #'oo-evilem-motion-char)
(bind! (n v o) "H" #'oo-evilem-motion-beginning-of-line)
;;; provide
(provide 'init-evil-easymotion)
;;; init-evil-easymotion.el ends here
