;;; init-modus-operandi.el --- initialize modus-operandi -*- lexical-binding: t; -*-
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
;; Initialize modus-operandi.
;;
;;; Code:
(require 'base)

(defhook! oo-load-modus-operandi-theme-h (after-init-hook)
  "Load `modus-operandi' theme."
  (load-theme 'modus-operandi :no-confirm nil))
;;;; Set custom faces
(oo-custom-set-faces
 'modus-operandi
 ;; Evil state faces
 '(spaceline-evil-normal ((t (:background "#005bbb" :foreground "#ffffff"))))
 '(spaceline-evil-insert ((t (:background "#007400" :foreground "#ffffff"))))
 '(spaceline-evil-visual ((t (:background "#aa2200" :foreground "#ffffff"))))
 '(spaceline-evil-replace ((t (:background "#aa2200" :foreground "#ffffff"))))
 '(spaceline-evil-motion ((t (:background "#783c00" :foreground "#ffffff"))))
 '(spaceline-evil-operator ((t (:background "#783c00" :foreground "#ffffff"))))
 '(spaceline-evil-emacs ((t (:background "#4f0090" :foreground "#ffffff")))))
;;; provide
(provide 'init-modus-operandi)
;;; init-modus-operandi.el ends here
