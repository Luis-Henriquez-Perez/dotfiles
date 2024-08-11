;;; oo-init.el -*- lexical-binding: t; -*-
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
;; This file loads everything that needs to be evaluated immediately on startup.
;;
;;; Code:
;;;; requirements
(require 'base)
(require 'oo-init-hooks)
(require 'oo-init-keybindings)
;;;; set initial font
(alet (or (font-spec :name "Martian Mono Nerd Font"
                     :weight 'normal
                     :slant 'normal
                     :size 14)
          (font-spec :name "Nimbus Mono PS"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "Iosevka Comfy Wide"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "SpaceMono Nerd Font"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "iMWritingMono Nerd Font Mono"
                     :weight 'normal
                     :slant 'normal
                     :size 15))
  (set-face-attribute 'default nil :font it))
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
