;;; oo-after-load-vertico.el --- vertico configuration -*- lexical-binding: t; -*-
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
;; Configuration for vertico.
;;
;;; Code:
(require 'vertico)
;;;; vertico
;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

;; When I am completing a word at point I want the matching style to be exact.
;; at the very least.
;; Orderless should be just at.
;; (defun +orderless-exact (component)
;;   `(: bos (literal ,component)))
;;; provide
(provide 'oo-after-load-vertico)
;;; oo-after-load-vertico.el ends here
