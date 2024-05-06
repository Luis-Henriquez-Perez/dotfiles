;;; 99-after-load-vertico.el --- TODO: add commentary -*- lexical-binding: t; -*-
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

(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)
(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

;; Try to split up loading.
(defhook! emacs-startup-hook&require-vertico ()
  (require 'vertico nil t))

(opt! vertico-quick1 "asdf")
(opt! vertico-quick2 "jkl;")

(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)

(opt! orderless-matching-styles '(orderless-initialism
                                  orderless-regexp))

(setq vertico-buffer-display-action
      '(display-buffer-in-direction
        (direction . below)
        (window-height . ,(+ 3 vertico-count))))

(defhook! vertico-mode-hook&enable-orderless ()
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))))

(oo-popup-at-bottom "\\*Vertico")

(oo-bind 'vertico-map :ieg "TAB" #'vertico-next)
(oo-bind 'vertico-map :ieg "C-k" #'vertico-previous)
(oo-bind 'vertico-map :ieg "C-j" #'vertico-next)
(oo-bind 'vertico-map :ieg ";" #'vertico-quick-exit)
(oo-bind 'vertico-map :ieg "C-;" #'vertico-quick-exit)
(oo-bind 'vertico-map :ieg [backtab] #'vertico-previous)

(oo-bind 'vertico-map :i "C-o" #'embark-act)

;; When I am completing a word at point I want the matching style to be exact.
;; at the very least.
;; Orderless should be just at.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;; (defun +orderless-exact (component)
;;   `(: bos (literal ,component)))
;;; provide
(provide '99-after-load-vertico)
;;; 99-after-load-vertico.el ends here
