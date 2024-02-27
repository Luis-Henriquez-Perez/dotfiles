;;; 19-init-evil.el -*- lexical-binding: t; -*-
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
;; Initial configuration for evil.
;;
;;; Code:
(oo-add-hook 'emacs-startup-hook #'evil-mode)

;; Don't load everything at once.
(defhook! after-init-hook&load-evil ()
  [:depth 10]
  (require 'evil nil t))
;; (oo-add-hook 'after-init-hook #'require :args '(evil) :depth 10)

;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)
;;;; evil-surround
(oo-add-hook 'prog-mode-hook #'evil-surround-mode)

(oo-add-hook 'text-mode-hook #'evil-surround-mode)
;;;; evil-cleverparens 
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)

(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;;; evil-operator 
(oo-bind :n "gr" #'evil-operator-eval)
;;;; evil-easymotion 
(autoload #'oo-goto-beginning-of-word "evil-easymotion")
(autoload #'oo-goto-end-of-word "evil-easymotion")
(autoload #'oo-goto-char "evil-easymotion")

(oo-bind :nv "w" #'oo-goto-beginning-of-word)
(oo-bind :nv "e" #'oo-goto-end-of-word)
(oo-bind :nv "f" #'oo-goto-char)
;;;; expand-region 
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
;;;; lispy 
(oo-bind :v "E" #'lispy-eval-and-replace)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)

(provide '19-init-evil)
;; 19-init-evil.el ends here
