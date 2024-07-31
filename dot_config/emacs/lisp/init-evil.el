;;; init-evil.el --- initialize evil -*- lexical-binding: t; -*-
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
;; Initialize evil.
;;
;;; Code:
(require 'base)

(defhook! after-init-hook&load-evil ()
  [:depth 10]
  (require 'evil nil t))
(hook! emacs-startup-hook&evil-mode)
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)
(opt! evil-move-cursor-back nil)
(opt! evil-move-beyond-eol nil)
(opt! evil-search-wrap nil)
(opt! evil-insert-state-cursor '((bar . 3) "chartreuse3"))
(opt! evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
(opt! evil-normal-state-cursor '(box "DarkGoldenrod2"))
(opt! evil-visual-state-cursor '((hollow) "dark gray"))
(opt! evil-operator-state-cursor '((hbar . 10) "hot pink"))
(opt! evil-replace-state-cursor '(box "chocolate"))
(opt! evil-motion-state-cursor '(box "plum3"))

(oo-call-after-load 'evil #'require 'oo-after-load-evil)

(bind! n "H" #'evil-first-non-blank)
(bind! n "L" #'evil-last-non-blank)
(bind! n "J" #'evil-scroll-page-down)
(bind! n "K" #'evil-scroll-page-up)

(bind! (n v) "g u" #'evil-upcase)
(bind! (n v) "g U" #'evil-downcase)

;; Pressing lowercase "o" is one less keystroke than "W" and it aligns with cio.
;; Though I will say I am not 100% sure it is the equivalent.
(bind! evil-motion-state-map "o" #'evil-forward-WORD-begin)

(bind! (n v) "g t" #'evil-goto-first-line)
(bind! (n v) "g b" #'evil-goto-line)

(bind! (n v) "g g" #'oo-eval-operator)
(bind! (n v) "g h" #'oo-eval-operator)
(bind! (n v) "g r" #'oo-eval-replace-operator)
(bind! (n v) "g l" #'oo-eval-print-operator)
(bind! (n v) "g p" #'oo-eval-print-operator)
;;; provide
(provide 'init-evil)
;;; init-evil.el ends here
