;;; 99-after-load-evil.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; settings
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)
;;;; minibuffer
(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! minibuffer-setup-hook&preserve-prior-evil-state ()
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! minibuffer-exit-hook&restore-prior-evil-state ()
  "Restore state after minibuffer."
  ;; :on evil-mode
  (when (bound-and-true-p evil-mode)
    (evil-change-state oo-evil-state-before-minibuffer)
    (setq oo-evil-state-before-minibuffer nil)))
;;;; cursors
(setq evil-insert-state-cursor '((bar . 3) "chartreuse3"))
(setq evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
(setq evil-normal-state-cursor '(box "DarkGoldenrod2"))
(setq evil-visual-state-cursor '((hollow) "dark gray"))
(setq evil-operator-state-cursor '((hbar . 10) "hot pink"))
(setq evil-replace-state-cursor '(box "chocolate"))
(setq evil-motion-state-cursor '(box "plum3"))

;; (oo-add-hook 'ah-after-enable-theme-hook #'evil-refresh-cursor)
(advice-add #'load-theme :around (lambda (fn &rest args) (apply fn args) (evil-refresh-cursor)))
;;;; escape
(defvar oo-escape-hook nil
  "Hook run after escaping.")

(defun @exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((run-hook-with-args-until-success 'oo-escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        (t (keyboard-quit))))

(oo-bind :ie [escape] #'evil-force-normal-state)

(oo-add-advice #'evil-force-normal-state :after #'@exit-everything)

(oo-bind :n "J" #'evil-scroll-page-down)
(oo-bind :n "K" #'evil-scroll-page-up)
;;;; load evil-org-headline-state
(oo-call-after-load 'org #'require '70-evil-org-headline-state)
;;;;; evil-surround
(oo-add-hook 'prog-mode-hook #'evil-surround-mode)

(oo-add-hook 'text-mode-hook #'evil-surround-mode)
;;; provide
(provide '99-after-load-evil)
;;; 99-after-load-evil.el ends here
