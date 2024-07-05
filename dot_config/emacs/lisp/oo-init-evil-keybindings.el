;;; oo-init-evil-keybindings.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Here are my evil bindings.  I made this file after a tough lesson grappling
;; with a keybinding macro.
;;
;;; Code:
(require 'evil)
(require 'oo-override-mode)
(require 'oo-init-keybindings)
;;;; vertico
(with-eval-after-load 'vertico
  (evil-define-key* '(insert emacs) vertico-map "TAB" #'vertico-next)
  (evil-define-key* '(insert emacs) vertico-map "C-k" #'vertico-previous)
  (evil-define-key* '(insert emacs) vertico-map "C-j" #'vertico-next)
  (evil-define-key* '(insert emacs) vertico-map ";" #'vertico-quick-exit)
  (evil-define-key* '(insert emacs) vertico-map "C-;" #'vertico-quick-exit)
  (evil-define-key* '(insert emacs) vertico-map [backtab] #'vertico-previous)
  (evil-define-key* '(insert) vertico-map "C-o" #'embark-act))
;;;; lispy
(with-eval-after-load 'lispyville
  (evil-define-key* 'insert lispyville-mode-map "SPC" #'lispy-space)
  (evil-define-key* 'insert lispyville-mode-map ";" #'lispy-comment))
;;;; dired
;; Dired is very picky about when these bindings happen.  It is the only package
;; I have had that is that picky.  I have noticed that unlike every other
;; package I have tried dired bindings do not work by trying to set them when
;; `dired-mode-map' is bound.  You need to use (eval-after-load 'dired ...).
;; Also, even if you have the `eval-after-load' it work work from the
;; `oo-after-load-dired' file--do not ask me why.  Again, only package I have
;; had this happen with.
(with-eval-after-load 'dired
  (evil-define-key* '(normal motion) dired-mode-map "h" #'dired-up-directory)
  (evil-define-key* '(normal motion) dired-mode-map "l" #'dired-find-file))
;;;; overriding map
(evil-define-key* '(insert) oo-override-mode-map (kbd oo-insert-leader-key) #'oo-leader-prefix-command)
(evil-define-key* '(normal motion visual) oo-override-mode-map (kbd oo-normal-leader-key) #'oo-leader-prefix-command)
(evil-define-key* '(normal motion visual) oo-override-mode-map (kbd ";") #'execute-extended-command)
(evil-define-key* '(insert emacs) global-map [escape] #'evil-force-normal-state)
;;;; miscellaneous
(evil-define-key* 'insert global-map (kbd "TAB") #'completion-at-point)
(evil-define-key* '(normal) global-map "x" #'hungry-delete-forward)
(evil-define-key* 'insert global-map (kbd "A-x") #'execute-extended-command)
(evil-define-key* 'insert global-map (kbd "M-x") #'execute-extended-command)
(evil-define-key* '(normal motion) global-map "+" #'text-scale-increase)
(evil-define-key* '(normal motion) global-map "-" #'text-scale-decrease)
(evil-define-key* 'normal global-map "H" #'evil-first-non-blank)
(evil-define-key* 'normal global-map "L" #'evil-last-non-blank)
(evil-define-key* 'visual global-map "V" #'expreg-contract)
(evil-define-key* 'visual global-map "v" #'expreg-expand)
(evil-define-key* 'normal global-map "J" #'evil-scroll-page-down)
(evil-define-key* 'normal global-map "K" #'evil-scroll-page-up)
;;;; motions
(evil-define-key* '(normal visual) global-map "w" #'oo-evilem-motion-beginning-of-word)
(evil-define-key* '(normal visual) global-map "W" #'oo-evilem-motion-beginning-of-WORD)
(evil-define-key* '(normal visual) global-map "e" #'oo-evilem-motion-end-of-word)
(evil-define-key* '(normal visual) global-map "E" #'oo-evilem-motion-end-of-WORD)
(evil-define-key* '(normal visual operator) global-map "f" #'oo-evilem-motion-char)
(evil-define-key* '(normal visual operator) global-map "H" #'oo-evilem-motion-beginning-of-line)
;;;; text objects
;;;;;;; line
(define-key evil-inner-text-objects-map "l" #'evil-inner-line)
(define-key evil-outer-text-objects-map "l" #'evil-a-line)
;;;;;;; block
;; Not sure what the difference is between block and form.
(define-key evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
(define-key evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(define-key evil-outer-text-objects-map "f" #'evil-cp-a-form)
(define-key evil-outer-text-objects-map "c" #'lispyville-outer-comment)
(define-key evil-inner-text-objects-map "c" #'lispyville-inner-comment)
;;;; operators
;;;;; eval
(evil-define-key* '(normal visual) global-map (kbd "g t") #'evil-goto-first-line)
(evil-define-key* '(normal visual) global-map (kbd "g b") #'evil-goto-line)
(evil-define-key* '(normal visual) global-map (kbd "g g") #'evil-operator-eval)
(evil-define-key* '(normal visual) global-map (kbd "g h") #'evil-operator-eval)
(evil-define-key* '(normal visual) global-map (kbd "g r") #'evil-operator-eval-replace)
(evil-define-key* '(normal visual) global-map (kbd "g l") #'oo-evil-operator-eval-print)
(evil-define-key* '(normal visual) global-map (kbd "g p") #'oo-evil-operator-eval-print)
;;;;; comment
(evil-define-key* '(normal visual) global-map (kbd "g c") #'lispyville-comment-or-uncomment)
(evil-define-key* '(normal visual) global-map (kbd "g l") #'lispyville-comment-and-clone-dwim)
;;;;; exchange
(evil-define-key* '(normal visual) global-map (kbd "g x") #'evil-exchange)
(evil-define-key* '(normal visual) global-map (kbd "g X") #'evil-exchange-cancel)
(evil-define-key* '(normal visual) global-map (kbd "g a") #'evil-exchange)
(evil-define-key* '(normal visual) global-map (kbd "g A") #'evil-exchange-cancel)
;;;;; g is kind of like the main prefix key of vim
(evil-define-key* '(normal visual) global-map (kbd "g u") #'evil-upcase)
(evil-define-key* '(normal visual) global-map (kbd "g U") #'evil-downcase)
;;;;; make `evil-for'
;; Pressing lowercase "o" is one less keystroke than "W" and it aligns with cio.
;; Though I will say I am not 100% sure it is the equivalent.
(define-key evil-motion-state-map "o" #'evil-forward-WORD-begin)
;;; provide
(provide 'oo-init-evil-keybindings)
;;; oo-init-evil-keybindings.el ends here
