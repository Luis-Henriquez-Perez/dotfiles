;;; init-evil-goggles.el --- Initialize evil-goggles -*- lexical-binding: t; -*-
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
;; Initialize evil-goggles.
;;
;;; Code:
;; 1. advise all commands used by evil-goggles
(oo-add-advice 'evil-delete :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-delete-line :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-org-delete :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-yank :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-yank-line :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-change :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-change-line :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-change-whole-line :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-indent :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-join :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-join-whitespace :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-fill-and-move :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-shift-left :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-shift-right :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-org :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-org :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-surround-region :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-commentary :around #'oo--load-evil-goggles)
(oo-add-advice 'evilnc-comment-operator :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-replace-with-register :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-set-marker :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-record-macro :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-paste-before :around #'oo--load-evil-goggles)
(oo-add-advice 'evil-paste-after :around #'oo--load-evil-goggles)

;; This is kind of tricky to lazy-load.  What I have to do is enable
;; `evil-goggle' the first time one of these functions is called.  So I should
;; create an advice and add it to all of these functions.
(defun! oo--load-evil-goggles (fn &rest args)
  (nif! (featurep 'evil-goggles)
	  (advice-remove fn #'oo--load-evil-goggles)
	(require 'evil-goggles)
	(evil-goggles-mode 1)
	(apply fn args)))

(defun! oo--load-evil-goggles (symbol fn &rest args)
  (nif! (featurep 'evil-goggles)
	  (advice-remove fn #'oo--load-evil-goggles)
	(require 'evil-goggles)
	(evil-goggles-mode 1)
	(apply fn args)))


;; Then register any command that needs registering.
(evil-yank                 . lispyville-yank)
(evil-delete               . lispyville-delete)
(evil-change               . lispyville-change)
(evil-yank-line            . lispyville-yank-line)
(evil-delete-line          . lispyville-delete-line)
(evil-change-line          . lispyville-change-line)
(evil-delete-char          . lispyville-delete-char-or-splice)
(evil-delete-backward-char . lispyville-delete-char-or-splice-backwards)
(evil-substitute           . lispyville-substitute)
(evil-change-whole-line    . lispyville-change-whole-line)
(evil-join                 . lispyville-join)
(for! ((old . new) commands)
  (awhen (assoc old evil-goggles--commands)
    (pushing! evil-goggles--commands (cons new (cdr it)))))
;;; provide
(provide 'init-evil-goggles)
;;; init-evil-goggles.el ends here
