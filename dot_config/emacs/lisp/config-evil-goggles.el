;;; config-evil-goggles.el --- Configure evil-goggles -*- lexical-binding: t; -*-
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
;; Configure evil-goggles.
;;
;;; Code:
;;;; register evil commands
(defun! oo-require-evil-goggles-a (fn &rest args)
  (unless (or (minibufferp)
			  (bound-and-true-p evil-goggles-mode))
	(require 'evil-goggles)
	(evil-goggles-mode 1))
  (apply fn args))

(advice-add 'evil-delete								:around #'oo-require-evil-goggles-a)
(advice-add 'evil-delete-line                           :around #'oo-require-evil-goggles-a)
(advice-add 'evil-org-delete							:around #'oo-require-evil-goggles-a)
(advice-add 'evil-yank                                  :around #'oo-require-evil-goggles-a)
(advice-add 'evil-yank-line                             :around #'oo-require-evil-goggles-a)
(advice-add 'evil-change								:around #'oo-require-evil-goggles-a)
(advice-add 'evil-change-line                           :around #'oo-require-evil-goggles-a)
(advice-add 'evil-change-whole-line                     :around #'oo-require-evil-goggles-a)
(advice-add 'evil-indent								:around #'oo-require-evil-goggles-a)
(advice-add 'evil-join                                  :around #'oo-require-evil-goggles-a)
(advice-add 'evil-join-whitespace                       :around #'oo-require-evil-goggles-a)
(advice-add 'evil-fill-and-move                         :around #'oo-require-evil-goggles-a)
(advice-add 'evil-shift-left							:around #'oo-require-evil-goggles-a)
(advice-add 'evil-shift-right                           :around #'oo-require-evil-goggles-a)
(advice-add 'evil-org                                   :around #'oo-require-evil-goggles-a)
(advice-add 'evil-org                                   :around #'oo-require-evil-goggles-a)
(advice-add 'evil-surround-region                       :around #'oo-require-evil-goggles-a)
(advice-add 'evil-commentary							:around #'oo-require-evil-goggles-a)
(advice-add 'evilnc-comment-operator					:around #'oo-require-evil-goggles-a)
(advice-add 'evil-replace-with-register                 :around #'oo-require-evil-goggles-a)
(advice-add 'evil-set-marker							:around #'oo-require-evil-goggles-a)
(advice-add 'evil-record-macro                          :around #'oo-require-evil-goggles-a)
(advice-add 'evil-paste-before                          :around #'oo-require-evil-goggles-a)
(advice-add 'evil-paste-after                           :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-yank							:around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-delete                          :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-change                          :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-yank-line                       :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-delete-line                     :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-change-line                     :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-delete-char-or-splice           :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-delete-char-or-splice-backwards :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-substitute                      :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-change-whole-line               :around #'oo-require-evil-goggles-a)
(advice-add 'lispyville-join							:around #'oo-require-evil-goggles-a)
(advice-add '+evil-eval-operator						:around #'oo-require-evil-goggles-a)
(advice-add '+evil-eval-replace-operator				:around #'oo-require-evil-goggles-a)
(advice-add '+evil-eval-print-operator                  :around #'oo-require-evil-goggles-a)
;;;; register lispyville commands
(defafter! oo-evil-goggles-lazy-load (lispyville)
  (set! list '((lispyville-delete-line evil-delete-line)
			   (lispyville-yank-line evil-yank-line)
			   (lispyville-change-line evil-change-line)
			   (lispyville-delete-char-or-splice evil-delete-char)
			   (lispyville-delete-char-or-splice-backwards evil-delete-backward-char)
			   (lispyville-substitute evil-substitute)
			   (lispyville-change-whole-line evil-change-whole-line)
			   (lispyville-join evil-join)
			   (lispyville-change evil-change)
			   (lispyville-delete evil-delete)
			   (lispyville-yank evil-yank)
               (+evil-eval-operator evil-change)
               (+evil-eval-replace-operator evil-change)
               (+evil-eval-print-operator evil-change)))
  (for! ((new old) list)
	(pushing! evil-goggles--commands (cons new (cdr (assoc old evil-goggles--commands))))))
;;; provide
(provide 'config-evil-goggles)
;;; config-evil-goggles.el ends here