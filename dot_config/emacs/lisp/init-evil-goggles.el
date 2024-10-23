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
(opt! evil-goggles-duration 0.1)

(defun! oo--load-evil-goggles (fn &rest args)
  (unless (or (minibufferp)
			  (bound-and-true-p evil-goggles-mode))
	(require 'evil-goggles)
	(evil-goggles-mode 1))
  (apply fn args))

(after! register-evil-goggles-commands (evil-goggles lispyville)
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

(advice-add 'evil-delete								:around #'oo--load-evil-goggles)
(advice-add 'evil-delete-line                           :around #'oo--load-evil-goggles)
(advice-add 'evil-org-delete							:around #'oo--load-evil-goggles)
(advice-add 'evil-yank                                  :around #'oo--load-evil-goggles)
(advice-add 'evil-yank-line                             :around #'oo--load-evil-goggles)
(advice-add 'evil-change								:around #'oo--load-evil-goggles)
(advice-add 'evil-change-line                           :around #'oo--load-evil-goggles)
(advice-add 'evil-change-whole-line                     :around #'oo--load-evil-goggles)
(advice-add 'evil-indent								:around #'oo--load-evil-goggles)
(advice-add 'evil-join                                  :around #'oo--load-evil-goggles)
(advice-add 'evil-join-whitespace                       :around #'oo--load-evil-goggles)
(advice-add 'evil-fill-and-move                         :around #'oo--load-evil-goggles)
(advice-add 'evil-shift-left							:around #'oo--load-evil-goggles)
(advice-add 'evil-shift-right                           :around #'oo--load-evil-goggles)
(advice-add 'evil-org                                   :around #'oo--load-evil-goggles)
(advice-add 'evil-org                                   :around #'oo--load-evil-goggles)
(advice-add 'evil-surround-region                       :around #'oo--load-evil-goggles)
(advice-add 'evil-commentary							:around #'oo--load-evil-goggles)
(advice-add 'evilnc-comment-operator					:around #'oo--load-evil-goggles)
(advice-add 'evil-replace-with-register                 :around #'oo--load-evil-goggles)
(advice-add 'evil-set-marker							:around #'oo--load-evil-goggles)
(advice-add 'evil-record-macro                          :around #'oo--load-evil-goggles)
(advice-add 'evil-paste-before                          :around #'oo--load-evil-goggles)
(advice-add 'evil-paste-after                           :around #'oo--load-evil-goggles)
(advice-add 'lispyville-yank							:around #'oo--load-evil-goggles)
(advice-add 'lispyville-delete                          :around #'oo--load-evil-goggles)
(advice-add 'lispyville-change                          :around #'oo--load-evil-goggles)
(advice-add 'lispyville-yank-line                       :around #'oo--load-evil-goggles)
(advice-add 'lispyville-delete-line                     :around #'oo--load-evil-goggles)
(advice-add 'lispyville-change-line                     :around #'oo--load-evil-goggles)
(advice-add 'lispyville-delete-char-or-splice           :around #'oo--load-evil-goggles)
(advice-add 'lispyville-delete-char-or-splice-backwards :around #'oo--load-evil-goggles)
(advice-add 'lispyville-substitute                      :around #'oo--load-evil-goggles)
(advice-add 'lispyville-change-whole-line               :around #'oo--load-evil-goggles)
(advice-add 'lispyville-join							:around #'oo--load-evil-goggles)
(advice-add '+evil-eval-operator						:around #'oo--load-evil-goggles)
(advice-add '+evil-eval-replace-operator				:around #'oo--load-evil-goggles)
(advice-add '+evil-eval-print-operator                  :around #'oo--load-evil-goggles)
;;; provide
(provide 'init-evil-goggles)
;;; init-evil-goggles.el ends here
