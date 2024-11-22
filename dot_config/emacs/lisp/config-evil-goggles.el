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
;;;; register lispyville commands
(block! (set! list '((+evil-eval-operator evil-change)
                     (+evil-eval-replace-operator evil-change)
                     (+evil-eval-print-operator evil-change)
                     (lispyville-delete-line evil-delete-line)
			         (lispyville-yank-line evil-yank-line)
			         (lispyville-change-line evil-change-line)
			         (lispyville-delete-char-or-splice evil-delete-char)
			         (lispyville-delete-char-or-splice-backwards evil-delete-backward-char)
			         (lispyville-substitute evil-substitute)
			         (lispyville-change-whole-line evil-change-whole-line)
			         (lispyville-join evil-join)
			         (lispyville-change evil-change)
			         (lispyville-delete evil-delete)
			         (lispyville-yank evil-yank)))
        (for! ((new old) list)
          (set! elt (cons new (cdr (assoc old evil-goggles--commands))))
          (cl-pushnew elt evil-goggles--commands :key #'car))
        (cl-assert (--all-p (assoc it list) (mapcar #'car list))))
;;; provide
(provide 'config-evil-goggles)
;;; config-evil-goggles.el ends here
