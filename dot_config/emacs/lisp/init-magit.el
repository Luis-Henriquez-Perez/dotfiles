;;; init-magit.el --- initialize magit -*- lexical-binding: t; -*-
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
;; Initialize magit.
;;
;;; Code:
;;;; requirements
(require 'base)
;;;; popup
(oo-popup-at-bottom "\\`magit")
;;;; enter insert state on commit
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(defhook! enter-insert-state (git-commit-mode-hook)
  "Enter `evil-insert-state' after `git-commit-mode'"
  (when (bound-and-true-p evil-mode)
	(evil-insert-state 1)))
;;;; bindings
(oo-call-after-load '(magit evil) #'evil-magit-init)

(bind! oo-git-map "s" #'magit-status)
(bind! oo-git-map "p" #'magit-push)
(bind! oo-git-map "c" #'magit-commit)
(bind! oo-git-map "B" #'magit-branch)
;;; provide
(provide 'init-magit)
;;; init-magit.el ends here
