;;; oo-after-load-helm.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(set! helm-candidate-number-limit 50)

(oo-popup-at-bottom "\\*Helm")

(bind! helm-map :ie "TAB" #'helm-next-line)
(bind! helm-map :ie [backtab] #'helm-previous-line)
(bind! helm-map :ie "C-j" #'helm-next-line)
(bind! helm-map :ie "C-k" #'helm-previous-line)

(bind! helm-map :ie "C-a" #'helm-select-action)
(bind! helm-map :ie "C-m" #'helm-toggle-visible-mark-forward)
(bind! helm-map :ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
(bind! helm-map :ie "S-TAB" #'helm-mark-current-line)

(bind! :ie 'helm-map "C-;" #'ace-jump-helm-line)

;;; provide
(provide 'oo-after-load-helm)
;;; oo-after-load-helm.el ends here
