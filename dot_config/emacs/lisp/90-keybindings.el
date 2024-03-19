;;; 90-keybindings.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; I realized it is really useful to be able to see keybindings all on their
;; own.  Also I want the ability to profile keybindings also.
;;
;;; Code:
;;;; leaders
;;;; window-map
;;;; bindings
(oo-bind :nm "+" #'text-scale-increase)
(oo-bind :nm "-" #'text-scale-decrease)

(oo-bind 'oo-toggle-map "r" #'read-only-mode)
(oo-bind 'oo-toggle-map "t" #'load-theme)
(oo-bind 'oo-toggle-map "d" #'toggle-debug-on-error)

(oo-bind 'oo-toggle-map "p" (lambda () (interactive) (profiler-start 'cpu+mem)))
(oo-bind 'oo-toggle-map "P" #'profiler-stop)
;;;; evil-textobj-line
;; TODO: create an abbrev to today's date.
;; TODO: there needs to be a standard for setting today.
;; While I was writing a code that would automate adding package headers to
;; files, I wanted to surround each line with quotes and that is when I thought
;; I would like a line text-object.
(autoload #'evil-inner-line "evil-textobj-line")
(autoload #'evil-a-line "evil-textobj-line")
(oo-bind 'evil-inner-text-objects-map "l" #'evil-inner-line)
(oo-bind 'evil-outer-text-objects-map "l" #'evil-a-line)
;;;; evil operators
(autoload 'evil-operator-eval "evil-extra-operator")
(autoload 'evil-operator-eval-replace "evil-extra-operator")
(oo-bind 'oo-leader-map "er" #'evil-operator-eval-replace :wk "eval and replace")
(oo-bind 'oo-leader-map "ee" #'evil-operator-eval :wk "eval")
;;; provide
(provide '90-keybindings)
;;; 90-keybindings.el ends here
