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
;; own.  Also I want the ability to profile keybindings also and that is *much*
;; easier to do when all my bindings are in one file.  However, the main reason
;; is the former--it is just easier.  Then again though, I do not know.  Maybe
;; instead of devoting an individual file for this I should just use something
;; like =annalist= to record and display the information to me.  But then I
;; cannot profile bindings as easily.
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
;;;; smartparens
(oo-bind 'oo-toggle-map "s" #'smartparens-mode)
;;;; burly
;;;;; leader bindings
(oo-bind 'oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-find-map "j" #'burly-open-bookmark)
;;;;; save window configuration with =b= or =S=
;; The command [[file:snapshots/_helpful_command__burly-bookmark-windows_.png][burly-bookmark-windows]] creates a bookmark with the information
;; necessary to reproduce the current window configuration.  I can then restore the
;; window information I've bookmarked with [[file:snapshots/_helpful_command__burly-open-bookmark_.png][burly]].
(oo-bind 'oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
;;;; expand-region
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
;;;; eshell
(oo-bind 'oo-app-map "e" #'eshell)
;;;; lispyville
(oo-bind :n "g," #'lispyville-comment-or-uncomment)
(oo-bind :n "gc" #'lispyville-comment-and-clone-dwim)
(oo-bind :n "gl" #'lispyville-comment-and-clone-dwim)

;; (oo-bind 'lispyville-mode-map :i "SPC" #'lispy-space)
;; (oo-bind 'lispyville-mode-map :i ";" #'lispy-comment)

;;;; macrostep
(oo-bind 'emacs-lisp-mode-map "me" #'macrostep-expand :localleader t :wk "expand")
(oo-bind 'emacs-lisp-mode-map "mc" #'macrostep-collapse :localleader t :wk "collapse")
(oo-bind 'emacs-lisp-mode-map "mC" #'macrostep-collapse-all :localleader t :wk "collapse all")
;;;; evil-easymotion
;; The problem with these keys is that they interfere with keyboard macros.  Let
;; me explain--when you use avy, it is not necessarily the case that the
;; following keys have the same letter.  For keyboard macros to work you need
;; keys to exhibit predictable behaviors.  I do not want to get rid of these
;; keys entirely, but I have to consider more carfully on how I will re-add them
;; to my configuration.
;; (autoload #'oo-goto-beginning-of-word "evil-easymotion")
;; (autoload #'oo-goto-end-of-word "evil-easymotion")
;; (autoload #'oo-goto-char "evil-easymotion")

;; (oo-bind :nv "w" #'oo-goto-beginning-of-word)
;; (oo-bind :nv "e" #'oo-goto-end-of-word)
;; (oo-bind :nv "f" #'oo-goto-char)
;;;; lispy
(oo-bind :v "E" #'lispy-eval-and-replace)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)
;;;; evil-cleverparens
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)

(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;; provide
(provide '90-keybindings)
;;; 90-keybindings.el ends here
