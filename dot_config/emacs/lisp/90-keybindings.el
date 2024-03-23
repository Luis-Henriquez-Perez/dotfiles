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
(require '06-base-leaders)
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

(oo-bind 'lispyville-mode-map :i "SPC" #'lispy-space)
(oo-bind 'lispyville-mode-map :i ";" #'lispy-comment)
;;;; evil-easymotion
;; The problem with these keys is that they interfere with keyboard macros.  Let
;; me explain--when you use avy, it is not necessarily the case that the
;; following keys have the same letter.  For keyboard macros to work you need
;; keys to exhibit predictable behaviors.  I do not want to get rid of these
;; keys entirely, but I have to consider more carfully on how I will re-add them
;; to my configuration.
;; TODO: Maybe make more direct or add support for autoloading in the lisp
;; directory.  Right now this loads easymotion which causes these functons to be
;; define by me with `eval-after-load'.
(autoload #'oo-evilem-bow "evil-easymotion")
(autoload #'oo-evilem-eow "evil-easymotion")
(autoload #'oo-evilem-goto-char "evil-easymotion")
(autoload #'oo-evilem-goto-bol "evil-easymotion")

;; TODO: Do not use these bindings for keyboard macros where the originals might
;; be more useful.
;; Note: these are *not* the same as
(oo-bind :nv "w" #'evilem-motion-beginning-of-word)
(oo-bind :nv "W" #'evilem-motion-beginning-of-WORD)
(oo-bind :nv "e" #'evilem-motion-end-of-word)
(oo-bind :nv "E" #'evilem-motion-end-of-WORD)
(oo-bind :nv "f" #'oo-evilem-char)
(oo-bind :nv "b" #'oo-evilem-bol)
;;;; lispy
(oo-bind :v "E" #'lispy-eval-and-replace)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)
;;;; evil-cleverparens
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)

(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;;; create a leader map for dotfile actions
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(oo-bind 'oo-leader-map "d" #'oo-dotfile-prefix-command :wk "dotfiles")

;; TODO: oo-bind should already do this for me.
(autoload #'chezmoi-find "chezmoi")
(autoload #'chezmoi-write "chezmoi")
(autoload #'chezmoi-open-other "chezmoi")
(oo-bind 'oo-dotfile-map "f" #'chezmoi-find)
;; I use the command =chezmoi-write= the most so far.  It syncs the current file
;; with its corresponding chezmoi file.  If called while in the target file, it
;; applies the changes in the target file to the source file and vice versa.
;; Only caveat is that if there is a more recent change in the "other" file,
;; then you have to use a prefix command to make sure you want to override those
;; changes.
(oo-bind 'oo-dotfile-map "w" #'chezmoi-write)
;; Binding to the "w" key is the more BLANK choice but "d" is closer to the
;; homerow for QWERTY keyboards.
(oo-bind 'oo-dotfile-map "d" #'chezmoi-write)
;; The command =chezmoi-open-other= is also useful.  Similar to =chezmoi-find=
;; it does something different depending on whether your in the source file or
;; the target file.  If you are in the source file, you open the target file and
;; vice versa.
(oo-bind 'oo-dotfile-map "o" #'chezmoi-open-other)
;;;; consult
(oo-bind 'oo-find-map "k" #'consult-bookmark :wk "bookmark")
(oo-bind 'oo-find-map "b" #'consult-bookmark :wk "bookmark")

(oo-bind 'oo-find-map "s" #'consult-line :wk "line")
(oo-bind 'oo-find-map "l" #'consult-line :wk "line")

(oo-bind 'oo-find-map "h" #'consult-outline :wk "outline")
(oo-bind 'oo-find-map "h" #'consult-org-heading :wk "heading")

(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)

(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)

(oo-bind 'oo-miscellany-map "l" #'consult-bookmark)

(opt! consult-preview-key nil)

(opt! consult-fontify-preserve nil)

(defun! oo-display-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'display-buffer)
  (call-interactively #'consult-buffer))

(oo-bind :alt #'display-buffer #'oo-display-buffer)
;;;; macrostep
(oo-bind 'emacs-lisp-mode-map "me" #'macrostep-expand :localleader t :wk "expand")
(oo-bind 'emacs-lisp-mode-map "mc" #'macrostep-collapse :localleader t :wk "collapse")
(oo-bind 'emacs-lisp-mode-map "mC" #'macrostep-collapse-all :localleader t :wk "collapse all")
;;;; helpful
(oo-bind :alt #'describe-function #'helpful-callable :feature 'helpful)
(oo-bind :alt #'describe-command  #'helpful-command  :feature 'helpful)
(oo-bind :alt #'describe-variable #'helpful-variable :feature 'helpful)
(oo-bind :alt #'describe-key      #'helpful-key      :feature 'helpful)
;;; provide
(provide '90-keybindings)
;;; 90-keybindings.el ends here
