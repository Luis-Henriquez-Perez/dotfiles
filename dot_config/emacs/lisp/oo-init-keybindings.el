;;; oo-init-keybindings.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-base)
;; (oo-call-after-load 'evil #'require 'oo-init-evil-keybindings)
;;;; level 1 evil bindings
;;;;; w W e E f b - evil motions
;; The problem with these keys is that they interfere with keyboard macros.  Let
;; me explain--when you use avy, it is not necessarily the case that the
;; following keys have the same letter.  For keyboard macros to work you need
;; keys to exhibit predictable behaviors.  I do not want to get rid of these
;; keys entirely, but I have to consider more carfully on how I will re-add them
;; to my configuration.  TODO: Maybe make more direct or add support for
;; autoloading in the lisp directory.  Right now this loads easymotion which
;; causes these functons to be define by me with `eval-after-load'.
(autoload #'oo-evilem-motion-beginning-of-word "evil-easymotion")
(autoload #'oo-evilem-motion-beginning-of-WORD "evil-easymotion")
(autoload #'oo-evilem-motion-end-of-word "evil-easymotion")
(autoload #'oo-evilem-motion-end-of-WORD "evil-easymotion")
(autoload #'oo-evilem-motion-char "evil-easymotion")
(autoload #'oo-evilem-motion-beginning-of-line "evil-easymotion")

;; TODO: Create an evil operator to narrow to a region.
;; TODO: Do not use these bindings for keyboard macros where the originals might
;; be more useful.
(oo-bind :nv "w" #'oo-evilem-motion-beginning-of-word)
(oo-bind :nv "W" #'oo-evilem-motion-beginning-of-WORD)
(oo-bind :nv "e" #'oo-evilem-motion-end-of-word)
(oo-bind :nv "E" #'oo-evilem-motion-end-of-WORD)
(oo-bind :nvo "f" #'oo-evilem-motion-char)
(oo-bind :nvo "H" #'oo-evilem-motion-beginning-of-line)
;; (oo-bind :nvo "L" #'oo-evilem-motion-end-of-line)
;;;;; +/- increasing text-size
(oo-bind :nm "+" #'text-scale-increase)
(oo-bind :nm "-" #'text-scale-decrease)
;;;;; l as a textobj for line
;; TODO: create an abbrev to today's date.
;; TODO: there needs to be a standard for setting today.
;; While I was writing a code that would automate adding package headers to
;; files, I wanted to surround each line with quotes and that is when I thought
;; I would like a line text-object.
(autoload #'evil-inner-line "evil-textobj-line")
(autoload #'evil-a-line "evil-textobj-line")
(oo-bind 'evil-inner-text-objects-map "l" #'evil-inner-line)
(oo-bind 'evil-outer-text-objects-map "l" #'evil-a-line)
;;;;; form textobject
;; TODO: Figure out how to do this only in lisp modes.  Although now that I
;; think about it I think I have used this in non-lisp modes as well.
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;;;; v and V to expand/contract region
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
;;;;; use TAB to complete a word
(oo-bind :i "TAB" #'completion-at-point)
;;;;; bind =H= and =L=
;; I find these bindings more useful on an everyday basis.
(oo-bind :n "H" #'evil-first-non-blank)
(oo-bind :n "L" #'evil-last-non-blank)
;;;;; g is kind of like the main prefix key of vim
;; The =g= prefix contains lots of things including the beginning of buffer.
;; I will use =ge= as the eval operator because especially in an interactive
;; editor like emacs being able to eval on demand is very important.
(autoload #'evil-operator-eval "evil-extra-operator")
(autoload #'evil-operator-eval-replace "evil-extra-operator")
;; =g a= is a bit easier to press than =g e= on a QWERTY keyboard.
;; TODO: Maybe make an operator for eval print.
;; TODO: Make evil use symbol object instead of word object in lisp code.

;; I feel like the eval operator is so important that it should have =gg=.  I
;; use this a lot more than the original =gg= binding for going to the beginning
;; of buffer.
;; (oo-bind :nv "g g" #'evil-operator-eval)
(oo-bind :nv "g j" #'evil-operator-eval-replace)

;; The command `lispyville-comment-or-uncomment' lags when commenting or
;; uncommenting in html buffers.  Its designed for lisp and gets confused with
;; the pairs in html somehow.  That is why it is better to use
;; `evilnc-comment-operator' in non-lisp buffers.
(oo-bind :nv "g c" #'lispyville-comment-or-uncomment)
(oo-bind :nv "g l" #'lispyville-comment-and-clone-dwim)
;; (oo-bind :nv "g c" #'evilnc-comment-operator)
;; (oo-bind :nv "g l" #'evilnc-)

(oo-bind :nv "g x" #'evil-exchange)
(oo-bind :nv "g X" #'evil-exchange-cancel)
(oo-bind :nv "g a" #'evil-exchange)
(oo-bind :nv "g A" #'evil-exchange-cancel)
;; TODO: consider switching =f= and =g=.  I feel that I will use =g= more than
;; =f=.
;; (oo-bind :nv "g b")

;; These two actually are bound the other way around, but in my opinion I need to
;; go from downcase to upcase more than from upcase to downcase.  So I would
;; rather the lowercase you be for upcasing.
(oo-bind :nv "g u" #'evil-upcase)
(oo-bind :nv "g U" #'evil-downcase)

(oo-bind :nv "g r" #'evil-goto-first-line)
(oo-bind :nv "g R" #'evil-goto-line)

(oo-bind 'evil-inner-text-objects-map "c" #'evil-cp-inner-comment)

;; Add textobj syntax operator.  This is very interesting.
(autoload #'evil-i-syntax "evil-textobj-syntax")
(autoload #'evil-a-syntax "evil-textobj-syntax")
(oo-bind 'evil-inner-text-objects-map "h" #'evil-i-syntax)
(oo-bind 'evil-outer-text-objects-map "h" #'evil-a-syntax)

;; toggle-map
(oo-bind 'oo-toggle-map "r" #'read-only-mode)
(oo-bind 'oo-toggle-map "t" #'load-theme)
(oo-bind 'oo-toggle-map "d" #'toggle-debug-on-error)

(oo-bind 'oo-toggle-map "p" (lambda () (interactive) (profiler-start 'cpu+mem)))
(oo-bind 'oo-toggle-map "P" #'profiler-stop)
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
;;;; eshell
(oo-bind 'oo-app-map "e" #'eshell)
;;;; lispy
(oo-bind 'lispyville-mode-map :i "SPC" #'lispy-space)
(oo-bind 'lispyville-mode-map :i ";" #'lispy-comment)
;; (oo-bind :v "E" #'lispy-eval-and-replace)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)
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
;;;; select a window with =w=, =j= or =o=
;; There are commands such as.  I do not need these commands.  After moving left,
;; right, up or down some direction once, the effort needed to traverse a window
;; using directional window movement commands greatly increases.  The command
;; [[file:snapshots/_helpful_command__ace-window][ace-window]] in contrast scales really well with a greater number of
;; windows.  And it only loses slightly to directional window commands when moving
;; one time.

;; The command [[file:snapshots/_helpful_command__ace-window_.png][ace-window]] leverages [[https://github.com/abo-abo/avy][avy]] to select a window.  It assigns each window
;; a character (I'm using [[][letters]] close to the homerow) which it displays on
;; the upper right-hand corner of windows. I've found that
;; ace-window is the quickest way possible to switch focus form one live window to
;; another.

;; The mnemonic bind is =w= and the quick bindings--which I will likely use most
;; often--are =o= and =j=.
(oo-bind 'oo-window-map "w" #'ace-window :wk "select")
(oo-bind 'oo-window-map "j" #'ace-window :wk "select")
(oo-bind 'oo-window-map "o" #'ace-window :wk "select")
;;;; swap two windows with =s=
;; Often when I want to switch focus from my main window to one of its
;; subsidiaries; I will want to swap buffers from the two windows.
;; Actually, =edwina= does provide functions to do this: namely
;; [[_helpful_command__edwina-swap-next-window_.png][edwina-swap-next-window]] and [[file:snapshots/_helpful_command__edwina-swap-previous-window_.png][edwina-swap-previous-window]].
;; But I can do something similar, but much faster with.  This is a case where =s= is
;; mnemonic and easy to press.
(oo-bind 'oo-window-map "s" #'ace-swap-window :wk "swap")
;;; provide
(provide 'oo-init-keybindings)
;;; oo-init-keybindings.el ends here
