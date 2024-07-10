;;; oo-init-keybindings.el --- initial keybindings -*- lexical-binding: t; -*-
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
;; These are all my initial keybindings for my Emacs configuration.
;;
;;; Code:
(require 'oo-base)
(require 'oo-override-mode)
;;;; base leaders
;; This file provides leaders keys for evil and non-evil states and it binds
;; these leader keys.

;; These leaders are specifically for evil mode states (not including insert and
;;                                                          Emacs).  I choose the space (=SPC=) key for evil leaders because it is one of if
;; not the easiest key to press because of its central placement on the keyboard
;; and its sheer size--at least on the [[https://en.wikipedia.org/wiki/QWERTY][qwerty]] keyboard that I use.  The choice
;; of =SPC m= for the major mode specific keys is simply for the pnemonic =m= which
;; stands for "major mode".  The short major mode prefix key =,= is for cases when I
;; want to shorten a key binding.  Although obviously not as easy to remember as
;; =m=, it provides me with one shorter keypress in certain situations.
(defconst oo-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst oo-normal-localleader-key "SPC l"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-normal-localleader-short-key ","
  "A shorter alternative `oo-localleader-key'.")
;; These leaders are for evil insert and emacs states as well as vanilla
;; Emacs.  Note that evil Emacs state is different from vanilla Emacs.  One of the
;; goals with these bindings is to set up keybindings in the case that I disable
;; evil mode or in the case that I want to use my bindings in insert or Emacs
;; state--or even vanilla Emacs.  The choice behind the bindings is the same as
;; [[id:][before]], except I just prepended the =Meta= (a.k.a. the =Alt= key) to everything.
(defconst oo-insert-leader-key "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst oo-insert-localleader-key "M-SPC l"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-alternate-leader-key "C-c SPC")

(defconst oo-emacs-localleader-key "C-c l l"
  "The localleader prefix key for major-mode specific commands.")
;;;; keybindings
;;;;; leader keymap
;;;;;; root map
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(define-key oo-override-mode-map (kbd oo-emacs-leader-key)  #'oo-leader-prefix-command)
(define-key oo-override-mode-map (kbd "C-c SPC")  #'oo-leader-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "l" "localleader"))
;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(define-key oo-leader-map (kbd oo-normal-leader-key) #'execute-extended-command)
;;;;;; oo-buffer-map
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(define-key oo-leader-map "b" #'oo-buffer-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "b" "buffer"))

(define-key oo-buffer-map "j" #'next-buffer)
(define-key oo-buffer-map "k" #'previous-buffer)
(define-key oo-buffer-map "x" #'kill-current-buffer)
(define-key oo-buffer-map "b" #'switch-to-buffer)
;;;;;; oo-git-map
(defvar oo-magit-map (make-sparse-keymap))
(define-prefix-command 'oo-magit-prefix-command 'oo-magit-map)
(define-key oo-leader-map "g" #'oo-magit-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "g" "magit"))

(define-key oo-magit-map "g" #'oo-magit-prefix-command)
(define-key oo-magit-map "s" #'magit-status)
(define-key oo-magit-map "p" #'magit-push)
(define-key oo-magit-map "n" #'vc-next-action)
(define-key oo-magit-map "c" #'magit-commit)
(define-key oo-magit-map "b" #'vc-create-branch)
(define-key oo-magit-map "B" #'magit-branch)
;;;;;; oo-window-map
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(define-key oo-leader-map "w" #'oo-window-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "w" "window"))

(define-key oo-window-map "t" #'transpose-frame)
(define-key oo-window-map "S" #'burly-bookmark-windows)
(define-key oo-window-map "b" #'burly-bookmark-windows)
(define-key oo-window-map "w" #'ace-window)
(define-key oo-window-map "j" #'ace-window)
(define-key oo-window-map "o" #'ace-window)
(define-key oo-window-map "s" #'ace-swap-window)
(define-key oo-window-map "S" #'burly-bookmark-windows)
(define-key oo-window-map "b" #'balance-windows)
(define-key oo-window-map "M" #'maximize-window)
(define-key oo-window-map "v" #'split-window-horizontally)
(define-key oo-window-map "h" #'split-window-vertically)
(declare-function winner-undo nil)
(define-key oo-window-map "u" #'winner-undo)
(define-key oo-window-map "d" #'delete-window)
(define-key oo-window-map "D" #'delete-other-windows)
(define-key oo-window-map "k" #'display-buffer)
;;;;;; oo-app-map
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(define-key oo-leader-map "a" #'oo-app-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "a" "app"))

(define-key oo-app-map "d" #'dired)
(define-key oo-app-map "e" #'eshell)
(define-key oo-app-map "E" #'restart-emacs-start-new-emacs)
;;;;;; oo-find-map
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(define-key oo-leader-map "f" #'oo-find-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "f" "find"))

(define-key oo-find-map "i" #'imenu)
(define-key oo-find-map "l" #'consult-yank-pop)
(define-key oo-find-map "j" #'oo-dwim-narrow)
(define-key oo-find-map "n" #'oo-dwim-narrow)
(define-key oo-find-map "o" #'find-file)
(define-key oo-find-map "f" #'switch-to-buffer)
(define-key oo-find-map "d" #'display-buffer)

;; (define-key oo-find-map "i" #'oo-open-emacs-init-file)
;; (define-key oo-find-map "e" #'oo-open-emacs-directory)
(define-key oo-find-map "b" #'burly-open-bookmark)
(define-key oo-find-map "k" #'consult-bookmark)
(define-key oo-find-map "b" #'consult-bookmark)
(define-key oo-find-map "s" #'consult-line)
(define-key oo-find-map "a" #'find-library)
(define-key oo-find-map "h" #'consult-outline)
(define-key oo-find-map "g" #'consult-grep)
;;;;;; oo-help-map
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(define-key oo-leader-map "h" #'oo-help-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "h" "help"))

;; Emacs has a family of describe functions that are used for help and
;; introspection.  To name a few, there's [[file:snapshots/_helpful_command__describe-function_.png][describe-function]], [[file:snapshots/_helpful_command__describe-character_.png][describe-character]].
;; The command =describe-callable=  and =describe-variable= are the ones I use the most
;; by far and I them it to be accessible quickly.  The various snapshots you see
;; named are a result of these functions and you can already guess buy how many
;; such snapshots there are how much I use these commands.
(define-key oo-help-map "m" #'describe-mode)
(define-key oo-help-map "l" #'describe-function)
(define-key oo-help-map "f" #'describe-function)
(define-key oo-help-map "j" #'describe-variable)
(define-key oo-help-map "v" #'describe-variable)
(define-key oo-help-map "h" #'describe-variable)
(define-key oo-help-map "C" #'describe-char)
(define-key oo-help-map "k" #'describe-key)
;;;;;; oo-emms-map
(defvar oo-emms-map (make-sparse-keymap))
(define-prefix-command 'oo-emms-prefix-command 'oo-emms-map)
(define-key oo-leader-map "e" #'oo-emms-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "e" "emms"))

(define-key oo-emms-map "f" #'emms-play-file)
(define-key oo-emms-map "p" #'emms-pause)
(define-key oo-emms-map "P" #'emms-stop)
(define-key oo-emms-map "r" #'emms-toggle-repeat-track)
(define-key oo-emms-map "R" #'emms-toggle-repeat-playlist)
(define-key oo-emms-map "v" #'emms-volume-lower)
(define-key oo-emms-map "V" #'emms-volume-raise)
(define-key oo-emms-map "s" #'emms-seek-to)
;;;;;; oo-toggle-map
(defvar oo-toggle-map (make-sparse-keymap)
  "Keymap that contains bindings for things that should be toggled.")

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "t" "toggle"))

(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(define-key oo-leader-map "t" #'oo-toggle-prefix-command)
(define-key oo-toggle-map "u" #'toggle-truncate-lines)
(define-key oo-toggle-map "n" #'oo-dwim-narrow)
(define-key oo-toggle-map "i" #'iedit-mode)
(define-key oo-toggle-map "e" #'eval-expression)
(define-key oo-toggle-map "f" #'oo-set-font-face)
(define-key oo-toggle-map "r" #'read-only-mode)
(define-key oo-toggle-map "t" #'load-theme)
(define-key oo-toggle-map "d" #'toggle-debug-on-error)
(define-key oo-toggle-map "P" #'profiler-stop)
(define-key oo-toggle-map "s" #'smartparens-mode)
;;;;;; oo-dotfile-map
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(define-key oo-leader-map "d" #'oo-dotfile-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "d" "dotfile"))

(define-key oo-dotfile-map "f" #'chezmoi-find)
;; I use the command =chezmoi-write= the most so far.  It syncs the current file
;; with its corresponding chezmoi file.  If called while in the target file, it
;; applies the changes in the target file to the source file and vice versa.
;; Only caveat is that if there is a more recent change in the "other" file,
;; then you have to use a prefix command to make sure you want to override those
;; changes.
(define-key oo-dotfile-map "w" #'chezmoi-write)
;; Binding to the "w" key is the more BLANK choice but "d" is closer to the
;; homerow for QWERTY keyboards.
(define-key oo-dotfile-map "d" #'chezmoi-write)
;; The command =chezmoi-open-other= is also useful.  Similar to =chezmoi-find=
;; it does something different depending on whether your in the source file or
;; the target file.  If you are in the source file, you open the target file and
;; vice versa.
(define-key oo-dotfile-map "o" #'chezmoi-open-other)
;;;;;; oo-quit-map
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(define-key oo-leader-map "q" #'oo-quit-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "q" "quit"))

(define-key oo-quit-map "R" #'restart-emacs)
(define-key oo-quit-map "E" #'restart-emacs-start-new-emacs)
(define-key oo-quit-map "q" #'save-buffers-kill-emacs)
(define-key oo-quit-map "r" #'restart-emacs)
;;;;;; oo-package-map
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(define-key oo-leader-map "p" #'oo/package-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "p" "package"))

(define-key oo-package-map "b" #'elpaca-browse)
(define-key oo-package-map "U" #'elpaca-update-all)
(define-key oo-package-map "u" #'elpaca-update)
(define-key oo-package-map "v" #'elpaca-visit)
(define-key oo-package-map "i" #'elpaca-try)
(define-key oo-package-map "r" #'elpaca-rebuild)
(define-key oo-package-map "d" #'elpaca-delete)
(define-key oo-package-map "l" #'elpaca-log)
(define-key oo-package-map "m" #'elpaca-manager)
;;;;; alternate bindings
(alt! imenu consult-imenu consult)
(alt! dired dirvish dirvish)
;;;;;; helpful
(alt! describe-function helpful-callable helpful)
(alt! describe-command helpful-command helpful)
(alt! describe-variable helpful-variable helpful)
(alt! describe-key helpful-key helpful)
;;;;;; vertico 
(with-eval-after-load 'vertico
  (define-key vertico-map "TAB" #'vertico-next)
  (define-key vertico-map "C-k" #'vertico-previous)
  (define-key vertico-map "C-j" #'vertico-next)
  (define-key vertico-map ";" #'vertico-quick-exit)
  (define-key vertico-map "C-;" #'vertico-quick-exit)
  (define-key vertico-map [backtab] #'vertico-previous)
  (define-key vertico-map "C-o" #'embark-act))
;;;;;; consult
(alt! switch-to-buffer consult-buffer consult)
(alt! yank-pop consult-yank-pop consult)
(alt! apropos consult-apropos consult)
(alt! man consult-man consult)
;;;;; localleaders
(defun! oo-localleader-bind (keymap key def)
  "Convenience function for defining localleader bindings."
  (flet! leader (leader)
    (kbd (concat leader "\s" key)))
  (define-key keymap (leader oo-emacs-localleader-key) def)
  (with-eval-after-load 'evil
    (evil-define-key* 'emacs keymap (leader oo-emacs-localleader-key) def)
    (evil-define-key* 'normal keymap (leader oo-normal-localleader-key) def)
    (evil-define-key* 'normal keymap (leader oo-normal-localleader-short-key) def)
    (evil-define-key* 'insert keymap (leader oo-insert-localleader-key) def)
    (evil-define-key* 'insert keymap (leader oo-insert-localleader-short-key) def)))

(oo-localleader-bind emacs-lisp-mode-map "me" #'macrostep-collapse)
(oo-localleader-bind emacs-lisp-mode-map "mc" #'macrostep-collapse)
(oo-localleader-bind emacs-lisp-mode-map "mC" #'macrostep-collapse-all)
;;;;; consult
(opt! consult-preview-key nil)

(opt! consult-fontify-preserve nil)

(alt! display-buffer oo-pop-to-buffer consult)
;;; provide
(provide 'oo-init-keybindings)
;;; oo-init-keybindings.el ends here
