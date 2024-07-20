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
(eval-when-compile (require 'oo-base-macros-bind-bang))
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
;;;;; overriding map
(bind! i oo-override-mode-map oo-insert-leader-key #'oo-leader-prefix-command)
(bind! (n m v) oo-override-mode-map oo-normal-leader-key #'oo-leader-prefix-command)
(bind! (n m v) oo-override-mode-map ";" #'execute-extended-command)
(bind! (i e) [escape] #'evil-force-normal-state)
;;;;; leader keymap
;;;;;; root map
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(bind! oo-override-mode-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(bind! oo-override-mode-map "C-c SPC"  #'oo-leader-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "l" "localleader"))
;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(bind! oo-leader-map oo-normal-leader-key #'execute-extended-command)
;;;;;; oo-buffer-map
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(bind! oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")

(bind! oo-buffer-map "j" #'next-buffer)
(bind! oo-buffer-map "k" #'previous-buffer)
(bind! oo-buffer-map "x" #'kill-current-buffer)
(bind! oo-buffer-map "b" #'switch-to-buffer)
;;;;;; oo-git-map
(defvar oo-magit-map (make-sparse-keymap))
(define-prefix-command 'oo-magit-prefix-command 'oo-magit-map)
(bind! oo-leader-map "g" #'oo-magit-prefix-command :wk "magit")

(bind! oo-magit-map "s" #'magit-status)
(bind! oo-magit-map "p" #'magit-push)
(bind! oo-magit-map "n" #'vc-next-action)
(bind! oo-magit-map "c" #'magit-commit)
(bind! oo-magit-map "b" #'vc-create-branch)
(bind! oo-magit-map "B" #'magit-branch)
;;;;;; oo-window-map
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(bind! oo-leader-map "w" #'oo-window-prefix-command :wk "window")

(bind! oo-window-map "t" #'transpose-frame)
(bind! oo-window-map "S" #'burly-bookmark-windows)
(bind! oo-window-map "b" #'burly-bookmark-windows)
(bind! oo-window-map "w" #'ace-window)
(bind! oo-window-map "j" #'ace-window)
(bind! oo-window-map "o" #'ace-window)
(bind! oo-window-map "s" #'ace-swap-window)
(bind! oo-window-map "S" #'burly-bookmark-windows)
(bind! oo-window-map "b" #'balance-windows)
(bind! oo-window-map "M" #'maximize-window)
(bind! oo-window-map "v" #'split-window-horizontally)
(bind! oo-window-map "h" #'split-window-vertically)
(bind! oo-window-map "u" #'winner-undo)
(bind! oo-window-map "d" #'delete-window)
(bind! oo-window-map "D" #'delete-other-windows)
(bind! oo-window-map "k" #'display-buffer)
;;;;;; oo-app-map
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(bind! oo-leader-map "a" #'oo-app-prefix-command :wk "app")

(bind! oo-app-map "n" #'notmuch)
(bind! oo-app-map "d" #'dired)
(bind! oo-app-map "e" #'eshell)
(bind! oo-app-map "E" #'restart-emacs-start-new-emacs)
;;;;;; oo-find-map
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(bind! oo-leader-map "f" #'oo-find-prefix-command :wk "find")

(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "i" #'imenu)
(bind! oo-find-map "p" #'consult-yank-pop)
(bind! oo-find-map "j" #'oo-dwim-narrow)
(bind! oo-find-map "n" #'oo-dwim-narrow)
(bind! oo-find-map "o" #'find-file)
(bind! oo-find-map "f" #'switch-to-buffer)
(bind! oo-find-map "d" #'display-buffer)

(bind! oo-find-map "b" #'burly-open-bookmark)
(bind! oo-find-map "k" #'consult-bookmark)
(bind! oo-find-map "b" #'consult-bookmark)
(bind! oo-find-map "l" #'consult-line)
(bind! oo-find-map "a" #'find-library)
(bind! oo-find-map "h" #'consult-outline)
(bind! oo-find-map "g" #'consult-grep)
;;;;;; oo-help-map
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(bind! oo-leader-map "h" #'oo-help-prefix-command :wk "help")

;; Emacs has a family of describe functions that are used for help and
;; introspection.  To name a few, there's [[file:snapshots/_helpful_command__describe-function_.png][describe-function]], [[file:snapshots/_helpful_command__describe-character_.png][describe-character]].
;; The command =describe-callable=  and =describe-variable= are the ones I use the most
;; by far and I them it to be accessible quickly.  The various snapshots you see
;; named are a result of these functions and you can already guess buy how many
;; such snapshots there are how much I use these commands.
(bind! oo-help-map "m" #'describe-mode)
(bind! oo-help-map "l" #'describe-function)
(bind! oo-help-map "f" #'describe-function)
(bind! oo-help-map "j" #'describe-variable)
(bind! oo-help-map "v" #'describe-variable)
(bind! oo-help-map "h" #'describe-variable)
(bind! oo-help-map "C" #'describe-char)
(bind! oo-help-map "k" #'describe-key)
;;;;;; oo-emms-map
(defvar oo-emms-map (make-sparse-keymap))
(define-prefix-command 'oo-emms-prefix-command 'oo-emms-map)
(bind! oo-leader-map "e" #'oo-emms-prefix-command :wk "emms")

(bind! oo-emms-map "f" #'emms-play-file)
(bind! oo-emms-map "p" #'emms-pause)
(bind! oo-emms-map "P" #'emms-stop)
(bind! oo-emms-map "r" #'emms-toggle-repeat-track)
(bind! oo-emms-map "R" #'emms-toggle-repeat-playlist)
(bind! oo-emms-map "v" #'emms-volume-lower)
(bind! oo-emms-map "V" #'emms-volume-raise)
(bind! oo-emms-map "s" #'emms-seek-to)
;;;;;; oo-toggle-map
(defvar oo-toggle-map (make-sparse-keymap)
  "Keymap that contains bindings for things that should be toggled.")
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(bind! oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")

(bind! oo-toggle-map "u" #'toggle-truncate-lines)
(bind! oo-toggle-map "n" #'oo-dwim-narrow)
(bind! oo-toggle-map "i" #'iedit-mode)
(bind! oo-toggle-map "e" #'eval-expression)
(bind! oo-toggle-map "f" #'oo-set-font-face)
(bind! oo-toggle-map "r" #'read-only-mode)
(bind! oo-toggle-map "t" #'load-theme)
(bind! oo-toggle-map "d" #'toggle-debug-on-error)
(bind! oo-toggle-map "P" #'profiler-stop)
(bind! oo-toggle-map "s" #'smartparens-mode)
;;;;;; oo-dotfile-map
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(bind! oo-leader-map "d" #'oo-dotfile-prefix-command :wk "dotfile")

(bind! oo-dotfile-map "f" #'chezmoi-find)
;; I use the command =chezmoi-write= the most so far.  It syncs the current file
;; with its corresponding chezmoi file.  If called while in the target file, it
;; applies the changes in the target file to the source file and vice versa.
;; Only caveat is that if there is a more recent change in the "other" file,
;; then you have to use a prefix command to make sure you want to override those
;; changes.
(bind! oo-dotfile-map "w" #'chezmoi-write)
;; Binding to the "w" key is the more BLANK choice but "d" is closer to the
;; homerow for QWERTY keyboards.
(bind! oo-dotfile-map "d" #'chezmoi-write)
;; The command =chezmoi-open-other= is also useful.  Similar to =chezmoi-find=
;; it does something different depending on whether your in the source file or
;; the target file.  If you are in the source file, you open the target file and
;; vice versa.
(bind! oo-dotfile-map "o" #'chezmoi-open-other)
;;;;;; oo-quit-map
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(bind! oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")

(bind! oo-quit-map "R" #'restart-emacs)
(bind! oo-quit-map "E" #'restart-emacs-start-new-emacs)
(bind! oo-quit-map "q" #'save-buffers-kill-emacs)
(bind! oo-quit-map "r" #'restart-emacs)
;;;;;; oo-package-map
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(bind! oo-leader-map "p" #'oo/package-prefix-command :wk "package")

(bind! oo-package-map "b" #'elpaca-browse)
(bind! oo-package-map "U" #'elpaca-update-all)
(bind! oo-package-map "u" #'elpaca-update)
(bind! oo-package-map "v" #'elpaca-visit)
(bind! oo-package-map "i" #'elpaca-try)
(bind! oo-package-map "r" #'elpaca-rebuild)
(bind! oo-package-map "d" #'elpaca-delete)
(bind! oo-package-map "l" #'elpaca-log)
(bind! oo-package-map "m" #'elpaca-manager)
;;;;; alternate bindings
(alt! imenu consult-imenu consult)
(alt! dired dirvish dirvish)
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

(oo-localleader-bind emacs-lisp-mode-map "me" #'macrostep-expand)
(oo-localleader-bind emacs-lisp-mode-map "mc" #'macrostep-collapse)
(oo-localleader-bind emacs-lisp-mode-map "mC" #'macrostep-collapse-all)
;;;;; miscellaneous
(bind! "C-c j" #'oo-add-new-abbrev)
(bind! "C-c k" #'unexpand-abbrev)
(bind! i "TAB" #'completion-at-point)
;; (bind! n "x" #'hungry-delete-forward)
(bind! i "A-x" #'execute-extended-command)
(bind! i "M-x" #'execute-extended-command)
(bind! (n m) "+" #'text-scale-increase)
(bind! (n m) "-" #'text-scale-decrease)
(bind! n "H" #'evil-first-non-blank)
(bind! n "L" #'evil-last-non-blank)
(bind! v "V" #'expreg-contract)
(bind! v "v" #'expreg-expand)
(bind! n "J" #'evil-scroll-page-down)
(bind! n "K" #'evil-scroll-page-up)
;;;;; package specific 
;;;;;; consult
(opt! consult-preview-key nil)

(opt! consult-fontify-preserve nil)

(alt! display-buffer oo-pop-to-buffer consult)
;;;;;; evil 
;;;;;;; operators
;;;;;;;; eval
(bind! (n v) "g t" #'evil-goto-first-line)
(bind! (n v) "g b" #'evil-goto-line)
(bind! (n v) "g g" #'oo-eval-operator)
(bind! (n v) "g h" #'oo-eval-operator)
(bind! (n v) "g r" #'oo-eval-replace-operator)
(bind! (n v) "g l" #'oo-eval-print-operator)
(bind! (n v) "g p" #'oo-eval-print-operator)
;;;;;;;; comment
(bind! (n v) "g c" #'lispyville-comment-or-uncomment)
(bind! (n v) "g l" #'lispyville-comment-and-clone-dwim)
;;;;;;;; exchange
(bind! (n v) "g x" #'evil-exchange)
(bind! (n v) "g X" #'evil-exchange-cancel)
(bind! (n v) "g a" #'evil-exchange)
(bind! (n v) "g A" #'evil-exchange-cancel)
;;;;;;;; g is kind of like the main prefix key of vim
(bind! (n v) "g u" #'evil-upcase)
(bind! (n v) "g U" #'evil-downcase)
;;;;;;;; make `evil-for'
;; Pressing lowercase "o" is one less keystroke than "W" and it aligns with cio.
;; Though I will say I am not 100% sure it is the equivalent.
(bind! evil-motion-state-map "o" #'evil-forward-WORD-begin)
;;;;;;; motions
(bind! (n v) "w" #'oo-evilem-motion-beginning-of-word)
(bind! (n v) "W" #'oo-evilem-motion-beginning-of-WORD)
(bind! (n v) "e" #'oo-evilem-motion-end-of-word)
(bind! (n v) "E" #'oo-evilem-motion-end-of-WORD)
(bind! (n v o) "f" #'oo-evilem-motion-char)
(bind! (n v o) "H" #'oo-evilem-motion-beginning-of-line)
;;;;;;; text objects
;;;;;;;; line
(bind! evil-inner-text-objects-map "l" #'evil-inner-line)
(bind! evil-outer-text-objects-map "l" #'evil-a-line)
;;;;;;;; block
;; Not sure what the difference is between block and form.
(bind! evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
(bind! evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
(bind! evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(bind! evil-outer-text-objects-map "f" #'evil-cp-a-form)
(bind! evil-outer-text-objects-map "c" #'lispyville-outer-comment)
(bind! evil-inner-text-objects-map "c" #'lispyville-inner-comment)
;;;;;; helpful
(alt! describe-function helpful-callable helpful)
(alt! describe-command helpful-command helpful)
(alt! describe-variable helpful-variable helpful)
(alt! describe-key helpful-key helpful)
;;;;;; vertico
(bind! vertico-map "TAB" #'vertico-next)
(bind! vertico-map "C-k" #'vertico-previous)
(bind! vertico-map "C-j" #'vertico-next)
(bind! vertico-map ";" #'vertico-quick-exit)
(bind! vertico-map "C-;" #'vertico-quick-exit)
(bind! vertico-map [backtab] #'vertico-previous)
(bind! vertico-map "C-o" #'embark-act)
;;;;;; consult
(alt! switch-to-buffer consult-buffer consult)
(alt! yank-pop consult-yank-pop consult)
(alt! apropos consult-apropos consult)
(alt! man consult-man consult)
;;;;;; helm
(bind! i helm-map "TAB" #'helm-next-line)
(bind! i helm-map [backtab] #'helm-previous-line)
(bind! i helm-map "C-j" #'helm-next-line)
(bind! i helm-map "C-k" #'helm-previous-line)

(bind! i helm-map "C-a" #'helm-select-action)
(bind! i helm-map "C-m" #'helm-toggle-visible-mark-forward)
;; (bind! i helm-map :ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
(bind! i helm-map "S-TAB" #'helm-mark-current-line)

(bind! i helm-map "C-;" #'ace-jump-helm-line)
;;;;;; corfu
(bind! i corfu-map "<tab>"   #'corfu-next)
(bind! i corfu-map [backtab] #'corfu-previous)
(bind! i corfu-map "S-TAB"   #'corfu-previous)
(bind! i corfu-map "C-;"     #'corfu-quick-complete)
(bind! i corfu-map "C-j"     #'corfu-next)
(bind! i corfu-map "C-k"     #'corfu-previous)
(bind! i corfu-map "C-p"     #'corfu-previous)
(bind! i corfu-map ";"       #'corfu-quick-complete)
(bind! i corfu-map "SPC"     #'corfu-insert)
;;;;;; vertico
(bind! i vertico-map "TAB"     #'vertico-next)
(bind! i vertico-map "C-k"     #'vertico-previous)
(bind! i vertico-map "C-j"     #'vertico-next)
(bind! i vertico-map ";"       #'vertico-quick-exit)
(bind! i vertico-map "C-;"     #'vertico-quick-exit)
(bind! i vertico-map [backtab] #'vertico-previous)
(bind! i vertico-map "C-o"     #'embark-act)
;;;;;; lispy
(bind! i lispyville-mode-map "SPC" #'lispy-space)
(bind! i lispyville-mode-map ";" #'lispy-comment)
;;;;;; dired
;; Dired is very picky about when these bindings happen.  It is the only package
;; I have had that is that picky.  I have noticed that unlike every other
;; package I have tried dired bindings do not work by trying to set them when
;; `dired-mode-map' is bound.  You need to use (eval-after-load 'dired ...).
;; Also, even if you have the `eval-after-load' it work work from the
;; `oo-after-load-dired' file--do not ask me why.  Again, only package I have
;; had this happen with.
(bind! (n m) dired-mode-map "h" #'dired-up-directory)
(bind! (n m) dired-mode-map "l" #'dired-find-file)
;;; provide
(provide 'oo-init-keybindings)
;;; oo-init-keybindings.el ends here
