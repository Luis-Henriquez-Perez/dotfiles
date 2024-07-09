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
(eval-when-compile (require 'oo-base-macros-bind-bang))
;;;; oo-override-map
;; Creating a minor mode to hold the leader map allows us to toggle our leader
;; bindings on or off.

;; Enabling =override-mode= needs to be the first thing we do in
;; =emacs-startup-hook=, or at least it needs to be before modes that set
;; keybindings like evil.  Otherwise, your bindings might not take effect
;; immediately.  This is why I set the advice depth to =-100=.
(defvar oo-override-mode-map (make-sparse-keymap))

(define-minor-mode oo-override-mode
  "Global minor mode for higher precedence evil keybindings."
  :keymap oo-override-mode-map
  :group 'oo
  :global t)

(hook! after-init-hook&oo-override-mode :depth -100)
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defhook! evil-mode-hook&make-intercept-map ()
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map oo-override-mode-map 'all t))

;; Looking at the [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html][Emacs keymap hierarchy]], emulation mode maps is pretty up
;; there.  The [[helpvar:emulation-mode-map-alists][emulation-mode-map-alists]]
(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))
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

(defconst oo-normal-localleader-key "SPC m"
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

(defconst oo-insert-localleader-key "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-localleader-key "C-c m"
  "The localleader prefix key for major-mode specific commands.")
;;;; keybindings
;;;;; leader keymap
;;;;;; root map
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(bind! :g   oo-override-mode-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(bind! :i   oo-override-mode-map oo-insert-leader-key #'oo-leader-prefix-command)
(bind! :nmv oo-override-mode-map oo-normal-leader-key #'oo-leader-prefix-command)

;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(bind! :nmv oo-override-mode-map ";" #'execute-extended-command)
(bind! oo-leader-map oo-normal-leader-key #'execute-extended-command :wk "execute command")
;;;;;; oo-buffer-map
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(bind! oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
(bind! oo-buffer-map "j" #'next-buffer)
(bind! oo-buffer-map "k" #'previous-buffer)
(bind! oo-buffer-map "x" #'kill-current-buffer)
(bind! oo-buffer-map "b" #'switch-to-buffer)
;;;;;; oo-vc-map
(defvar oo-vc-map (make-sparse-keymap))
(define-prefix-command 'oo-vc-prefix-command 'oo-vc-map)
(bind! oo-leader-map "v" #'oo-vc-prefix-command :wk "vc")

(bind! oo-vc-map "s" #'vc-st)
(bind! oo-vc-map "p" #'vc-push)
(bind! oo-vc-map "n" #'vc-next-action)
(bind! oo-vc-map "b" #'vc-create-branch)
;;;;;; oo-magit-map
(defvar oo-magit-map (make-sparse-keymap))
(define-prefix-command 'oo-magit-prefix-command 'oo-magit-map)
(bind! oo-leader-map "g" #'oo-magit-prefix-command :wk "git")

(bind! oo-magit-map "s" #'magit-status)
(bind! oo-magit-map "p" #'magit-push)
;; (bind! oo-magit-map "n" #'vc-next-action)
(bind! oo-magit-map "c" #'magit-commit)
;; (bind! oo-magit-map "b" #'vc-create-branch)
(bind! oo-magit-map "b" #'magit-branch)
;;;;;; oo-window-map
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(bind! oo-leader-map "w" #'oo-window-prefix-command :wk "window")

(bind! oo-window-map "t" #'transpose-frame :wk "transpose")
(bind! oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(bind! oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
(bind! oo-window-map "w" #'ace-window :wk "select")
(bind! oo-window-map "j" #'burly-open-bookmark)
(bind! oo-window-map "o" #'ace-window :wk "select")
(bind! oo-window-map "s" #'ace-swap-window :wk "swap")
(bind! oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(bind! oo-window-map "b" #'balance-windows :wk "balance")
(bind! oo-window-map "M" #'maximize-window :wk "maximize")
(bind! oo-window-map "v" #'split-window-horizontally :wk "vsplit")
(bind! oo-window-map "h" #'split-window-vertically :wk "hsplit")
(declare-function winner-undo nil)
(bind! oo-window-map "u" #'winner-undo :wk "undo")
(bind! oo-window-map "d" #'delete-window :wk "delete")
(bind! oo-window-map "D" #'delete-other-windows :wk "delete others")
(bind! oo-window-map "k" #'display-buffer :wk "open")
;;;;;; oo-app-map
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(bind! oo-leader-map "a" #'oo-app-prefix-command :wk "app")

(bind! oo-app-map "d" #'dired)
(bind! oo-app-map "n" #'notmuch)
(bind! oo-app-map "m" #'mu4e)
(bind! :alt dired dirvish :feature dirvish)
(bind! oo-app-map "e" #'eshell)
(bind! oo-app-map "E" #'restart-emacs-start-new-emacs :wk "new instance")
;;;;;; oo-find-map
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(bind! oo-leader-map "f" #'oo-find-prefix-command :wk "find")

(bind! :alt imenu consult-imenu :feature consult)
(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "a" #'imenu)
(bind! oo-find-map "k" #'consult-yank-pop)
(bind! oo-find-map "j" #'oo-dwim-narrow)
(bind! oo-find-map "o" #'find-file)
(bind! oo-find-map "f" #'switch-to-buffer)
(bind! oo-find-map "d" #'pop-to-buffer)

;; (bind! oo-find-map "i" #'oo-open-emacs-init-file)
;; (bind! oo-find-map "e" #'oo-open-emacs-directory)

;; (bind! oo-find-map "b" #'burly-open-bookmark :wk "bookmark")
(bind! oo-find-map "b" #'consult-bookmark :wk "bookmark")
(bind! oo-find-map "l" #'consult-line :wk "line")
(bind! oo-find-map "s" #'find-library :wk "library")
(bind! oo-find-map "h" #'consult-outline :wk "outline")
(bind! oo-find-map "g" #'consult-grep :wk "grep")
;;;;;; oo-help-map
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(bind! oo-leader-map "l" #'oo-help-prefix-command :wk "help")
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
;;;;;; emms
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
;;;;;; oo-workspace-map
(defvar oo-workspace-map (make-sparse-keymap))
(define-prefix-command 'oo-workspace-prefix-command 'oo-workspace-map)
(bind! oo-leader-map "k" #'oo-workspace-prefix-command :wk "workspace")

(bind! oo-workspace-map "j" #'tab-next :wk "workspace")
(bind! oo-workspace-map "k" #'tab-previous :wk "workspace")
(bind! oo-workspace-map "d" #'tab-close :wk "workspace")
(bind! oo-workspace-map "o" #'burly-bookmark-windows :wk "workspace")
;;;;;; oo-quick-map
(defvar oo-quick-map (make-sparse-keymap))
(define-prefix-command 'oo-quick-prefix-command 'oo-quick-map)
(bind! oo-leader-map "j" #'oo-quick-prefix-command :wk "quick")

(bind! oo-quick-map "d" #'eval-expression)
(bind! oo-quick-map "j" #'oo-dwim-narrow)
(bind! oo-quick-map "f" #'grugru)
;;;;;; oo-toggle-map
(defvar oo-toggle-map (make-sparse-keymap))
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

;; (bind! oo-toggle-map "p" (lambda () (interactive) (profiler-start 'cpu+mem)))
(bind! oo-toggle-map "P" #'profiler-stop)
(bind! oo-toggle-map "s" #'smartparens-mode)
;;;;;; oo-dotfile-map
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(bind! oo-leader-map "d" #'oo-dotfile-prefix-command :wk "dotfiles")

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

(bind! oo-quit-map "R" #'restart-emacs :wk "restart")
(bind! oo-quit-map "E" #'restart-emacs-start-new-emacs :wk "new instance")
(bind! oo-quit-map "q" #'save-buffers-kill-emacs :wk "quit")
(bind! oo-quit-map "r" #'restart-emacs :wk "restart")
;;;;;; oo-package-map
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)

(bind! oo-package-map "b" #'elpaca-browse     :wk "browse")
(bind! oo-leader-map "p" #'oo/package-prefix-command :wk "package")
(bind! oo-package-map "U" #'elpaca-update-all :wk "update all")
(bind! oo-package-map "u" #'elpaca-update     :wk "update")
(bind! oo-package-map "v" #'elpaca-visit      :wk "visit")
(bind! oo-package-map "i" #'elpaca-try        :wk "try")
(bind! oo-package-map "r" #'elpaca-rebuild    :wk "rebuild")
(bind! oo-package-map "d" #'elpaca-delete     :wk "delete")
(bind! oo-package-map "l" #'elpaca-log        :wk "log")
(bind! oo-package-map "m" #'elpaca-manager    :wk "manager")
;;;;; level 1 bindings
;;;;;; abbreviations
(bind! :nie "C-c u" #'unexpand-abbrev)
(bind! :nie "C-c j" #'oo-add-new-abbrev)
;;;;;; lispy
(bind! lispyville-mode-map :i "SPC" #'lispy-space)
(bind! lispyville-mode-map :i ";" #'lispy-comment)
(bind! :i "TAB" #'completion-at-point)
;;;;;; miscellaneous
(bind! :i "A-x" #'execute-extended-command)
(bind! :i "M-x" #'execute-extended-command)
(bind! :nm "+" #'text-scale-increase)
(bind! :nm "-" #'text-scale-decrease)
(bind! :n "H" #'evil-first-non-blank)
(bind! :n "L" #'evil-last-non-blank)
(bind! :v "V" #'expreg-contract)
(bind! :v "v" #'expreg-expand)
;;;;;; motions
(bind! :nv "w" #'oo-evilem-motion-beginning-of-word)
(bind! :nv "W" #'oo-evilem-motion-beginning-of-WORD)
(bind! :nv "e" #'oo-evilem-motion-end-of-word)
(bind! :nv "E" #'oo-evilem-motion-end-of-WORD)
(bind! :nvo "f" #'oo-evilem-motion-char)
(bind! :nvo "H" #'oo-evilem-motion-beginning-of-line)
;;;;;; text objects
;;;;;;; line
(bind! evil-inner-text-objects-map "l" #'evil-inner-line)
(bind! evil-outer-text-objects-map "l" #'evil-a-line)
;;;;;;; block
;; Not sure what the difference is between block and form.
(bind! evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
(bind! evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
(bind! evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(bind! evil-outer-text-objects-map "f" #'evil-cp-a-form)
(bind! evil-inner-text-objects-map "c" #'evil-cp-inner-comment)
;;;;;;; syntax
(bind! evil-inner-text-objects-map "h" #'evil-i-syntax)
(bind! evil-outer-text-objects-map "h" #'evil-a-syntax)
;;;;;;; entire
(bind! evil-outer-text-objects-map "e" #'evil-entire-entire-buffer)
(bind! evil-inner-text-objects-map "e" #'evil-entire-entire-buffer)
;;;;;;; column
(bind! evil-inner-text-objects-map "k" 'evil-textobj-column-word)
(bind! evil-inner-text-objects-map "K" 'evil-textobj-column-WORD)
;;;;;; operators
;;;;;;; eval
(bind! :nv "g t" #'evil-goto-first-line)
(bind! :nv "g b" #'evil-goto-line)
(bind! :nv "g g" #'oo-eval-operator)
(bind! :nv "g h" #'oo-eval-operator)
(bind! :nv "g r" #'oo-eval-replace-operator)
(bind! :nv "g l" #'oo-eval-print-operator)
(bind! :nv "g p" #'oo-eval-print-operator)
;;;;;;; comment
(bind! :nv "g c" #'lispyville-comment-or-uncomment)
(bind! :nv "g l" #'lispyville-comment-and-clone-dwim)
;;;;;;; exchange
(bind! :nv "g x" #'evil-exchange)
(bind! :nv "g X" #'evil-exchange-cancel)
(bind! :nv "g a" #'evil-exchange)
(bind! :nv "g A" #'evil-exchange-cancel)
;;;;;; g is kind of like the main prefix key of vim
(bind! :nv "g u" #'evil-upcase)
(bind! :nv "g U" #'evil-downcase)
;;;;; alternate bindings
;;;;;; helpful
(bind! :alt describe-function helpful-callable :feature helpful)
(bind! :alt describe-command  helpful-command  :feature helpful)
(bind! :alt describe-variable helpful-variable :feature helpful)
(bind! :alt describe-key      helpful-key      :feature helpful)
;;;;;; consult
(bind! :alt switch-to-buffer consult-buffer   :feature consult)
(bind! :alt yank-pop         consult-yank-pop :feature consult)
(bind! :alt apropos          consult-apropos  :feature consult)
(bind! :alt man              consult-man      :feature consult)
;;;;; localleaders
;;;;;; macrostep
(bind! emacs-lisp-mode-map "me" #'macrostep-expand       :localleader t :wk "expand")
(bind! emacs-lisp-mode-map "mc" #'macrostep-collapse     :localleader t :wk "collapse")
(bind! emacs-lisp-mode-map "mC" #'macrostep-collapse-all :localleader t :wk "collapse all")
;;;;; consult
(opt! consult-preview-key nil)

(opt! consult-fontify-preserve nil)

(defun! oo-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))

(bind! :alt pop-to-buffer oo-pop-to-buffer)
;;;;; dired
;; Dired is very picky about when these bindings happen.  It is the only package
;; I have had that is that picky.  I have noticed that unlike every other
;; package I have tried dired bindings do not work by trying to set them when
;; `dired-mode-map' is bound.  You need to use (eval-after-load 'dired ...).
;; Also, even if you have the `eval-after-load' it work work from the
;; `oo-after-load-dired' file--do not ask me why.  Again, only package I have
;; had this happen with.
(bind! dired-mode-map :nm "h" #'dired-up-directory)

(bind! dired-mode-map :nm "l" #'dired-find-file)
;;; provide
(provide 'oo-init-keybindings)
;;; oo-init-keybindings.el ends here
