;;; oo-base-leaders.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; create a keymap that overrides most other keymaps
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
  :global t)

(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)
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

;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

(oo-bind :g   'oo-override-mode-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(oo-bind :i   'oo-override-mode-map oo-insert-leader-key #'oo-leader-prefix-command)
(oo-bind :nmv 'oo-override-mode-map oo-normal-leader-key #'oo-leader-prefix-command)

;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(oo-bind :nmv 'oo-override-mode-map ";" #'execute-extended-command)
(oo-bind oo-leader-map oo-normal-leader-key #'execute-extended-command :wk "execute command")
(oo-bind :i "A-x" #'execute-extended-command)
(oo-bind :i "M-x" #'execute-extended-command)

(defun! oo--bind-localleader (fns metadata)
  "Automate binding to leader."
  (set! alist '((oo-normal-localleader-short-key . normal)
                (oo-normal-localleader-key . normal)
                (oo-insert-localleader-key . insert)
                (oo-insert-localleader-short-key . insert)
                (oo-emacs-localleader-key . emacs)
                (oo-emacs-localleader-key . global)))
  (set! key (map-elt metadata :key))
  (set! localleader (map-elt metadata :localleader))
  (if localleader
      (for! ((leader . state) alist)
        (set! new-key (if (vectorp key)
                          key
                        (concat (symbol-value leader) "\s" key)))
        (alet (-> metadata
                  (map-insert :key new-key)
                  (map-insert :state state))
          (oo--resolve-binding fns it)))
    (oo--resolve-binding fns metadata)))

(adjoining! oo-binding-fns #'oo--bind-localleader)
;;;; keybindings
;;;;; setup leader maps
(defvar oo-toggle-map (make-sparse-keymap))
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(oo-bind 'oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")

(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(oo-bind 'oo-leader-map "p" #'oo/package-prefix-command :wk "package")

(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(oo-bind 'oo-leader-map "f" #'oo-find-prefix-command :wk "find")

(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(oo-bind oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(oo-bind oo-leader-map "l" #'oo-help-prefix-command :wk "help")
(oo-bind oo-leader-map "h" #'oo-help-prefix-command :wk "help")

(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(oo-bind 'oo-leader-map "a" #'oo-app-prefix-command :wk "app")

(oo-bind 'oo-find-map "d" #'switch-to-buffer)
(oo-bind 'oo-find-map "f" #'display-buffer)

(oo-bind 'oo-quit-map "q" #'save-buffers-kill-emacs :wk "quit")
(oo-bind 'oo-quit-map "r" #'restart-emacs :wk "restart")
;; Should make this one restart with no prompt, just automatically save buffers
;; and exit processes.
(oo-bind 'oo-quit-map "R" #'restart-emacs :wk "restart")
(oo-bind 'oo-quit-map "E" #'restart-emacs-start-new-emacs :wk "new instance")

(oo-bind 'oo-app-map "E" #'restart-emacs-start-new-emacs :wk "new instance")

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(oo-bind oo-leader-map "l" #'oo-help-prefix-command :wk "help")
(oo-bind oo-leader-map "h" #'oo-help-prefix-command :wk "help")

(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(oo-bind oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
(oo-bind 'oo-buffer-map "j" #'next-buffer)
(oo-bind 'oo-buffer-map "k" #'previous-buffer)
(oo-bind 'oo-buffer-map "x" #'kill-current-buffer)
(oo-bind 'oo-buffer-map "b" #'switch-to-buffer)

(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(oo-bind oo-leader-map "w" #'oo-window-prefix-command :wk "window")

(oo-bind 'oo-toggle-map "f" #'oo-set-font-face)

(oo-bind 'oo-package-map "b" #'elpaca-browse     :wk "browse")
(oo-bind 'oo-package-map "U" #'elpaca-update-all :wk "update all")
(oo-bind 'oo-package-map "u" #'elpaca-update     :wk "update")
(oo-bind 'oo-package-map "v" #'elpaca-visit      :wk "visit")
(oo-bind 'oo-package-map "i" #'elpaca-try        :wk "try")
(oo-bind 'oo-package-map "r" #'elpaca-rebuild    :wk "rebuild")
(oo-bind 'oo-package-map "d" #'elpaca-delete     :wk "delete")
(oo-bind 'oo-package-map "l" #'elpaca-log        :wk "log")
(oo-bind 'oo-package-map "m" #'elpaca-manager    :wk "manager")

;; Emacs has a family of describe functions that are used for help and
;; introspection.  To name a few, there's [[file:snapshots/_helpful_command__describe-function_.png][describe-function]], [[file:snapshots/_helpful_command__describe-character_.png][describe-character]].
;; The command =describe-callable=  and =describe-variable= are the ones I use the most
;; by far and I them it to be accessible quickly.  The various snapshots you see
;; named are a result of these functions and you can already guess buy how many
;; such snapshots there are how much I use these commands.
(oo-bind oo-help-map "m" #'describe-mode)
(oo-bind oo-help-map "l" #'describe-function)
(oo-bind oo-help-map "f" #'describe-function)
(oo-bind oo-help-map "j" #'describe-variable)
(oo-bind oo-help-map "v" #'describe-variable)
(oo-bind oo-help-map "h" #'describe-variable)
(oo-bind oo-help-map "C" #'describe-char)
(oo-bind oo-help-map "k" #'describe-key)
;;;;; eval binding
;; I evaluate things so often and even in non-emacs
(defvar oo-eval-map (make-sparse-keymap))
(define-prefix-command 'oo-eval-prefix-command 'oo-eval-map)
(oo-bind 'oo-leader-map "e" #'oo-eval-prefix-command :wk "eval")
;;;;; maximize a window with =M=
;; Of course sometimes you want to focus on a window more than others.  Typically
;; this is handled with =edwina= because it makes the master window take up more than
;; =50%= of the width--by default =55%= (see [[file:snapshots/_helpful_variable__edwina-mfact_.png][edwina-mfact]]).
(oo-bind 'oo-window-map "M" #'maximize-window :wk "maximize")
;;;;; splitting windows
;; I'm unsure about whether to have any bindings splitting windows.  Since =edwina=
;; automates the way I split windows I do not use these splitting commands much.
;; However, I'm not so sure whether I should remove them entirely.
(oo-bind 'oo-window-map "v" #'split-window-horizontally :wk "vsplit")
(oo-bind 'oo-window-map "h" #'split-window-vertically :wk "hsplit")
;; (oo-bind 'oo-window-map "V" #'oo-split-window-right-and-focus :wk "vsplit+focus")
;; (oo-bind 'oo-window-map "v" #'oo-split-window-below-and-focus :wk "split+focus")
;;;;; undo changes to window configuration with =u=
;; There's a global mode called [[https://www.emacswiki.org/emacs/WinnerMode#:~:text=Winner%20Mode%20is%20a%20global%20minor%20mode%20that,included%20in%20GNU%20Emacs%2C%20and%20documented%20as%20winner-mode.][winner-mode]] that allow you to undo changes to
;; your window configuration.
(oo-bind 'oo-window-map "u" #'winner-undo :wk "undo")
;;;;; delete a window with =D= or =d=
;; The letter =d= is both mnemonic for deleting windows and it is easy to press
;; because its own the home key.
(oo-bind 'oo-window-map "d" #'delete-window :wk "delete")
(oo-bind 'oo-window-map "D" #'delete-other-windows :wk "delete others")
;;;;; open a new window with =k=
;; This binding overlaps with.  My reasoning is you can think of it as opening a
;; new window.
(oo-bind 'oo-window-map "k" #'display-buffer :wk "open")
;;;;; bind =TAB= to toggle children in =outshine-mode=
;; I need a way of folding headings.  This is a quick fix until I can
;; adapt the headline state I wrote about into a generic outline state.
;; (oo-bind 'outshine-mode-map :n "TAB" #'outline-toggle-children)
;;;;; similarly bind =TAB= to =outline-cycle= in =outli-mode=
;; (oo-bind 'outli-mode-map :n "TAB" #'outline-cycle)
;; (oo-bind 'outli-mode-map :n "TAB" #'outline-toggle-children)
;;;;; binding for =eval-expression=
;; I used to call this the eval leader map, but I do not know.
;; (oo-bind "e" #'eval-expression)
;;; provide
(provide 'oo-base-leaders)
;;; oo-base-leaders.el ends here
