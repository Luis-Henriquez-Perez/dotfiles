;;; oo-init-leader-map.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
  :group 'oo
  :global t)

(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defhook! evil-mode-hook&make-intercept-map ()
  "Register `oo-override-mode-map' as an intercept map."
  (evil-make-intercept-map oo-override-mode-map 'all t))

;; Looking at the [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html][Emacs keymap hierarchy]], emulation mode maps is pretty up
;; there.  The [[helpvar:emulation-mode-map-alists][emulation-mode-map-alists]]
(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))
;;;; leader
(defconst oo-emacs-leader-key "C-c"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-localleader-key "C-c m"
  "The localleader prefix key for major-mode specific commands.")

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-map-command 'oo-leader-map)

(define-key oo-override-mode-map (kbd oo-emacs-leader-key) 'oo-leader-map-command)

(define-key oo-leader-map (kbd "SPC") 'execute-extended-command)
;;;; setup leader maps
(defvar oo-toggle-map (make-sparse-keymap))
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(define-key oo-leader-map "t" #'oo-toggle-prefix-command)

(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(define-key oo-leader-map "p" #'oo/package-prefix-command)

(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(define-key oo-leader-map "f" #'oo-find-prefix-command)

(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(define-key oo-leader-map "q" #'oo-quit-prefix-command)

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(define-key oo-leader-map "l" #'oo-help-prefix-command)
(define-key oo-leader-map "h" #'oo-help-prefix-command)

(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(define-key oo-leader-map "a" #'oo-app-prefix-command)

(define-key oo-find-map "d" #'switch-to-buffer)
(define-key oo-find-map "f" #'display-buffer)

(define-key oo-quit-map "q" #'save-buffers-kill-emacs)
(define-key oo-quit-map "r" #'restart-emacs)
;; Should make this one restart with no prompt, just automatically save buffers
;; and exit processes.
(define-key oo-quit-map "R" #'restart-emacs)
(define-key oo-quit-map "E" #'restart-emacs-start-new-emacs)

(define-key oo-app-map "E" #'restart-emacs-start-new-emacs)

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(define-key oo-leader-map "l" #'oo-help-prefix-command)
(define-key oo-leader-map "h" #'oo-help-prefix-command)

(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(define-key oo-leader-map "b" #'oo-buffer-prefix-command)
(define-key oo-buffer-map "j" #'next-buffer)
(define-key oo-buffer-map "k" #'previous-buffer)
(define-key oo-buffer-map "x" #'kill-current-buffer)
(define-key oo-buffer-map "b" #'switch-to-buffer)

(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(define-key oo-leader-map "w" #'oo-window-prefix-command)

(define-key oo-toggle-map "f" #'oo-set-font-face)

(define-key oo-package-map "b" #'elpaca-browse    )
(define-key oo-package-map "U" #'elpaca-update-all)
(define-key oo-package-map "u" #'elpaca-update    )
(define-key oo-package-map "v" #'elpaca-visit     )
(define-key oo-package-map "i" #'elpaca-try       )
(define-key oo-package-map "r" #'elpaca-rebuild   )
(define-key oo-package-map "d" #'elpaca-delete    )
(define-key oo-package-map "l" #'elpaca-log       )
(define-key oo-package-map "m" #'elpaca-manager   )

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

;; (bind! oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
;; (bind! oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
;; (bind! oo-find-map "j" #'burly-open-bookmark)
;;; provide
(provide 'oo-init-leader-map)
;;; oo-init-leader-map.el ends here
