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
(require 'base)
(require 'oo-override-mode)
(eval-when-compile (require 'base-macros-bind))
;;;; keybindings
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
;;;;;; oo-buffer-map
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(bind! oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
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
;;;;;; oo-app-map
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(bind! oo-leader-map "a" #'oo-app-prefix-command :wk "app")
;;;;;; oo-find-map
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(bind! oo-leader-map "f" #'oo-find-prefix-command :wk "find")

(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "I" #'oo-open-emacs-config)
(bind! oo-find-map "i" #'imenu)
(bind! oo-find-map "j" #'oo-dwim-narrow)
(bind! oo-find-map "n" #'oo-dwim-narrow)
(bind! oo-find-map "o" #'find-file)
(bind! oo-find-map "f" #'switch-to-buffer)
(bind! oo-find-map "d" #'display-buffer)

(bind! oo-find-map "a" #'find-library)
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

(bind! oo-toggle-map "l" #'display-line-numbers-mode)
(bind! oo-toggle-map "u" #'toggle-truncate-lines)
(bind! oo-toggle-map "n" #'oo-dwim-narrow)
(bind! oo-toggle-map "i" #'iedit-mode)
(bind! oo-toggle-map "e" #'eval-expression)
(bind! oo-toggle-map "f" #'oo-set-font-face)
(bind! oo-toggle-map "r" #'read-only-mode)
(bind! oo-toggle-map "d" #'toggle-debug-on-error)
(bind! oo-toggle-map "P" #'profiler-stop)
(bind! oo-toggle-map "s" #'smartparens-mode)
;;;;;; oo-dotfile-map
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(bind! oo-leader-map "d" #'oo-dotfile-prefix-command :wk "dotfile")
;;;;;; oo-quit-map
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(bind! oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")
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
;;;;; package specific
;; (bind! oo-buffer-map "i" #'oo-open-emacs-init-file)
;; (bind! oo-buffer-map "I" #'oo-open-emacs-config)
;; (bind! oo-buffer-map "I" #'oo-open-emacs-lisp-dir)
;;; provide
(provide 'oo-init-keybindings)
;;; oo-init-keybindings.el ends here
