;;; base-vars.el --- core variables -*- lexical-binding: t; -*-
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
;; This file defines constants.  It is modeled after
;; https://github.com/d12frosted/environment/tree/master/emacs.
;;
;;; Code:
(defconst oo-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a value of $HOME.")

(defconst oo-xdg-config-dir (file-name-as-directory
                             (or (getenv "XDG_CONFIG_HOME")
                                 (concat oo-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst oo-emacs-dir (file-name-as-directory (expand-file-name "emacs/" oo-xdg-config-dir))
  "The path to this Emacs directory.")

(defconst oo-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Directory where")
;;;; uniform location to store files
(defvaralias 'oo-config-dir 'oo-etc-dir)
(defconst oo-etc-dir (expand-file-name "etc/" user-emacs-directory)
  "Directory where.")

(make-directory oo-etc-dir t)

(defvaralias 'oo-data-dir 'oo-var-dir)
(defconst oo-var-dir (expand-file-name "var/" user-emacs-directory))

(make-directory oo-var-dir t)
;;;; store errors that occur in hooks and advices
;; This is influenced by the excellent stackoverflow on
;; [[https://emacs.stackexchange.com/questions/669/how-to-gracefully-handle-errors-in-init-file][how-to-gracefully-handle-errors-in-init-file]].  The idea is that I don't want
;; errors to interfere with startup or with my usage of Emacs.  But I also don't
;; want to ignore them altogether.  Instead, I want to raise them when I decide
;; to I want to deal with them; from the comfort of a working Emacs
;; configuration.
(defvar oo-errors nil
  "An alist of errors that occur in advices or hooks during Emacs startup.
Each element looks like (HOOK-OR-ADVICE . ERROR).  HOOK-OR-ADVICE is a hook or
an advice symbol that raised an error.  ERROR is the error object generated by
HOOK-OR-ADVICE.")
;;;; determine whether emacs is in debug mode
;; This variable is snatched from [[https://github.com/hlissner/doom-emacs][Doom]].  The point of this variable is to serve as
;; an indicator of whether the current Emacs instance is run for
;; debugging. Normally, when starting Emacs I suppress non-critical errors so as
;; not to interfere with startup; but if I know I'm specifically debugging my
;; configuration using this variable then I might disable the suppression of
;; errors.
(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")
;; ;;; log buffer
;; (defvar oo-log-buffer (get-buffer-create "*oo-log*")
;;   "Buffer where information should be logged.")
;;;; system information
;;;; keybinding leaders
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
;;; provide
(provide 'base-vars)
;;; base-vars.el ends here
