;;; init-simple.el --- initialize simple -*- lexical-binding: t; -*-
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
;; Initialize simple.
;;
;;; Code:
(hook! text-mode-hook&visual-line-mode)
;;;; disable the blinking of matching parentheses
;; This made scrolling the cursor really slow.  Maybe because it was enabled
;; with =show-parens-mode= at the same time.  This isn't needed if I have
;; =show-parens-mode= already enabled.
(setq blink-matching-paren nil)
;;;; don't suggest keybindings or the like for me
;; See [[https://stackoverflow.com/questions/19781529/how-to-disable-emacs-messages-like-you-can-run-the-command-x-with-y][this stackoverflow post]].  After invoking [[file:snapshots/*helpful command: execute-extended-command*.png][execute-extended-command]] on a
;; command that has an existing keybinding, or something that could be abbreviated,
;; emacs will suggest a shorter way.
(setq suggest-key-bindings nil)
;;; provide
(provide 'init-simple)
;;; init-simple.el ends here
