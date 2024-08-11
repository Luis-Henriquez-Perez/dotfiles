;;; oo-override-mode.el --- Initialize oo-override-mode -*- lexical-binding: t; -*-
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
;; Provide mode for overriding keybindings.
;;
;;; Code:
;; Creating a minor mode to hold the leader map allows us to toggle our leader
;; bindings on or off.

;; Enabling =override-mode= needs to be the first thing we do in
;; =emacs-startup-hook=, or at least it needs to be before modes that set
;; keybindings like evil.  Otherwise, your bindings might not take effect
;; immediately.  This is why I set the advice depth to =-100=.
(defvar oo-override-mode-map (make-sparse-keymap)
  "Keymap made to override everything.")

(define-minor-mode oo-override-mode
  "Global minor mode for higher precedence evil keybindings."
  :keymap oo-override-mode-map
  :group 'oo
  :global t)

;; Looking at the [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html][Emacs keymap hierarchy]], emulation mode maps is pretty up
;; there.  The [[helpvar:emulation-mode-map-alists][emulation-mode-map-alists]]
(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))
;;; provide
(provide 'oo-override-mode)
;;; oo-override-mode.el ends here
