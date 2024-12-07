;;; oo-init.el -*- lexical-binding: t; -*-
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
;; This file loads everything that needs to be evaluated immediately on startup.
;;
;;; Code:
;;;; requirements
(require 'base)
(require 'oo-init-hooks)
(require 'oo-init-keybindings)
;;;; set initial font
;; This is very basic font setting based on available faces.  I have seen much
;; more complex font setups like in minemacs (which probably got its from doom)
;; but for now this will do.
(defvar oo-default-fonts (list (font-spec :family "RecMonoDuotone Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "CaskaydiaCove Nerd Font Mono"
                                          :weight 'light
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "Mononoki Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "JetBrainsMono Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18))
  "List of fonts to check.")

(defun! oo-set-default-font-h ()
  "Set the default font based on available fonts."
  (dolist (font oo-default-fonts)
    (trace! "Checking whether %s font is available..." font)
    (awhen (find-font font)
      (info! "Setting font to...%s" it)
      (set-face-attribute 'default nil :font font)
      (done!)))
  (set! default-font (face-attribute 'default :family))
  (info! "Unable to set font to any in `oo-default-font-list', defaulting to `%s'." default-font))

(add-hook 'after-init-hook #'oo-set-default-font-h 80)
;;;; sort lines
(defun! oo-sort-elpaca-forms-h ()
  "Sort elpaca package forms in current buffer."
  (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (set! beg (point-min))
  (set! end (point-max))
  (save-excursion
    (goto-char beg)
    (when (re-search-forward rx end t nil)
      (sort-regexp-fields nil rx "\\1" (match-beginning 0) end))))

(defhook! oo-setup-auto-line-sorting-maybe-h (find-file-hook)
  "Setup auto line sorting for `init-elpaca'."
  (set! path "~/.local/share/chezmoi/dot_config/emacs/lisp/init-elpaca.el")
  (when (f-same-p (buffer-file-name) (f-full path))
    (info! "Setup auto-sorting for %s..." (f-base path))
    (hook! before-save-hook oo-sort-elpaca-forms-h :local t)))

(defun! oo-align-abbrev-forms-h ()
  "Align all abbrev forms in current buffer."
  (set! regexp "(define-abbrev\\(?1:\\s-+\\)\\S-+\\(?2:\\s-+\\)\".*?\"\\(?3:\\s-+\\)\".*?\"\\(?4:\\s-+\\)\\S-+\\(?5:\\s-+\\):enable-function\\(?6:\\s-+\\).+)")
  (set! rules `((rule1 . ((regexp . ,regexp) (group . (1 2 3 4 5 6))))))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward rx nil t nil)
      (shut-up (align (match-beginning 0) (point-max) nil rules)))))

(defhook! oo-setup-auto-alignment-maybe-h (find-file-hook)
  "Set up auto alignment for certain buffers."
  (set! path "~/.local/share/chezmoi/dot_config/emacs/lisp/+abbrev-plain-text-abbrevs.el")
  (when (f-same-p (buffer-file-name) (f-full path))
    (info! "Setup auto-aligning for %S..." (f-base path))
    (add-hook 'before-save-hook #'oo-align-abbrev-forms-h nil t)))
;;;; Start emacs server
;; This is so that if I need to use some sort of program to open a file, it will
;; use he running emacs daemon.
;; (unless (server-running-p) (server-start))
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
