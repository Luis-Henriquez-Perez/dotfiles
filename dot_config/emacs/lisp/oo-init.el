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
(alet (or (font-spec :name "Iosevka Comfy Wide"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "Nimbus Mono PS"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "Martian Mono Nerd Font"
                     :weight 'normal
                     :slant 'normal
                     :size 14)
          (font-spec :name "SpaceMono Nerd Font"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "iMWritingMono Nerd Font Mono"
                     :weight 'normal
                     :slant 'normal
                     :size 15))
  (set-face-attribute 'default nil :font it))
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
    (oo-add-hook 'before-save-hook #'oo-sort-elpaca-forms-h :local t)))

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
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
