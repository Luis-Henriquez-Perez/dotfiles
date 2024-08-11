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
(alet (or (font-spec :name "Martian Mono Nerd Font"
                     :weight 'normal
                     :slant 'normal
                     :size 14)
          (font-spec :name "Nimbus Mono PS"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "Iosevka Comfy Wide"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "SpaceMono Nerd Font"
                     :weight 'normal
                     :slant 'normal
                     :size 15)
          (font-spec :name "iMWritingMono Nerd Font Mono"
                     :weight 'normal
                     :slant 'normal
                     :size 15))
  (set-face-attribute 'default nil :font it))
;;;; less confusing kill buffer
;; https://christiantietze.de/posts/2023/09/kill-unsaved-buffer-ux-action-labels/
(defun! oo--prompt-in-less-confusing-way (_ buffer &rest args)
  "Ask user in the minibuffer whether to save before killing.
Replace `kill-buffer--possibly-save' as advice."
  (set! prompt (format "Buffer %s modified." (buffer-name)))
  (set! choices '((?s "Save and kill buffer" "save the buffer and then kill it")
                  (?d "Discard and kill buffer without saving" "kill buffer without saving")
                  (?c "Cancel" "Exit without doing anything")))
  (set! long-form (and (not use-short-answers) (not (use-dialog-box-p))))
  (set! response (car (read-multiple-choice prompt choices nil nil long-form)))
  (cl-case response
    (?s (with-current-buffer buffer (save-buffer)) t)
    (?d t)
    (t nil)))

(advice-add 'kill-buffer--possibly-save :around #'oo--prompt-in-less-confusing-way)
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
