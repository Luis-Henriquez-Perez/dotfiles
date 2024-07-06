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
(require 'oo-base)
(require 'oo-init-no-littering)
(require 'oo-init-abbrev)
(require 'oo-init-dashboard)
(require 'oo-init-recentf)
(require 'oo-init-hooks)
(require 'oo-init-keybindings)
;;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defadvice! load-theme@ARdisable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))
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
;;;; window divider
(opt! window-divider-default-bottom-width 7)
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-places t)
;;;; less confusing kill buffer
(defun oo--prompt-in-less-confusing-way (original-function buffer &rest args)
  "Ask user in the minibuffer whether to save before killing.

Replaces `kill-buffer--possibly-save' as advice, so
ORIGINAL-FUNCTION is unused and never delegated to. Its first
parameter is the buffer, which is the `car' or ARGS."
  (let ((response
         (car
          (read-multiple-choice
           (format "Buffer %s modified."
                   (buffer-name))
           '((?s "Save and kill buffer" "save the buffer and then kill it")
             (?d "Discard and kill buffer without saving" "kill buffer without saving")
             (?c "Cancel" "Exit without doing anything"))
           nil nil (and (not use-short-answers)
                        (not (use-dialog-box-p)))))))
    (cond ((= response ?s)
           (with-current-buffer buffer (save-buffer))
           t)
          ((= response ?d)
           t)
          ((= response ?c)
           nil)
          )))

(advice-add 'kill-buffer--possibly-save :around #'oo--prompt-in-less-confusing-way)
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
