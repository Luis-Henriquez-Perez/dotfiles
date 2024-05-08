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
;; This is everything that needs to be evaluated immediately on startup.
;;
;;; Code:
(require 'oo-base)
(require 'oo-init-no-littering)
(require 'oo-init-abbrev)
(require 'oo-init-dashboard)
(require 'oo-init-recentf)
(require 'oo-init-hooks)
(require 'oo-init-keybindings)
;;;; initial buffer choice
(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
              (get-buffer-create "*scratch*"))
    (lgr-info oo-lgr "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)
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
;;;; minibuffer
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defhook! minibuffer-setup-hook&increase-garbage-collection ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  [:depth 10]
  (oo-record-value 'gc-cons-threshold)
  (oo-record-value 'gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(defhook! minibuffer-exit-hook&decrease-garbage-collection ()
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  [:depth 90]
  (oo-restore-value 'gc-cons-threshold)
  (oo-restore-value 'gc-cons-percentage))
;;;; set initial font
(progn!
  (aset! (or (font-spec :name "Martian Mono Nerd Font"
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
                        :size 15)))
  (set-face-attribute 'default nil :font it))
;;;; garbage collection
(defun! oo-lower-garbage-collection ()
  "Lower garbage collection until it reaches default values."
  ;; This is a sanity check to ensure.
  (cl-assert (zerop (% gc-cons-threshold (* 4 1024 1024))))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo-lower-garbage-collection)
    (cl-decf gc-cons-threshold (* 4 1024 1024))
    (cl-decf gc-cons-percentage 0.1)
    (cond ((= gc-cons-threshold (* 8 1024 1024))
           (setq gc-cons-percentage 0.4))
          (t
           (run-with-timer 5 nil #'oo-lower-garbage-collection)))))

(defhook! emacs-startup-hook&restore-startup-values ()
  [:depth 91]
  (oo-restore-value 'file-name-handler-alist)
  (setq gc-cons-threshold (* 32 1024 1024))
  (run-with-timer 5 nil #'oo-lower-garbage-collection)
  (require 'oo-init-modeline))
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
