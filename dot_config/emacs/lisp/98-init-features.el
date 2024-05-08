;;; 98-init-features.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is initialization code all of the packages I use.  I have vacillated
;; between having them as separate files.  But to be honest I do not think
;; there's a point for doing so, particularly because many of these
;; configurations do not affect.
;;
;;; Code:
(require 'on)
;;;; core
;; These are fundamental, essential.  They should be moved into a more core
;; file.
;;;;; disable old themes before enabling new ones
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
;;;; feature-specific customization
;;;;; re-builder
;; By default, use `rx' syntax.  It is my preferred syntax.
(opt! reb-re-syntax 'rx)
;;;;; unknown
;; I like an indentation of 4 spaces; maybe I have gotten used to it with Python.
(opt! sgml-basic-offset 4)
;;;;; dabbrev
(opt! dabbrev-check-all-buffers nil)
;;;;; frame
;; TODO: Figure out how to set this based on the color of the theme.
(defhook! after-init-hook&set-window-divider-face ()
  "Set the window divider face."
  [:depth 11]
  (set-face-foreground 'window-divider "black"))

(defadvice! load-theme@AFset-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

;; (add-hook 'emacs-startup-hook
;;           (lambda (&rest _) (oo-add-advice #'load-theme :after #'oo-set-window-divider-face)))

;; You can either use =right-only= to place window dividers on the right of each
;; window.  Or =bottom-only= to place them just on the bottom.
(opt! window-divider-default-places t)

;; The default value is 6.
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-bottom-width 7)
;;;; custom functions
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (set! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;; provide
(provide '98-init-features)
;;; 98-init-features.el ends here
