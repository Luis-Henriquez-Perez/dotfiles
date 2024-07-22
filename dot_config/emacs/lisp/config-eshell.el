;;; config-eshell.el --- configuration for eshell -*- lexical-binding: t; -*-
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
;; This is my configuration for eshell.
;;
;;; Code:
(require 'eshell-z)
(require 'eshell-up)
;;;; open eshell at bottom
(oo-popup-at-bottom "\\*eshell")
;;;; hooks
(hook! eshell-mode-hook abbrev-mode)

(hook! eshell-mode-hook smartparens-mode)
;;;; prevent eshell from printing out messages on load
;; Eshell prints various messages about loading modules.  These messages
;; originate from the function [[][eshell-unload-all-modules]].  I would rather
;; not see these messages.
(oo-add-advice #'eshell-unload-all-modules :around #'oo-funcall-silently)
;; At first I thought the culprit was this function, but I was wrong.  The
;; printing comes from =eshell-mode=.  In any case, however, I silence it as
;; well.
(oo-add-advice #'eshell-mode :around #'oo-funcall-silently)
;;;; TODO: configure eshell prompt
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(opt! eshell-banner-message "")
(opt! eshell-highlight-prompt nil)
(opt! eshell-prompt-function 'epe-theme-lambda)
;;;; eshell history
(opt! eshell-hist-ignoredups t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;;;; enable eat
(hook! eshell-mode-hook eat-eshell-mode)
;;;; clear
;; Unexpectedly for me the eshell clear scrolled to the bottom.  As seen in a
;; stackoverflow answer as well as multiple blog posts, the solution is to use
;; "clear 1" instead, essentually telling emacs to use "clear-scrollback".  I
;; still do not like this though because it actually erases the contents of the
;; buffer and I do not want to do this unnecessarily.  I just want it to scroll up.
;; TODO: make into a snippet and/or abbrev
;; (message "current buffer %S" (buffer-name))
;; TODO: edit surrounding form so that it works in comments
;; (message "var %S" var)
;; (defun! eshell/scroll-to-top ()
;;   ;; The function `recenter' does not seem to work in the eshell buffer.  I do
;;   ;; not know why.
;;   ;; (call-interactively #'recenter-top-bottom)
;;   ;; (call-interactively #'recenter-top-bottom)
;;   ;; Need a function of the number of lines in the window for the following to
;;   ;; work.
;;   ;; (set! line-number (line-number-at-pos))
;;   ;; (goto-char (point-min))
;;   ;; (forward-line (1- line-number))
;;   )
;; (eshell/alias "clear" )
;;; provide
(provide 'config-eshell)
;;; config-eshell.el ends here
