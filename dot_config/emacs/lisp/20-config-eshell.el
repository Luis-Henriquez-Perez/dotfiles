;;; 20-config-eshell.el --- TODO: add commentary -*- lexical-binding: t; -*-
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

;;;; TODO: configure eshell prompt
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(opt! eshell-highlight-prompt nil)
(opt! eshell-prompt-function 'epe-theme-lambda)
;; (oo-text-abbrev "incs" "increase")
;; (oo-text-abbrev "decs" "deccrease")
;;;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;;;; run eat inside of eshell
;; For `eat-eshell-mode'.
;; (oo-add-hook 'eshell-load-hook #'eat-eshell-mode)
;;;;
;;;; aliases
;; https://olddeuteronomy.github.io/post/eshell-aliases-and-prompt/
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#aliases
;; (eshell/alias "e" "find-file $1")
;; For now do this, but I just really want to scroll up.  I do not want to
;; actually delete the buffer contents.  I mean I guess its O.K. since the
;; contents should be saved in eshell-history, but its more secure to actuall
;; have the physical buffer contents.
(eshell/alias "clear" "eshell/clear t")
(eshell/alias "ff" "find-file $1")
(eshell/alias "ffow" "find-file-other-window $1")
(eshell/alias "ffow" "find-file-other-window $1")
(eshell/alias "open" "find-file $1")
(eshell/alias "d" "dired $1")
;; https://howardism.org/Technical/Emacs/eshell-why.html
(eshell/alias "less" "view-file $1")
;; https://stackoverflow.com/questions/10566532/how-can-bash-execute-a-command-in-a-different-directory-context
;; TODO: Allow arguments to commands.  I ommited them for the sake o.
(eshell/alias "emacs-test" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "etest" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "emacs-compile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "ecompile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "emacs-eval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "eeval" "{cd $user-emacs-directory; eldev -d eval $1}")
;; (eshell/alias "update-wallpaper" "")
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
(provide '20-config-eshell)
;;; 20-config-eshell.el ends here
