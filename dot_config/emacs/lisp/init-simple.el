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
(require 'base)

;;;; hooks
(hook! prog-mode-hook auto-fill-mode)
(hook! text-mode-hook auto-fill-mode)
(hook! text-mode-hook visual-line-mode)

;;;; handle trailing whitespace
(setq-default show-trailing-whitespace nil)

(defhook! manage-trailing-whitespace (prog-mode-hook conf-mode)
  "Show trailing whitespace and delete it before saving."
  (setq show-trailing-whitespace t)
  (hook! before-save-hook delete-trailing-whitespace :local t))
;;;; less confusing kill buffer
;; https://christiantietze.de/posts/2023/09/kill-unsaved-buffer-ux-action-labels/
(defadvice! prompt-clearly (around kill-buffer--possibly-save _ buffer &rest args)
  "Ask user in the minibuffer whether to save before killing.
Replace `kill-buffer--possibly-save' as advice."
  (set! prompt (format "Buffer %s modified." (buffer-name)))
  (set! choices '((?s "Save and kill buffer" "save the buffer and then kill it")
                  (?d "Discard and kill buffer without saving" "kill buffer without saving")
                  (?c "Cancel" "Exit without doing anything")))
  (set! long-form (and (not use-short-answers) (not (use-dialog-box-p))))
  (set! response (car (read-multiple-choice prompt (reverse choices) nil nil long-form)))
  (cl-case response
    (?s (with-current-buffer buffer (save-buffer)) t)
    (?d t)
    (t nil)))
;;;; keybindings
(bind! oo-buffer-map "x" #'kill-current-buffer)
;;; provide
(provide 'init-simple)
;;; init-simple.el ends here
