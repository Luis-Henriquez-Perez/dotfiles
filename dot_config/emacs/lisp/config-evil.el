;;; config-evil.el --- evil configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for evil.
;;
;;; Code:
(require 'evil)
;;;; settings
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)

(opt! evil-visualstar/persistent t)
;;;; minibuffer
(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! preserve-prior-evil-state (minibuffer-setup-hook)
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! minibuffer-exit-hook (restore-prior-evil-state)
  "Restore state after minibuffer."
  ;; :on evil-mode
  (when (bound-and-true-p evil-mode)
    (evil-change-state oo-evil-state-before-minibuffer)
    (setq oo-evil-state-before-minibuffer nil)))

(defadvice! refresh-cursor (around load-theme fn &rest args)
  (when (bound-and-true-p evil-mode)
	(evil-refresh-cursor)))
;;;; escape
(defvar oo-escape-hook nil
  "Hook run after escaping.")

(defun @exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((run-hook-with-args-until-success 'oo-escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        (t (keyboard-quit))))

(oo-add-advice #'evil-force-normal-state :after #'@exit-everything)
;;;; eval operator
;; This is shamelessly copied from `evil-extra-operator'.
(evil-define-operator +evil-eval-operator (beg end)
  "Evil operator for evaluating code."
  :move-point nil
  (interactive "<r>")
  (eval-region beg end t))

;; This is also shamelessly copied with the difference that the format string is
;; "%S" instead of "%s".  Honestly, I think not having it that way was a bug.
(evil-define-operator +evil-eval-replace-operator (beg end)
  "Evil operator for replacing contents with result from eval."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "%S" (eval (read text)))))
    (delete-region beg end)
    (insert result)))

(evil-define-operator +evil-eval-print-operator (beg end)
  "Evil operator for printing the results of contents below."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "\n=> %S" (eval (read text)))))
    (goto-char end)
    (alet (point)
      (insert result)
      (comment-region it (point)))))
;;; provide
(provide 'config-evil)
;;; config-evil.el ends here
