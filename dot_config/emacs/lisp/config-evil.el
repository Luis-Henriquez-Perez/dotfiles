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

(defhook! oo-preserve-prior-evil-state-h (minibuffer-setup-hook)
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! oo-restore-prior-evil-state-h (minibuffer-exit-hook)
  "Restore state after minibuffer."
  (when (bound-and-true-p evil-mode)
    (when oo-evil-state-before-minibuffer
      (evil-change-state oo-evil-state-before-minibuffer))
    (setq oo-evil-state-before-minibuffer nil)))

(defun! oo--refresh-cursor (orig-fn &rest args)
  (when (bound-and-true-p evil-mode)
	(evil-refresh-cursor))
  (apply orig-fn args))

(advice-add 'load-theme :around 'oo--refresh-cursor)
;;;; escape
(defun! oo--exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
		 (if (or defining-kbd-macro executing-kbd-macro)
			 (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
		((or defining-kbd-macro executing-kbd-macro) nil)
        (t
		 (keyboard-quit))))

(advice-add #'evil-force-normal-state :after #'oo--exit-everything)
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
;;;; insert state hook
(defhook! +evil-enter-insert-state-h ()
  "Enter insert state if `evil-mode' is enabled."
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))
;;;; Evil cursor color support
;; Did not realize for the longest time that evil cursor can be a function that
;; changes the cursor.  With this in mind, the best way to set the cursor size
;; and shape dynamically is to set the corresponding cursor symbols to functions.

(defun! +evil-normal-state-cursor ()
  (evil-set-cursor t)
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-normal :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-normal)
         (set! bg (face-attribute 'spaceline-evil-normal :background nil t))
         (evil-set-cursor-color bg))))

(defun! +evil-insert-state-cursor ()
  (evil-set-cursor '(bar . 2))
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-insert :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-insert)
         (set! bg (face-attribute 'spaceline-evil-insert :background nil t))
         (evil-set-cursor-color bg))))

(defun! +evil-visual-state-cursor ()
  (evil-set-cursor t)
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-visual :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-visual)
         (set! bg (face-attribute 'spaceline-evil-visual :background nil t))
         (evil-set-cursor-color bg))))

(defun! +evil-motion-state-cursor ()
  (evil-set-cursor t)
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-motion :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-motion)
         (set! bg (face-attribute 'spaceline-evil-motion :background nil t))
         (evil-set-cursor-color bg))))

(defun! +evil-replace-state-cursor ()
  (evil-set-cursor t)
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-replace :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-replace)
         (set! bg (face-attribute 'spaceline-evil-replace :background nil t))
         (evil-set-cursor-color bg))))

(defun! +evil-operator-state-cursor ()
  (evil-set-cursor '(hbar . 9))
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-operator :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-operator)
         (set! bg (face-attribute 'spaceline-evil-operator :background nil t))
         (evil-set-cursor-color bg))))

(defun! +evil-emacs-state-cursor ()
  (evil-set-cursor t)
  (cond ((bound-and-true-p telephone-line-mode)
         (set! bg (face-attribute 'telephone-line-evil-emacs :background nil t))
         (evil-set-cursor-color bg))
        ((facep 'spaceline-evil-emacs)
         (set! bg (face-attribute 'spaceline-evil-emacs :background nil t))
         (evil-set-cursor-color bg))))

(opt! evil-normal-state-cursor   #'+evil-normal-state-cursor)
(opt! evil-insert-state-cursor   #'+evil-insert-state-cursor)
(opt! evil-visual-state-cursor   #'+evil-visual-state-cursor)
(opt! evil-motion-state-cursor   #'+evil-motion-state-cursor)
(opt! evil-replace-state-cursor  #'+evil-replace-state-cursor)
(opt! evil-operator-state-cursor #'+evil-operator-state-cursor)
(opt! evil-emacs-state-cursor    #'+evil-emacs-state-cursor)
;;;; cross-configuration
;;;;; org-capture
(oo-add-hook 'org-capture-mode-hook #'+evil-enter-insert-state-h)
;;;;; git-commit
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(oo-add-hook 'git-commit-mode-hook #'+evil-enter-insert-state-h)
;;;;; denote
(oo-add-hook 'denote-after-new-note-hook #'+evil-enter-insert-state-h)
;;;;; corfu
;; When using evil, neither `corfu-map' nor `tempel-map' bindings will work
;; because the maps are overridden by evil.  In order for them to work, we need
;; to boost give the maps greater precedence.
(defafter! oo-make-corfu-map-an-overriding-map (corfu)
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))
;;;;; tempel
(defafter! oo-make-tempel-map-an-overriding-map (tempel)
  (evil-make-overriding-map tempel-map))
;;; provide
(provide 'config-evil)
;;; config-evil.el ends here
