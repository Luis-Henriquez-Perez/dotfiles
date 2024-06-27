;;; 70-evil-org-headline-state.el --- evil state for org headlines -*- lexical-binding: t; -*-
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
;; Here I define an evil state for navigating and performing org mode
;; operations.
;;
;; Editing org is different enough from editing plain text that it warrants
;; having its own state.  The truth is that evil editing commands in normal
;; state simply do not make sense are are not as useful in org when we deal with
;; headlines primarily.
;;
;; This is still in the early stages, but it is certainly better than nothing.
;;
;;; Code:
;;;; requirements
(require 'evil)
(require 'org)
;;;; define org-headline-state
(evil-define-state org-headline
  "Org state for navigating and manipulating org headlines."
  :tag " <H> "
  :message "-- org headline --"
  :cursor ((box "blue1"))
  :states evil
  :enable (normal)
  :suppress-keymap t
  ;; If the state is being enabled, it should be when org mode is already
  ;; enabled.
  (when (equal evil-state 'org-headline)
    (cl-assert (derived-mode-p 'org-mode))))

(setq evil-org-headline-state-cursor '(box "blue1"))
(evil-set-initial-state 'org-mode 'org-headline)
;; Make sure `oo-override-mode-map' takes precedence.
(evil-make-intercept-map oo-override-mode-map 'org t)
;;;; utility functions
;; I ignore the feature where if your in the middle of the line it will split it.
;; That is wierd I do not want that.
(defun oo-org-insert-heading-before-current (&optional arg)
  "Insert heading before current."
  (interactive "P")
  (goto-char (line-beginning-position))
  (org-insert-heading arg nil nil))

(defun! oo-org-goto-headline-beginning ()
  "Put point at the beginning of headline text."
  (interactive)
  (cl-assert (org-at-heading-p))
  (goto-char (line-beginning-position))
  (set! regexp "^\\(?1:\\*+\\)[[:blank:]]+\\(?2:.*\\)[	 ]*$")
  (aand (looking-at regexp)
        (match-beginning 2)
        (goto-char it)))

(defun! oo-org-next-visible-heading (arg)
  "Same as `org-next-visible-heading' but."
  (interactive "p")
  (set! start (point))
  (org-next-visible-heading arg)
  (if (org-at-heading-p)
      (oo-org-goto-headline-beginning)
    (goto-char start)))

(defun! oo-org-previous-visible-heading (arg)
  "Same as `oo-org-next-visible-heading'."
  (interactive "p")
  (set! start (point))
  (org-previous-visible-heading arg)
  (if (org-at-heading-p)
      (oo-org-goto-headline-beginning)
    (goto-char start)))

(defhook! org-insert-heading-hook&enter-insert-state ()
  "Enter insert state after inserting a headline."
  (and (bound-and-true-p evil-mode)
       (evil-insert-state t)))
;;;; keybindings
;; (bind! evil-org-headline-state-map "y" #'org-copy-subtree)
(bind! org-mode-map :ie "ESC" #'evil-org-headline-state)
(bind! org-mode-map :ie [escape] #'evil-org-headline-state)
;; I need a way to enter this state in normal state.
(bind! org-mode-map :n "R" #'evil-org-headline-state)

(bind! evil-org-headline-state-map "w" #'widen)
(bind! evil-org-headline-state-map "s" #'org-insert-subheading)
(bind! evil-org-headline-state-map "R" #'org-refile)
(bind! evil-org-headline-state-map "N" #'evil-normal-state)
(bind! evil-org-headline-state-map "n" #'org-narrow-to-subtree)
(bind! evil-org-headline-state-map "x" #'org-cut-subtree)
(bind! evil-org-headline-state-map "j" #'oo-org-next-visible-heading)
(bind! evil-org-headline-state-map "k" #'oo-org-previous-visible-heading)
(bind! evil-org-headline-state-map "h" #'org-do-promote)
(bind! evil-org-headline-state-map "l" #'org-do-demote)
(bind! evil-org-headline-state-map "t" #'org-todo)
;; (bind! evil-org-headline-state-map "L" #'evilem-motion-heading)
;; TODO: use good way of making `org-up-heading-safe' interactive.
;; (bind! evil-org-headline-state-map "H" #'org-up-heading-safe)
(bind! evil-org-headline-state-map "J" #'org-move-subtree-down)
(bind! evil-org-headline-state-map "K" #'org-move-subtree-up)
(bind! evil-org-headline-state-map "<" #'org-promote)
(bind! evil-org-headline-state-map ">" #'org-demote)
(bind! evil-org-headline-state-map "TAB" #'outline-toggle-children)
(bind! evil-org-headline-state-map "o" #'org-insert-heading-after-current)
(bind! evil-org-headline-state-map "O" #'oo-org-insert-heading-before-current)
(bind! evil-org-headline-state-map [escape] #'evil-normal-state)
;;; provide
(provide '70-evil-org-headline-state)
;;; 70-org-headline-state.el ends here
