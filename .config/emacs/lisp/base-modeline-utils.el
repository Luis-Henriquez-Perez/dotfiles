;;; base-modeline-utils.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Generic modeline segment functions to avoid code duplication when using
;; different modeline packages.
;;
;;; Code:
;;;; variables
(defvar oo-modeline-icons 'all-the-icons
  "Type of icons to use in the modeline.
Values can be `nerd-icons', `all-the-icons' and nil.")
(setq oo-modeline-icons 'all-the-icons)
(setq oo-modeline-icons 'nerd-icons)

(defmacro! defsegment! (&rest args)
  "Define a modeline segment."
  (declare (indent defun) (doc-string 3))
  (set! (name arglist metadata body) (oo--definer-components args))
  `(defun! ,name ,arglist
     ,@metadata
     (condition-case err
         (progn ,@body)
       (error
        (error! "Modeline segment %s raised a %s because of %s" ',name (car err) (cdr err))
        ""))))
;;;; utility functions
;; (defun oo-dwim-concatentate ()
;;   ""
;;   (concat (mapcar #')))
;;;; segments
(defsegment! oo-modeline-segment--buffer-name (&rest _)
  "Buffer name indicator for modeline."
  (pcase oo-modeline-icons
    ('nerd-icons
     (format "%s %s" (nerd-icons-icon-for-buffer) (buffer-name)))
    ('all-the-icons
     (format "%s %s" (all-the-icons-icon-for-buffer) (buffer-name)))
    (_
     (buffer-name))))

(defsegment! oo-modeline-segment--kbd-macro (&optional face)
  "Keybinding macro indicator."
  (or (and defining-kbd-macro
           (pcase oo-modeline-icons
             ('nerd-icons (nerd-icons-mdicon "nf-md-record_circle" :v-adjust -0.2))
             ('all-the-icons (all-the-icons-material "fiber_manual_record" :face 'error :v-adjust -0.2))
             (_ "•REC")))
      (and executing-kbd-macro
           (pcase oo-modeline-icons
             ('nerd-icons (nerd-icons-mdicon "nf-md-play" :face 'error))
             ('all-the-icons (all-the-icons-faicon "play" :face 'error))
             (_ "PLAYING")))))

(defsegment! oo-modeline-segment--version-control (&rest _)
  "Return the branch name as a modeline segment."
  (when (and (buffer-file-name)
             (locate-dominating-file (buffer-file-name) ".git"))
    (set! branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (pcase oo-modeline-icons
      ('nerd-icons
       (set! icon (nerd-icons-devicon "nf-dev-git_branch" :v-adjust -0.01))
       (format "%s %s" icon branch))
      ('all-the-icons
       (set! icon (all-the-icons-octicon "git-branch" (spaceline-face-func) :v-adjust -0.01))
       (format "%s %s" icon branch))
      (_
       branch))))

(defsegment! oo-modeline-segment--narrow (&optional _)
  "Return modeline indicator for narrowed buffer."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (pcase oo-modeline-icons
      ('all-the-icons
       (all-the-icons-material "unfold_less" :face 'warning))
      ('nerd-icons
       (nerd-icons-codicon "nf-cod-fold" :face 'warning))
      (_
       "><"))))

(defsegment! oo-modeline-segment--pomodoro (&optional face)
  "Pomodoro time left for work or break indicator."
  (defvar pomodoro-mode-line-string)
  (when (and (bound-and-true-p pomodoro-mode-line-string)
             (not (string-empty-p pomodoro-mode-line-string)))
    (string-match (rx (group letter) (group digit digit ":" digit digit)) pomodoro-mode-line-string)
    (set! type (match-string 1 pomodoro-mode-line-string))
    (set! time (match-string 2 pomodoro-mode-line-string))
    (pcase oo-modeline-icons
      ('all-the-icons
       (pcase type
         ("w" (set! icon (all-the-icons-nerd-pom "pomodoro-ticking" :face face :v-adjust 0)))
         ("b" (set! icon (all-the-icons-nerd-cod "coffee" :face face :v-adjust 0))))
       (format "%s %s" icons time))
      ('nerd-icons
       (pcase type
         ("w" (set! icon (nerd-icons-pomicon "pomodoro-ticking" :face face :v-adjust 0)))
         ("b" (set! icon (nerd-icons-codicon "coffee" :face face :v-adjust 0))))
       (format "%s %s" icon time))
      (_
       (format "%s %s" type time)))))

(defsegment! oo-modeline-segment--current-time (&optional face)
  "Display the current time."
  (format-time-string "%m-%d %H:%M"))

(defsegment! oo-modeline-segment--read-only (&optional face)
  (when buffer-read-only
    (pcase oo-modeline-icons
      ('all-the-icons
       (all-the-icons-material "lock" :face 'error))
      ('nerd-icons
       (set! icon (nerd-icons-faicon "nf-fa-lock" :face 'error))
       icon)
      (_
       "LOCKED"))))

(defsegment! oo-modeline-segment--buffer-modified (&optional face)
  "Buffer modified"
  (when (and (buffer-file-name) (buffer-modified-p))
    (pcase oo-modeline-icons
      ('all-the-icons
       (all-the-icons-material "save" :face 'error))
      ('nerd-icons
       (nerd-icons-faicon "nf-fa-save" :face 'error))
      (_
       "MODIFIED"))))

(defsegment! oo-modeline-segment--evil-state ()
  "Return modeline indication for evil state."
  (when (bound-and-true-p evil-mode)
    (symbol-name evil-state)))
;;; provide
(provide 'base-modeline-utils)
;;; base-modeline-utils.el ends here
