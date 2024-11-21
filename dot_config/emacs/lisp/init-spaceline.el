;;; init-spaceline.el --- Initialize spaceline -*- lexical-binding: t; -*-
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
;; Initialize spaceline.
;;
;;; Code:
;;;; requirements
(require 'powerline)
(require 'spaceline)
(require 'spaceline-segments)
;; (require 'all-the-icons)
(require 'all-the-icons-nerd-fonts)
(require 'nerd-icons)
;;;; reset powerline after theme change
(defun! oo-reset-powerline-a (orig-fn &rest args)
  (prog1 (apply orig-fn args)
    (powerline-reset)))

(advice-add 'load-theme :around #'oo-reset-powerline-a)
;;;; set powerline height
(setq powerline-height 33)
;;;; define custom segments
(spaceline-define-segment my-kbd-macro
  "Display an icon to represent when."
  (or (and defining-kbd-macro
           (nerd-icons-codicon "nf-cod-record")
           ;; (all-the-icons-nerd-cod "record" :face 'error :v-adjust -0.1)
           )
      (and executing-kbd-macro
           (all-the-icons-faicon "play" :face 'error)
           ;; (format "EXECUTING KBD MACRO...")
           )))

(spaceline-define-segment my-narrow
  "Indicate when the current buffer is narrowed."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (if (and (display-graphic-p))
        (progn nil
               ;; (all-the-icons-material "unfold_less" :face 'warning)
               (nerd-icons-octicon "nf-oct-fold" :face 'warning))
      "><")))

(spaceline-define-segment my-buffer-read-only
  "Display"
  (when buffer-read-only
    (if (not (and (display-graphic-p) (require 'all-the-icons)))
        "X"
      (nerd-icons-faicon "nf-fa-lock")
      ;; (all-the-icons-material "lock" :face 'error)
      )))

(spaceline-define-segment my-buffer-modified
  "Buffer modified"
  (when (buffer-modified-p)
    ;; (require 'all-the-icons)
    (nerd-icons-faicon "nf-fa-save")
    ;; (all-the-icons-material "save" :face 'error)
    ))

(spaceline-define-segment my-pomodoro
  "Display left for pomodoro."
  (block!
    (when (and (bound-and-true-p pomodoro-mode-line-string)
               (not (string-empty-p pomodoro-mode-line-string)))
      (require 'all-the-icons-nerd-fonts)
      (string-match (rx (group letter) (group digit digit ":" digit digit)) pomodoro-mode-line-string)
      (set! type (match-string 1 pomodoro-mode-line-string))
      (set! time (match-string 2 pomodoro-mode-line-string))
      (string-join (list
                    (pcase type
                      ("w" (all-the-icons-nerd-pom "pomodoro-ticking" :face 'powerline-active0 :v-adjust 0))
                      ("b" (all-the-icons-nerd-cod "coffee" :face 'powerline-active0 :v-adjust 0)))
                    time)
                   "\s"))))

(spaceline-define-segment my-version-control
  "Display current git branch."
  (aand buffer-file-name
        (require 'vc)
        (vc-backend buffer-file-name)
        (substring vc-mode (+ (if (eq it 'Hg) 2 3) 2))
        (format "%s %s" (all-the-icons-octicon "git-branch" :face 'powerline-active0 :v-adjust 0) (string-trim it))))

(spaceline-define-segment my-evil-state
  "Display the current evil state if evil-mode is enabled."
  (when (bound-and-true-p evil-mode)
    (symbol-name evil-state)))

(spaceline-define-segment my-current-time
  "Display the current time."
  (format-time-string "%m-%d %H:%M"))
;;;; initialize modeline at startup
(defhook! oo-init-modeline-h (after-init-hook)
  (spaceline-compile
    'main
    '((my-evil-state :face (intern (format "spaceline-evil-%s" evil-state)))
      ((my-narrow my-kbd-macro my-buffer-read-only my-buffer-modified buffer-id remote-host) :priority 98)
      (my-version-control :face 'powerline-active0))
    '((my-pomodoro :face 'powerline-active0) major-mode (my-current-time :face (intern (format "spaceline-evil-%s" evil-state)))))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
;;; provide
(provide 'init-spaceline)
;;; init-spaceline.el ends here
