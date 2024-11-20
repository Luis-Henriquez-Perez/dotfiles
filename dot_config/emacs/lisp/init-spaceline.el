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
(require 'powerline)
(require 'spaceline)
(require 'spaceline-segments)
(require 'all-the-icons-nerd-fonts)
;; =spaceline-highlight-face-func= to =spaceline-highlight-face-evil-state
;; (oo-update-modeline)
(setq powerline-height 33)

(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))


(spaceline-define-segment kbd-macro+
  "Buffer read-only."
  (or (and defining-kbd-macro
           (all-the-icons-nerd-cod "record" :face 'error :v-adjust 0))
      (and executing-kbd-macro
           (all-the-icons-faicon "play" :face 'error)
           ;; (format "EXECUTING KBD MACRO...")
           )))

(spaceline-define-segment narrow+
  "Buffer read-only."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (all-the-icons-material "unfold_less" :face 'warning)))

(spaceline-define-segment buffer-read-only+
  "Buffer read-only."
  (when buffer-read-only
    (if (not (and (display-graphic-p) (require 'all-the-icons)))
        "X"
      (all-the-icons-material "lock" :face 'error))))

(spaceline-define-segment buffer-modified+
  "Buffer modified"
  (when (buffer-modified-p)
    (require 'all-the-icons)
    (all-the-icons-material "save" :face 'error)))

(spaceline-define-segment pomodoro+
  "Buffer modified"
  (block!
    (when (and (bound-and-true-p pomodoro-mode-line-string)
               (not (string-empty-p pomodoro-mode-line-string)))
      (require 'all-the-icons-nerd-fonts)
      (string-match (rx (group letter) (group digit digit ":" digit digit)) pomodoro-mode-line-string)
      (set! type (match-string 1 pomodoro-mode-line-string))
      (set! time (match-string 2 pomodoro-mode-line-string))
      (string-join (list
                    ;; (all-the-icons-nerd-pom "pomodoro-ticking" :face 'error :v-adjust 0)
                    (pcase type
                      ("w" (all-the-icons-nerd-pom "pomodoro-ticking" :face 'powerline-active0 :v-adjust 0))
                      ("b" (all-the-icons-nerd-cod "coffee" :face 'powerline-active0 :v-adjust 0)))
                    time)
                   "\s"))))

(spaceline-define-segment version-control+
  ""
  (when buffer-file-name
    (require 'vc)
    (aand (vc-backend buffer-file-name)
          (substring vc-mode (+ (if (eq it 'Hg) 2 3) 2))
          (format "%s %s" (all-the-icons-octicon "git-branch" :face 'powerline-active0 :v-adjust 0) (string-trim it)))))

(spaceline-define-segment evil-state+
  ""
  (when (bound-and-true-p evil-mode)

    (symbol-name evil-state)))

(spaceline-define-segment current-time+
  (format-time-string "%m-%d %H:%M"))

(spaceline-compile
  'main
  '((evil-state+ :face (intern (format "telephone-line-evil-%s" evil-state)))
    ((narrow+ kbd-macro+ buffer-read-only+ buffer-modified+ buffer-id remote-host) :priority 98)
    (version-control+ :face 'powerline-active0))
  '((pomodoro+ :face 'powerline-active0) major-mode (current-time+ :face (intern (format "telephone-line-evil-%s" evil-state)))))
;;; provide
(provide 'init-spaceline)
;;; init-spaceline.el ends here
