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
(require 'spaceline)
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

(spaceline-define-segment +buffer-read-only
  "Buffer read-only."
  (when buffer-read-only
    (if (not (and (display-graphic-p) (require 'all-the-icons)))
        "X"
      (all-the-icons-material "lock" :face 'error))))

(spaceline-define-segment +buffer-modified
  "Buffer modified"
  (when (buffer-modified-p)
    (require 'all-the-icons)
    (all-the-icons-material "save" :face 'error)))

(spaceline-define-segment +pomodoro
  "Buffer modified"
  (when (buffer-modified-p)
    (require 'all-the-icons)
    (all-the-icons-material "save" :face 'error)))

(spaceline-compile
  'main
  '((evil-state :face (intern (format "telephone-line-evil-%s" evil-state)))
    ((+buffer-read-only +buffer-modified buffer-size buffer-id remote-host) :priority 98))
  '(major-mode
    buffer-position))
;;; provide
(provide 'init-spaceline)
;;; init-spaceline.el ends here
