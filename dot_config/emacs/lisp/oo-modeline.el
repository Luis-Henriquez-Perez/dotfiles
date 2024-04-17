;;; oo-modeline.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
;;;; macro to generate a modeline segment
(defmacro! defsegment! (name &rest body)
  "Define a segment for the modeline."
  (declare (indent defun))
  (set! fname (intern (format "oo-modeline-segment-%s" name)))
  `(progn
     (defvar-local ,fname '(:eval (,fname)))
     (put ',fname 'risky-local-variable t)
     (defun ,fname () (progn ,@body))))
;;;; buffer information
(defsegment! buffer-name ()
  (format "%s\s" (buffer-name)))
;;;; major-mode information
(defsegment! major-mode ()
  (format "%s\s" (symbol-name major-mode)))
;;;; kbd-macro information
(defsegment! kbd-macro ()
  (or (and defining-kbd-macro
           "DEFINING KBD MACRO...\s")
      (and executing-kbd-macro
           "EXECUTING KBD MACRO...\s")))
;;;; narrowing information
(defsegment! narrow ()
  (when (buffer-narrowed-p)
    "NARROWED\s"))
;;;; org timer (what I use as pomodoro)
(defsegment! pomodoro ()
  (when (and (featurep 'org))
    ;; (format-time-string "[]")
    ))
;;;; current-time
(defsegment! current-time ()
  (format "%s\s" (current-time-string)))
;;;; evil-state
(defsegment! evil-state ()
  (when (bound-and-true-p evil-mode)
    (format "<%s>\s" evil-state)))
;;;; modeline
(progn
  (setq-default mode-line-format
                '("%e"
                  ;; oo-modeline-pomodoro-timer
                  oo-modeline-segment-buffer-name
                  oo-modeline-segment-major-mode
                  oo-modeline-segment-evil-state
                  oo-modeline-segment-narrow
                  oo-modeline-segment-kbd-macro
                  ;; oo-modeline-segment-current-time
                  ))
  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update))
;;; provide
(provide 'oo-modeline)
;;; oo-modeline.el ends here
