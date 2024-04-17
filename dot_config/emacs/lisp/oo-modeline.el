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
;;;; buffer information
(defsegment! buffer-name ()
  :face '((t :background "gray50" :foreground "black"))
  (buffer-name))
;;;;  major-mode information
(defsegment! major-mode)
(defvar-local oo-mode-line-major-mode-info
    '(:eval (propertize (symbol-name major-mode) 'face 'error)))

(setq-local oo-mode-line-major-mode-info
            '(:eval (propertize (capitalize (symbol-name major-mode)) 'face 'oo-mode-line-major-mode-info-face)))

(put 'oo-mode-line-mode-info 'risky-local-variable t)

(defface oo-mode-line-major-mode-info-face '((t :background "red" :foreground "pink"))
  "Face with background for modeline.")

(custom-set-faces 'oo-mode-line-buffer-info-face
                  '(t :background "gray50" :foreground "white"))

(set-face-attribute 'oo-mode-line-buffer-info-face nil :background "gray50")

(set-face-attribute 'oo-mode-line-major-mode-info-face nil :background "black")
;;;; kbd-macro information
(defvar-local oo-mode-line-kbd-macro-info
    '(:eval (when defining-kbd-macro )))

(put 'oo-mode-line-mode-info 'risky-local-variable t)

;; I am not sure if there is a benefit to.
;; well, the benefit would be that I do not have to reset the variable with
;; `setq-local'.  Instead I can simply change the body of the function and
;; re-evaluate it.
(defun oo-modeline-keyboard-macro ()
  "Return the string."
  (when defining-kbd-macro
    "DEFINING KBD MACRO..."))

(defsegment! kbd-macro ()
  ;; :face oo-face-face
  (when defining-kbd-macro
    "DEFINING KBD MACRO..."))
;;;; narrowing information
(defvar-local oo-mode-line-narrowing-info
    '(:eval (when defining-kbd-macro )))
;;;; modeline
(progn
  (setq-default mode-line-format
                '("%e"
                  oo-mode-line-kbd-macro-info
                  oo-mode-line-narrowing-info
                  oo-mode-line-pomodoro-timer
                  oo-mode-line-evil-state-info
                  oo-mode-line-buffer-info
                  oo-mode-line-mode-info))
  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update))
;;;; macro to generate a modeline segment
(defmacro! defsegment! (name &rest body)
  "Define a segment for the modeline."
  (declare (indent defun))
  ;; (set! face )
  (set! fname (intern (format "oo-modeline-segment-%s" name)))
  ;; Allow setting the face.
  ;; (set! var (intern (format "oo-modeline-")))
  `(progn
     ;; (defface ,face ,face-definition)
     (defvar-local ,fname '(:eval (,fname)))
     (put ',fname 'risky-local-variable t)
     (defun ,fname () (progn ,@body))))
;;; provide
(provide 'oo-modeline)
;;; oo-modeline.el ends here
