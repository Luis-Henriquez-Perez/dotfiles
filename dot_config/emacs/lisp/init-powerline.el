;;; init-powerline.el --- Initialize powerline -*- lexical-binding: t; -*-
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
;; Initialize powerline.
;;
;; Powerline is not actively maintained and--at least based on what I have
;; read--I do not see it being the most popular modeline.  What I liked about it
;; was that it does not try to be a modeline but rather a library for creating
;; your own custom modeline.  It is more low level.  For someone like me who
;; wants to heavily customize the modeline.
;;
;;; Code:
(require 'powerline)

(defun! oo--buffer-info ()
  ""
  (set! buffer-name (buffer-name))
  (pushing! segment buffer-name)
  (cond (buffer-read-only
         (set! icon (cond ((not (and (display-graphic-p) (require 'all-the-icons)))
                           "X")
                          (t
                           (all-the-icons-material "lock" :face 'error))))
         (pushing! segment icon))
        ((buffer-modified-p)

         (require 'all-the-icons)
         (set! icon (all-the-icons-material "save" :face 'error))
         (pushing! segment icon)))
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (pushing! segment (all-the-icons-material "unfold_less" :face 'warning)))
  (string-join segment "\s"))

;; (when (or (and defining-kbd-macro
;;                (require 'all-the-icons-nerd-fonts)
;;                (pushing! segment (all-the-icons-nerd-md "record-circle"))
;;                ;; (pushing! segment (all-the-icons-nerd-cod "record"))
;;                (format "DEFINING KBD MACRO..."))
;;           (and executing-kbd-macro
;;                (format "EXECUTING KBD MACRO..."))))

;; I specifically set the modeline to this because I want the property that when
;; I change the modeline function, my modeline is automatically changed.
;; Probably this is less effect than trying to make it ino a raw string but so
;; be it.
(progn (opt! powerline-height 33)
       (opt! powerline-default-separator 'arrow)
       (setq-default mode-line-format '(:eval (oo-main-modeline)))
       (oo-update-modeline)
       (powerline-reset))

(defun +powerline-left-separator ()
  (intern (format "powerline-%s-%s" (powerline-current-separator) (car powerline-default-separator-dir))))

(defun +powerline-right-separator ()
  (intern (format "powerline-%s-%s" (powerline-current-separator) (cdr powerline-default-separator-dir))))

(defun! oo-main-modeline ()
  (set! active (powerline-selected-window-active))
  (set! mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
  (set! mode-line (if active 'mode-line 'mode-line-inactive))
  ;; (set! face0 (if active 'mode-line 'mode-line-inactive))
  ;; (set! face1 (if active 'powerline-active0 'powerline-inactive0))
  (set! evil-face (intern (format "telephone-line-evil-%s" evil-state)))
  ;; (set! face1 (if active 'powerline-active0 'powerline-inactive0))
  (set! face1 (if active 'telephone-line-accent-active 'telephone-line-accent-inactive))
  (set! face0 (if active 'powerline-active1 'powerline-inactive1))
  (set! face2 (if active 'powerline-active2 'powerline-inactive2))
  (set! separator-left (+powerline-left-separator))
  (set! separator-right (+powerline-right-separator))
  (set! face3 (list t :foreground (face-attribute 'error :foreground) :background (face-attribute face0 :background)))
  ;; (set! face3 (list t :foreground 'warning :background (face-attribute face0 :background)))
  (set! lhs (append (when (bound-and-true-p evil-mode)
                      (list (powerline-raw (symbol-name evil-state) evil-face 'l)
                            (powerline-raw " " evil-face)
                            (funcall separator-left evil-face face0)))
                    (awhen! (and buffer-file-name
                                (require 'vc)
                                (vc-backend buffer-file-name))
                      (list (powerline-raw (string-trim (substring vc-mode (+ (if (eq it 'Hg) 2 3) 2))) face0 'l)
                            (funcall separator-left face0 face1)))
                    (list (when buffer-read-only
                            (cond ((and (display-graphic-p) (require 'all-the-icons))
                                   (powerline-raw (all-the-icons-material "lock" :face face1) face1 'l))
                                  (t
                                   "X")))
                          (when (buffer-modified-p)
                            (require 'all-the-icons)
                            (powerline-raw (all-the-icons-material "save" :face face1) face1 'l))
                          (when (or (buffer-narrowed-p)
                                    (and (bound-and-true-p fancy-narrow-mode)
                                         (fancy-narrow-active-p))
                                    (bound-and-true-p dired-narrow-mode))
                            (powerline-raw (all-the-icons-material "unfold_less" :face face1) face1 'l))
                          (powerline-raw (buffer-name) face1 'l)
                          ;; (powerline-raw (oo--buffer-info) face0)
                          ;; (powerline-buffer-id `(mode-line-buffer-id ,face0) nil)
                          ;; (powerline-raw " " face0)
                          (funcall separator-left face1 face2))
                    ;; (list nil
                    ;;       ;; (+powerline--buffer-info-segment)
                    ;;       ;; (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                    ;;       ;;   (powerline-raw erc-modified-channels-object face1 'l))
                    ;;       ;; (powerline-process face1)
                    ;;       ;; (powerline-minor-modes face1 'l)
                    ;;       (powerline-narrow face1 'l)
                    ;;       (powerline-raw " " face1)
                    ;;       (funcall separator-left face1 face0)
                    ;;       (powerline-vc 'org-level-4 'r)
                    ;;       (funcall separator-left face1 face0))
                    ))
  (set! rhs (append nil
                    (when (and (bound-and-true-p pomodoro-mode-line-string) (not (string-empty-p pomodoro-mode-line-string)))
                      (require 'all-the-icons-nerd-fonts)
                      (string-match (rx (group letter) (group digit digit ":" digit digit)) pomodoro-mode-line-string)
                      (set! type (match-string 1 pomodoro-mode-line-string))
                      (set! time (match-string 2 pomodoro-mode-line-string))
                      (list (funcall separator-right face1 face2)
                            (pcase type
                              ("w" (powerline-raw (all-the-icons-nerd-pom "pomodoro-ticking" :face face2 :v-adjust 0) face2 'r))
                              ("b" (powerline-raw (all-the-icons-nerd-cod "coffee" :face face2 :v-adjust 0) face2 'r)))
                            (powerline-raw time face2 'r)))
                    (list (funcall separator-right face0 face1)
                          (powerline-major-mode face1 'r))
                    (list (funcall separator-right face1 evil-face)
                          (powerline-raw (format-time-string "%m-%d %H:%M") evil-face 'r))
                    ;; (unless window-system
                    ;;   (powerline-raw (char-to-string #xe0a1) face1 'l))
                    ;; (powerline-raw "%4l " face1 'l)
                    ;; (powerline-raw ":" face1 'l)
                    ;; (powerline-raw "%3c" face1 'r)
                    ;; (funcall separator-right face1 evil-face)
                    ;; (powerline-raw " " evil-face)
                    ;; (powerline-raw "foo" evil-face 'r)
                    ;; (when powerline-display-hud
                    ;;   (powerline-hud face0 face2))
                    ))
  (set! left (powerline-render lhs))
  (set! right (powerline-render rhs))
  ;; (set! n (- (window-width) (length left) (length right) 6))
  (concat left
          ;; (make-string n ? )
          (powerline-fill face2 (powerline-width rhs))
          right))
;;; provide
(provide 'init-powerline)
;;; init-powerline.el ends here
