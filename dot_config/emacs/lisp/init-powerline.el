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
(setq-default mode-line-format '(:eval (oo-main-modeline)))
(opt! powerline-height 30)
(oo-update-modeline)
(opt! powerline-default-separator 'arrow)

(defun +powerline--evil-state-segment ()
  (when (bound-and-true-p evil-mode)
    (string-join (list (powerline-raw (symbol-name evil-state) face0)
                       (funcall separator-left face0 face1))
                 "\s")))

(defun oo--buffer-info-segment ()
  (cond (buffer-read-only
         (all-the-icons-material "lock" :face face3))
        ((buffer-modified-p)
         (require 'all-the-icons)
         (concat (all-the-icons-material "save" :face face3)
                 (powerline-raw " " face0)))))

(defun +powerline-left-separator ()
  (intern (format "powerline-%s-%s" (powerline-current-separator) (car powerline-default-separator-dir))))

(defun +powerline-right-separator ()
  (intern (format "powerline-%s-%s" (powerline-current-separator) (cdr powerline-default-separator-dir))))

(defun! oo-main-modeline ()
  ""
  (set! active (powerline-selected-window-active))
  (set! mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
  (set! mode-line (if active 'mode-line 'mode-line-inactive))
  (set! face0 (if active 'powerline-active0 'powerline-inactive0))
  (set! face1 (if active 'powerline-active1 'powerline-inactive1))
  (set! face2 (if active 'powerline-active2 'powerline-inactive2))
  (set! separator-left (+powerline-left-separator))
  (set! separator-right (+powerline-right-separator))
  (set! face3 (list t :foreground (face-attribute 'error :foreground) :background (face-attribute face0 :background)))
  ;; (set! face3 (list t :foreground 'warning :background (face-attribute face0 :background)))
  (set! lhs (list
             (+powerline--evil-state-segment)
             (+powerline--buffer-info-segment)
             (powerline-raw (buffer-name) face0)
             ;; (powerline-raw (oo--buffer-info) face0)
             ;; (powerline-buffer-id `(mode-line-buffer-id ,face0) nil)
             ;; (powerline-raw " " face0)
             (funcall separator-left face0 face1)
             ;; (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
             ;;   (powerline-raw erc-modified-channels-object face1 'l))
             (powerline-major-mode face1 'l)
             ;; (powerline-process face1)
             ;; (powerline-minor-modes face1 'l)
             (powerline-narrow face1 'l)
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (powerline-vc face2 'r)
             ;; (when (bound-and-true-p nyan-mode)
             ;;   (powerline-raw (list (nyan-create)) face2 'l))
             ))
  (set! rhs (list (powerline-raw global-mode-string face2 'r)
                  (funcall separator-right face2 face1)
                  (unless window-system
                    (powerline-raw (char-to-string #xe0a1) face1 'l))
                  (powerline-raw "%4l" face1 'l)
                  ;; (powerline-raw ":" face1 'l)
                  (powerline-raw "%3c" face1 'r)
                  (funcall separator-right face1 face0)
                  (powerline-raw " " face0)
                  (powerline-raw "%6p" face0 'r)
                  (when powerline-display-hud
                    (powerline-hud face0 face2))
                  (powerline-fill face0 0)))
  (concat (powerline-render lhs)
          (powerline-fill face2 (powerline-width rhs))
          ;; (powerline-render rhs)
          ))
;; (defun oo--dired-modeline ()
;;   ""
;;   )

;; (defun oo--eshell-modeline ()
;;   ""
;;   )
;;; provide
(provide 'init-powerline)
;;; init-powerline.el ends here
