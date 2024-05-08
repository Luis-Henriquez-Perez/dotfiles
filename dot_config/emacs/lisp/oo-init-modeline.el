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
;; (defvar oo-line-spinner nil)
;; (setq oo-line-spinner (spinner-create 'rotating-line))
;; (spinner-start oo-line-spinner)
;; (spinner-stop oo-line-spinner)
;;;; macro to generate a modeline segment
(defmacro! defsegment! (name &rest body)
  "Define a segment for the modeline."
  (declare (indent defun))
  (set! fname (intern (format "oo-mode-line-segment-%s" name)))
  `(progn
     (defvar-local ,fname '(:eval (,fname)))
     (put ',fname 'risky-local-variable t)
     (defun! ,fname () (with-demoted-errors ,(format "error in %s: %%S" fname) ,@body))))
;;;; buffer information
(defsegment! buffer-name ()
  (buffer-name))
;;;; major-mode information
(defsegment! major-mode ()
  (format "[%s]" (capitalize (symbol-name major-mode))))
;;;; kbd-macro information
(defsegment! kbd-macro ()
  (or (and defining-kbd-macro
           (format "DEFINING KBD MACRO..."))
      (and executing-kbd-macro
           (format "EXECUTING KBD MACRO..."))))
;;;; narrowing information
(defsegment! narrow ()
  (when (buffer-narrowed-p)
    "NARROWED"))
;;;; org timer (what I use as pomodoro)
(defsegment! org-timer ()
  (when (bound-and-true-p org-timer-countdown-timer)
    ;; TODO set the face depending on the timer based on the percentage of the
    ;; time done.  And consider whether I chould do this with an advice or in
    ;; this function.
    (string-trim org-timer-mode-line-string)))

;; Color org-timer segment based on percentage of timer done.
;; (defun oo-mode-line-org-timer-face (string)
;;   "Return STRING propertized properly."
;;   (set! percentage)
;;   (cond ((<)
;;          (propertize string 'face 'success))
;;         (())
;;         (t
;;          (propertize string 'face 'failure))))
;;;; current-time
;; TODO: how to display somet
(defsegment! current-time ()
  (format-time-string "%m-%d %H:%M"))
;;;; evil-state
(defsegment! evil-state ()
  (when (bound-and-true-p evil-mode)
    (format "%s" (string-trim evil-mode-line-tag))))
;;;; battery
(defsegment! battery ()
  (require 'battery)
  (set! status (funcall battery-status-function))
  (set! percentage (thread-last (battery-format "%p" status)
                                (string-to-number)
                                (round)))
  (set! discharging-p (equal "discharging" (battery-format "%B" status)))
  (when (and discharging-p (< percentage 60))
    (format "%s%%%" percentage)))
;;;; emms
;; TODO: Add how much time is left plaing...
(defsegment! emms ()
  (when (bound-and-true-p emms-player-playing-p)
    (require 'f)
    (set! track (f-filename (emms-track-description (emms-playlist-current-selected-track))))
    (cond ((bound-and-true-p emms-player-paused-p)
           (format "PAUSED %s" track))
          ((bound-and-true-p emms-repeat-track)
           (format "REPEATING %s" track))
          (t
           (format "PLAYING %s" track)))))
;;;; version control information
(defsegment! vc-branch ()
  (when buffer-file-name
    (require 'vc)
    (aand (vc-backend buffer-file-name)
          (substring vc-mode (+ (if (eq it 'Hg) 2 3) 2))
          (string-trim it))))
;;;; default directory
(defsegment! directory ()
  (abbreviate-file-name default-directory))
;;;; read-only
(defsegment! read-only ()
  (when buffer-read-only
    "READ-ONLY"))
;;;; modeline
;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline

;; The modeline gets rendered twice with this.  Once in `oo-mode-line-render'
;; and another time when Emacs actually renders the modeline.  Maybe then this
;; should just return the resulting string to be more performant.  It is such a
;; waste to render the left and right side of the modeline just to get the
;; length and then leave it to be done internally again.
                                        ; (defun! oo-mode-line-render (left right)
;;   "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively."
;;   (set! lt (format-mode-line left))
;;   (set! rt (format-mode-line right))
;;   (set! width (- (window-total-width) (+ (length lt) (length rt))))
;;   (append left (list (format (format "%%%ds" width) "")) right))

;; https://emacs.stackexchange.com/questions/46846/timer-on-modeline-how-to-update-modeline-every-second
;; Interesting time as percentage of the day.
;; (format "%.1f%%"
;;         (* 100 (/ (+ (* (string-to-number 24-hours) 3600)
;;                      (* (string-to-number minutes) 60)
;;                      (string-to-number seconds))
;;                   86400.0)))
;; https://emacs.stackexchange.com/questions/16442/padding-around-modeline-text
;; This adds a box around the modeline.
;; This needs to be set every...
(set-face-attribute 'mode-line nil :box '(:line-width 3))

;; From chatgpt.
;; (defun oo-percentage-of-day-completed ()
;;   "Calculate the percentage of the day completed based on the current time."
;;   (let* ((current-time (decode-time (current-time)))
;;          (hour (nth 2 current-time))
;;          (minute (nth 1 current-time))
;;          (seconds-in-day (* 24 60 60))
;;          (current-seconds (+ (* hour 3600) (* minute 60))))
;;     (/ (* 100.0 current-seconds) seconds-in-day)))

(defun! oo-mode-line-render (left right)
  "Return the rendered modeline."
  (set! lt (string-join (-remove #'string-empty-p (mapcar #'format-mode-line left)) "\s"))
  (set! rt (string-join (-remove #'string-empty-p (mapcar #'format-mode-line right)) "\s"))
  (set! width (- (window-total-width) (+ (length lt) (length rt)) 2) 0)
  (concat "\s" lt (make-string (max width 0) 32) rt "\s"))

;; Update every minute.
(defvar oo-mode-line-update-timer nil)
(setq oo-mode-line-update-timer (run-with-timer 30 1 #'force-mode-line-update))

(setq-default mode-line-format
              '((:eval
                 (oo-mode-line-render
                  '("%e"
                    oo-mode-line-segment-buffer-name
                    oo-mode-line-segment-major-mode
                    ;; To be honest not sure if I need evil-state.
                    ;; oo-mode-line-segment-evil-state
                    oo-mode-line-segment-directory
                    oo-mode-line-segment-narrow
                    oo-mode-line-segment-kbd-macro
                    oo-mode-line-segment-emms)
                  '(oo-mode-line-segment-vc-branch
                    oo-mode-line-segment-org-timer
                    oo-mode-line-segment-battery
                    oo-mode-line-segment-current-time)))))

;; TODO: Maybe do this lazily.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (setq mode-line-format (default-value 'mode-line-format))))
;;; provide
(provide 'oo-init-modeline)
;;; oo-init-modeline.el ends here
