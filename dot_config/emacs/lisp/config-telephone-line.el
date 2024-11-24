;;; config-telephone-line.el --- Configure telephone-line -*- lexical-binding: t; -*-
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
;; Configure telephone-line.
;;
;;; Code:
;;;; custom segments
;;;;; major-mode information
(telephone-line-defsegment* +telephone-line-major-mode-segment ()
  ;; Do not show the ugly "\l" that indicates lexical binding.
  (alet (format-mode-line (funcall (telephone-line-major-mode-segment) face))
    (if (string-match "\\`ELisp" it)
        (substring it (match-beginning 0) (match-end 0))
      it)))
;;;;; kbd-macro information
(telephone-line-defsegment* +telephone-line-kbd-macro-segment ()
  (or (and defining-kbd-macro
           (format "DEFINING KBD MACRO..."))
      (and executing-kbd-macro
           (format "EXECUTING KBD MACRO..."))))
;;;;; narrowing information
(telephone-line-defsegment* +telephone-line-narrow-segment ()
  (when (buffer-narrowed-p)
    "NARROWED"))
;;;;; buffer
(telephone-line-defsegment* +telephone-line-buffer-segment ()
  (autolet!
   (set! buffer-name (telephone-line-raw mode-line-buffer-identification t))
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
   ;; (when (or (and defining-kbd-macro
   ;;                (require 'all-the-icons-nerd-fonts)
   ;;                (pushing! segment (all-the-icons-nerd-md "record-circle"))
   ;;                ;; (pushing! segment (all-the-icons-nerd-cod "record"))
   ;;                (format "DEFINING KBD MACRO..."))
   ;;           (and executing-kbd-macro
   ;;                (format "EXECUTING KBD MACRO..."))))
   (when (or (buffer-narrowed-p)
             (and (bound-and-true-p fancy-narrow-mode)
                  (fancy-narrow-active-p))
             (bound-and-true-p dired-narrow-mode))
     (pushing! segment (all-the-icons-material "unfold_less" :face 'warning)))
   (string-join segment "\s")))
;;;;; pomodoro
(telephone-line-defsegment* +telephone-line-pomodoro-segment ()
  )
;;;;; org timer (what I use as pomodoro)
;; (telephone-line-defsegment* +telephone-line-org-timer-segment ()
;;   (when (bound-and-true-p org-timer-countdown-timer)
;;     ;; TODO set the face depending on the timer based on the percentage of the
;;     ;; time done.  And consider whether I chould do this with an advice or in
;;     ;; this function.
;;     (string-trim org-timer-mode-line-string)))
;; (advice-add 'org-timer-set-mode-line :around #'ignore)
;; (advice-remove 'org-timer-set-mode-line #'ignore)
;; (setq org-timer-display nil)

;; Color org-timer segment based on percentage of timer done.
;; (defun oo-mode-line-org-timer-face (string)
;;   "Return STRING propertized properly."
;;   (set! percentage)
;;   (cond ((<)
;;          (propertize string 'face 'success))
;;         (())
;;         (t
;;          (propertize string 'face 'failure))))
;;;;; current-time
;; TODO: how to display somet
(telephone-line-defsegment* +telephone-line-current-time-segment ()
  (format-time-string "%m-%d %H:%M"))
;;;;; battery
;; (telephone-line-defsegment* +telephone-line-battery-segment ()
;;   (require 'battery)
;;   (set! status (funcall battery-status-function))
;;   (set! percentage (thread-last (battery-format "%p" status)
;;                                 (string-to-number)
;;                                 (round)))
;;   (set! discharging-p (equal "discharging" (battery-format "%B" status)))
;;   (when (and discharging-p (< percentage 60))
;;     (format "%s%%%" percentage)))
;;;;; emms
;; TODO: Add how much time is left plaing...
;; (telephone-line-defsegment* +telephone-line-emms-segment ()
;;   (when (bound-and-true-p emms-player-playing-p)
;;     (set! path (emms-track-description (emms-playlist-current-selected-track)))
;;     (set! track (file-name-nondirectory (directory-file-name path)))
;;     (cond ((bound-and-true-p emms-player-paused-p)
;;            (format "PAUSED %s" track))
;;           ((bound-and-true-p emms-repeat-track)
;;            (format "REPEATING %s" track))
;;           (t
;;            (format "PLAYING %s" track)))))
;;;;; version control information
(telephone-line-defsegment* +telephone-line-vc-segment ()
  (when buffer-file-name
    (require 'vc)
    (aand (vc-backend buffer-file-name)
          (substring vc-mode (+ (if (eq it 'Hg) 2 3) 2))
          (string-trim it))))
;;;;; read-only
(telephone-line-defsegment* +telephone-line-read-only-segment ()
  (when buffer-read-only
    "READ-ONLY"))
;;;; add utilities for updating the modeline
;; When you modify the modeline variable the modeline is not automatically
;; updated.  You only see the updated version when you open a new buffer.  To
;; actually see the updated modeline in buffers that are already open you need
;; to change their buffer-local mode-line-format variable and then call
;; `force-mode-line-update' after you make the change to the default value of
;; `mode-line-format'.
(defun oo-update-modeline ()
  "Render the updated modeline."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq-local mode-line-format (default-value 'mode-line-format))))
  (force-mode-line-update))

(defun! +telephone-line-update ()
  "Update the telephone-line modeline."
  (interactive)
  (set! modeline (if telephone-line-mode `("%e" ,@(telephone-line--generate-mode-line)) telephone-line--default-mode-line))
  (setq-default mode-line-format modeline)
  (oo-update-modeline))
;;; provide
(provide 'config-telephone-line)
;;; config-telephone-line.el ends here
