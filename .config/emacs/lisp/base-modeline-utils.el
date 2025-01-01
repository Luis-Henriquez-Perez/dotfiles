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
;;;; requirements
(require 'powerline)
;;;; variables
(defvar oo-modeline-icons 'all-the-icons
  "Type of icons to use in the modeline.
Values can be `nerd-icons', `all-the-icons' and nil.")

(defmacro! modeline-component! (&rest args)
  "Define a modeline segment."
  (declare (indent defun) (doc-string 3))
  (set! (name arglist metadata body) (oo--definer-components args))
  (set! return-value (gensym "return-value"))
  `(defun! ,name ,arglist
     ,@metadata
     (let (,return-value)
       (condition-case err
           (setq ,return-value (progn ,@body))
         (error
          (error! "Modeline segment %s raised a %s because of %s" ',name (car err) (cdr err))
          (return! "")))
       (pcase ,return-value
         ('nil
          "")
         ((pred stringp)
          ,return-value)
         (_
          (error! "Modeline segment %s returned a non-string %s" ',name ,return-value)
          "")))))
;;;; utility functions
;; (defun ;; When you modify the modeline variable the modeline is not automatically
;; updated.  You only see the updated version when you open a new buffer.  To
;; actually see the updated modeline in buffers that are already open you need
;; to change their buffer-local mode-line-format variable and then call
;; `force-mode-line-update' after you make the change to the default value of
;; `mode-line-format'.
(defun oo-modeline-update ()
  "Render the updated modeline."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq-local mode-line-format (default-value 'mode-line-format))))
  (force-mode-line-update))
;;;; segments
(modeline-component! oo-modeline-component--buffer-name ()
  "Buffer name indicator for modeline."
  (pcase oo-modeline-icons
    ('nerd-icons
      (format "%s %s" (nerd-icons-icon-for-buffer) (buffer-name)))
    ('all-the-icons
      (format "%s %s" (all-the-icons-icon-for-buffer) (buffer-name)))
    (_
     (buffer-name))))

(modeline-component! oo-modeline-component--kbd-macro ()
  "Keybinding macro indicator."
  (or (and defining-kbd-macro
           (pcase oo-modeline-icons
             ('nerd-icons (nerd-icons-mdicon "nf-md-record_circle" :v-adjust -0.2))
             ('all-the-icons (all-the-icons-material "fiber_manual_record" :face 'error :v-adjust -0.2))
             (_ "•REC")))
      (and executing-kbd-macro
           (pcase oo-modeline-icons
             ('nerd-icons (nerd-icons-mdicon "nf-md-play"))
             ('all-the-icons (all-the-icons-faicon "play"))
             (_ "PLAYING")))))

(modeline-component! oo-modeline-component--version-control ()
  "Return the branch name as a modeline segment."
  (when (and (buffer-file-name)
             (locate-dominating-file (buffer-file-name) ".git"))
    (set! branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (pcase oo-modeline-icons
      ('nerd-icons
        (set! icon (nerd-icons-devicon "nf-dev-git_branch" :v-adjust -0.01))
        (format "%s %s" icon branch))
      ('all-the-icons
        (set! icon (all-the-icons-octicon "git-branch" :v-adjust -0.01))
        (format "%s %s" icon branch))
      (_
       branch))))

(modeline-component! oo-modeline-component--narrow ()
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

(modeline-component! oo-modeline-component--pomodoro ()
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
          ("w" (set! icon (all-the-icons-octicon "clock" :v-adjust 0)))
          ("b" (set! icon (all-the-icons-faicon "coffee" :v-adjust 0))))
        (format "%s %s" icon time))
      ('nerd-icons
        (pcase type
          ("w" (set! icon (nerd-icons-pomicon "nf-pom-pomodoro_ticking" :v-adjust 0)))
          ("b" (set! icon (nerd-icons-codicon "nf-cod-coffee" :v-adjust 0))))
        (format "%s %s" icon time))
      (_
       (format "%s %s" type time)))))

(modeline-component! oo-modeline-component--current-time ()
  "Display the current time."
  (format-time-string "%m-%d %H:%M"))

(modeline-component! oo-modeline-component--read-only ()
  "Return indicator for whether file is read-only."
  (when buffer-read-only
    (pcase oo-modeline-icons
      ('all-the-icons
        (all-the-icons-material "lock" :face 'error))
      ('nerd-icons
        (set! icon (nerd-icons-faicon "nf-fa-lock" :face 'error))
        icon)
      (_
       "LOCKED"))))

(modeline-component! oo-modeline-component--buffer-modified ()
  "Return indicator for buffer modified.
If the current buffer is modified."
  (when (and (buffer-file-name) (buffer-modified-p))
    (pcase oo-modeline-icons
      ('all-the-icons
        (all-the-icons-material "save" :face 'error))
      ('nerd-icons
        (nerd-icons-faicon "nf-fa-save" :face 'error))
      (_
       "MODIFIED"))))

(modeline-component! oo-modeline-component--evil-state ()
  "Return indicator for evil state."
  (when (bound-and-true-p evil-mode)
    (symbol-name evil-state)))

(modeline-component! oo-modeline-component--battery ()
  (set! status (funcall battery-status-function))
  (set! percentage (thread-last (battery-format "%p" status)
                                (string-to-number)
                                (round)))
  (set! discharging-p (equal "discharging" (battery-format "%B" status)))
  (when (and discharging-p (< percentage 60))
    (format "%s%%%" percentage)))

(modeline-component! oo-modeline-component--emms ()
  "Return indicator for emms.
Returns whether current track is playing."
  (when (bound-and-true-p emms-player-playing-p)
    (set! path (emms-track-description (emms-playlist-current-selected-track)))
    (set! track (file-name-nondirectory (directory-file-name path)))
    (cond ((bound-and-true-p emms-player-paused-p)
           (pcase oo-modeline-icons
             ('nerd-icons
              (nerd-icons-codicon))
             ('all-the-icons
              (all-the-icons-faicon "pause-circle"))
             (_
              (format "PAUSED %s" track))))
          ((bound-and-true-p emms-repeat-track)
           (pcase oo-modeline-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-repeat"))
             ('all-the-icons
              (all-the-icons-faicon "repeat"))
             (_
              (format "REPEAT %s" track))))
          (t
           (pcase oo-modeline-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-play"))
             ('all-the-icons
              (all-the-icons-material "play-circle"))
             (_
              (format "PLAY %s" track)))))))
;;;; Org timer
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
;;;; Main modeline function
;; (require 'telephone-line)
;; (defun! oo.segment (components)
;;   (set! rendered (powerline-render (cdr args)))
;;   (if (string-empty-p rendered)
;;       (powerline-render (list rendered separator))
;;     ""))

;; (defmacro o.segmentl (&rest components)
;;   (cl-with-gensyms (result content)
;;     `(let* ((,result (list ,@components))
;;             (,content (butlast ,result)))
;;        (if (cl-some (lambda (it) (and (stringp it) (not (string-empty-p it)))) ,content)
;;            (append ,content (last ,result))
;;          nil))))

;; (defmacro o.segmentr (&rest components)
;;   (cl-with-gensyms (result content)
;;     `(let* ((,result (list ,@components))
;;             (,content (cdr ,result)))
;;        (if (cl-some (lambda (it) (and it (not (string-empty-p it)))) ,content)
;;            (cons (car ,result) ,content)
;;          nil))))
;; '((evil-state)
;;   (narrow read-only kbd-macro buffer-modified buffer-name)
;;   (version-control))
;; (segment '(evil-state) 'left)
;; (segment '(narrow read-only kbd-macro buffer-modified buffer-name) 'left)
;; (segment '(version-control) 'left)

;; (setq powerline-height 40)
;; (oo-modeline-update)
;; (setq powerline-default-separator 'arrow)

;; (defun! oo-main-modeline ()
;;   (set! active (powerline-selected-window-active))
;;   (set! mode-line (if active 'mode-line 'mode-line-inactive))
;;   (set! face0 (if active 'powerline-active0 'powerline-inactive0))
;;   (set! face1 (if active 'powerline-active1 'powerline-inactive1))
;;   (set! face2 (if active 'powerline-active2 'powerline-inactive2))
;;   (set! evil-face (intern (format "telephone-line-evil-%s" evil-state)))
;;   (set! separator-left (+powerline-left-separator))
;;   (set! separator-right (+powerline-right-separator))
;;   (set! lhs (append (o.segmentl (powerline-raw (oo-modeline-component--evil-state) evil-face)
;;                                 (funcall separator-left evil-face face0))
;;                     (o.segmentl (powerline-raw (oo-modeline-component--narrow) face0 'r)
;;                                 (powerline-raw (oo-modeline-component--read-only) face0 'r)
;;                                 (powerline-raw (oo-modeline-component--kbd-macro) face0 'r)
;;                                 (powerline-raw (oo-modeline-component--buffer-modified) face0 'r)
;;                                 (powerline-raw (oo-modeline-component--buffer-name) face0)
;;                                 (funcall separator-left face0 face1))
;;                     (o.segmentl (powerline-raw (oo-modeline-component--version-control) face1)
;;                                 (funcall separator-left face1 face2))))
;;   (set! rhs (append (o.segmentr (funcall separator-right face2 face1)
;;                                 (powerline-raw (oo-modeline-component--pomodoro) 'l))
;;                     (o.segmentr (funcall separator-right face0 face1)
;;                                 (powerline-raw (oo-modeline-component--current-time) face0 'l))))
;;   (concat (powerline-render lhs)
;;           (powerline-fill face2 (powerline-width rhs))
;;           (powerline-render rhs)))

;; (o.modeline-update)
;; (setq-local mode-line-format '("%e" (:eval (oo-main-modeline))))
;;; provide
(provide 'base-modeline-utils)
;;; base-modeline-utils.el ends here
