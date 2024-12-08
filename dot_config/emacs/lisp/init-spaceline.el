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
(require 'spaceline)
(require 'spaceline-segments)
(require 'all-the-icons)
;;;; reset powerline after theme change
(defun oo-reset-modeline-h (_)
  (powerline-reset))

(add-hook 'enable-theme-functions #'oo-reset-modeline-h)
;;;; defsegment!
;; This lets me use autolet! in the body of the macro and expresses the segments
;; as functions that I can freely modify and re-evaluate to make the segment
;; change in real time.  This makes it much easier to debug segments or even to
;; determine if they work beforehand.
(defmacro! +spaceline-define-segment! (name value &rest props)
  (declare (indent 1) (doc-string 2))
  (set! fn (intern (format "+spaceline-%s-segment" name)))
  (set! meta (when (stringp (car-safe value)) (list (pop value))))
  `(progn
     (defun! ,fn () ,@(append meta value))
     (spaceline-define-segment ,name ,@(append meta `((funcall #',fn))) ,@props)))
;;;; set powerline height
(setq powerline-height 33)
;;;; define custom segments
(+spaceline-define-segment! +kbd-macro
  "Display an icon to represent when."
  (or (and defining-kbd-macro
           (cond ((featurep 'all-the-icons)
                  (all-the-icons-material "fiber_manual_record" :face 'error :v-adjust -0.2)
                  ;; (all-the-icons-nerd-cod "record" :face 'error :v-adjust -0.1)
                  )
                 ((featurep 'nerd-icons)
                  (nerd-icons-codicon "nf-cod-record"))
                 (t
                  "•REC")))
      (and executing-kbd-macro
           (all-the-icons-faicon "play" :face 'error)
           ;; (format "EXECUTING KBD MACRO...")
           )))

(+spaceline-define-segment! +narrow
  "Indicate when the current buffer is narrowed."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (cond ((fboundp 'all-the-icons-material)
           (all-the-icons-material "unfold_less" :face 'warning))
          ((fboundp 'nerd-icons-octicon)
           (nerd-icons-octicon "nf-oct-fold" :face 'warning))
          (t
           "><"))))

(+spaceline-define-segment! +buffer-read-only
  "Display"
  (when buffer-read-only
    (if (not (and (display-graphic-p) (require 'all-the-icons)))
        "X"
      (cond ((fboundp 'all-the-icons-material)
             (all-the-icons-material "lock" :face 'error))
            ((fboundp 'nerd-icons-octicon)
             (nerd-icons-faicon "nf-fa-lock"))
            (t
             "LOCK")))))

(+spaceline-define-segment! +buffer-modified
  "Buffer modified"
  (when (and (buffer-file-name) (buffer-modified-p))
    (cond (t (all-the-icons-material "save" :face 'error))
          (t (all-the-icons-material "save" :face 'error)))))

(+spaceline-define-segment! +pomodoro
  "Display left for pomodoro."
  (when (and (bound-and-true-p pomodoro-mode-line-string)
             (not (string-empty-p pomodoro-mode-line-string)))
    (require 'all-the-icons-nerd-fonts)
    (string-match (rx (group letter) (group digit digit ":" digit digit)) pomodoro-mode-line-string)
    (set! type (match-string 1 pomodoro-mode-line-string))
    (set! time (match-string 2 pomodoro-mode-line-string))
    (string-join (list
                  (pcase type
                    ;; ("w" (nerd-icons-pomicon "nf-pom-pomodoro_ticking"))
                    ("w" (all-the-icons-nerd-pom "pomodoro-ticking" :face 'powerline-active0 :v-adjust 0))
                    ;; ("b" (nerd-icons-codicon "nf-cod-coffee"))
                    ("b" (all-the-icons-nerd-cod "coffee" :face 'powerline-active0 :v-adjust 0))
                    )
                  time)
                 "\s")))

;; The value of vc-mode is not always correct.
(+spaceline-define-segment! +version-control
  "Display current git branch."
  (let ((default-directory (or (and buffer-file-name
                                    (locate-dominating-file buffer-file-name ".git"))
                               default-directory)))
    (when (and default-directory (file-directory-p (concat default-directory ".git")))
      (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))))

(+spaceline-define-segment! +evil-state
  "Display the current evil state if evil-mode is enabled."
  (when (bound-and-true-p evil-mode)
    (symbol-name evil-state)))

(+spaceline-define-segment! +current-time
  "Display the current time."
  (format-time-string "%m-%d %H:%M"))
;;;; toggle default separator
;; I want the ability to quickly switch between different separators.
(setq powerline-default-separator 'curve)

(defun! oo-choose-modeline-separator ()
  ""
  (interactive)
  (set! separators '(alternate arrow arrow-fade bar box brace
                               butt chamfer contour curve rounded roundstub wave zigzag
                               slant utf-8))
  (awhen (completing-read "Choose separator: " separators)
    (setq powerline-default-separator it)
    (spaceline-compile)))

(defun! oo-choose-random-separator ()
  "Set a random separator."
  (interactive)
  (set! separators '(alternate arrow arrow-fade bar box brace
                               butt chamfer contour curve rounded roundstub wave zigzag
                               slant utf-8))
  (setq powerline-default-separator (seq-random-elt separators))
  (spaceline-compile)
  (message "set separator to %s" powerline-default-separator))
;;;; do not byte-compile the modeline at startup
;; Although this saves time the longer you use the modeline, it means that the
;; call to `spaceline-compile' is called takes significantly longer which is
;; particularly undesirable during startup.  Despite what the README says the
;; rendering/updating of the modeline does not make a noticeable difference to
;; me.  I imagine it matters more for particularly expensive modeline segments.
;; Still I will byte-compile it but not during startup, at some point during
;; idle time.
;; TODO: during idle time byte-compile the spaceline function.
(setq spaceline-byte-compile nil)
;;;; initialize modeline at startup
(defun oo-init-modeline-h ()
  (spaceline-compile
    'main
    '((my-evil-state :face (alet (intern (format "spaceline-evil-%s" evil-state)) (if (facep it) it 'default-face)))
      ((my-narrow my-kbd-macro my-buffer-read-only my-buffer-modified buffer-id remote-host) :priority 98)
      (my-version-control :face 'powerline-active0))
    '((my-pomodoro :face 'powerline-active0) major-mode (my-current-time :face (+spaceline-evil-face))))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(add-hook 'after-init-hook #'oo-init-modeline-h 90)
;;; provide
(provide 'init-spaceline)
;;; init-spaceline.el ends here
