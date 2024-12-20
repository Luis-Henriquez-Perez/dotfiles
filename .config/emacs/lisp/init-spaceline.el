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
;;;; settings
(opt! spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
(opt! powerline-height 33)
(opt! powerline-default-separator 'curve)
;; Although this saves time the longer you use the modeline, it means that the
;; call to `spaceline-compile' is called takes significantly longer which is
;; particularly undesirable during startup.  Despite what the README says the
;; rendering/updating of the modeline does not make a noticeable difference to
;; me.  I imagine it matters more for particularly expensive modeline segments.
;; Still I will byte-compile it but not during startup, at some point during
;; idle time.
;; TODO: during idle time byte-compile the spaceline function.
(setq spaceline-byte-compile nil)
;;;; reset powerline after theme change
(hook! enable-theme-functions powerline-reset :ignore-args t)
;;;; defsegment!
;; This lets me use autolet! in the body of the macro and expresses the segments
;; as functions that I can freely modify and re-evaluate to make the segment
;; change in real time.  This makes it much easier to debug segments or even to
;; determine if they work beforehand.
(defmacro! +spaceline-define-segment! (name &rest body)
  (declare (indent 1) (doc-string 2))
  (string-match "\\`\\+\\(.+\\)\\'" (symbol-name name))
  (set! base (match-string 1 (symbol-name name)))
  (set! fn (intern (format "+spaceline-%s-segment" base)))
  (set! docstring (when (stringp (car-safe body)) (list (pop body))))
  `(progn
     (defun! ,fn ()
       ,@docstring
       (condition-case err
           (progn ,@body)
         (error
          (error! "Segment %s raised an %s error because of %s." ',base (car err) (cdr err))
          "X")))
     (spaceline-define-segment ,name ,@docstring (,fn))))
;;;; segments
(+spaceline-define-segment! +kbd-macro
  "Display an icon to represent when."
  (or (and defining-kbd-macro
           (if (display-graphic-p)
               (all-the-icons-material "fiber_manual_record" :face 'error :v-adjust -0.2)
             "•REC"))
      (and executing-kbd-macro
           (all-the-icons-faicon "play" :face 'error))))

(+spaceline-define-segment! +narrow
  "Indicate when the current buffer is narrowed."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (if (display-graphic-p)
        (all-the-icons-material "unfold_less" :face 'warning)
      "><")))

(+spaceline-define-segment! +buffer-read-only
  "Display"
  (when buffer-read-only
    (if (display-graphic-p)
        (all-the-icons-material "lock" :face 'error)
      "LOCKED")))

(+spaceline-define-segment! +buffer-modified
  "Buffer modified"
  (when (and (buffer-file-name) (buffer-modified-p))
    (all-the-icons-material "save" :face 'error)))

(defvar pomodoro-mode-line-string)
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
                    ("w" (all-the-icons-nerd-pom "pomodoro-ticking" :face 'powerline-active0 :v-adjust 0))
                    ("b" (all-the-icons-nerd-cod "coffee" :face 'powerline-active0 :v-adjust 0)))
                  time)
                 "\s")))

(defun! oo-dotfile-git-command ()
  "Return the git command for dotfile operations."
  (set! dots (expand-file-name "~/.dotfiles/"))
  (set! home (expand-file-name "~"))
  (format "%s --git-dir=%s --work-tree=%s" (executable-find "git") dots home))

(defun! oo-is-dotfile-p ()
  "Return non-nil if current-buffer is a dotfile."
  :init ((default-directory default-directory))
  (set! fname (shell-quote-argument (convert-standard-filename (buffer-file-name))))
  (set! default-directory (file-name-directory fname))
  (shell-command-to-string (oo-dotfile-git-command)))

(+spaceline-define-segment! +version-control
  "Display current git branch.
If file is a dotfile managed by my git bare repo, display that branch."
  (set! git (executable-find "git"))
  (when (and (buffer-file-name)
             (or (locate-dominating-file (buffer-file-name) ".git")
                 (and (oo-is-dotfile-p)
                      (set! git (oo-dotfile-git-command)))))
    (set! default-directory (file-name-directory fname))
    (set! bg (face-attribute 'powerline-active0 :background nil 'default))
    (set! fg (face-attribute 'warning :background nil 'default))
    (message "branch-> %s" branch)
    (set! branch (thread-last (format "%s rev-parse --abbrev-ref HEAD" git)
                              (shell-command-to-string)
                              (string-trim)))
    (set! face `((t (:background ,bg :foreground ,fg))))
    (if (display-graphic-p)
        (format "%s %s" (all-the-icons-octicon "git-branch" :face face :v-adjust -0.01) branch)
      branch)))
(shell-command-to-string (format "%s rev-parse --abbrev-ref HEAD" (oo-dotfile-git-command)))
(+spaceline-define-segment! +evil-state
  "Display the current evil state if evil-mode is enabled."
  (when (bound-and-true-p evil-mode)
    (symbol-name evil-state)))

(+spaceline-define-segment! +current-time
  "Display the current time."
  (format-time-string "%m-%d %H:%M"))
;;;; toggle default separator
;; I want the ability to quickly switch between different separators.

;; (defun! oo-choose-modeline-separator ()
;;   ""
;;   (interactive)
;;   (set! separators '(alternate arrow arrow-fade bar box brace
;;                                butt chamfer contour curve rounded roundstub wave zigzag
;;                                slant utf-8))
;;   (awhen! (completing-read "Choose separator: " separators)
;;     (setq powerline-default-separator it)
;;     (spaceline-compile)))

;; (defun! oo-choose-random-separator ()
;;   "Set a random separator."
;;   (interactive)
;;   (set! separators '(alternate arrow arrow-fade bar box brace
;;                                butt chamfer contour curve rounded roundstub wave zigzag
;;                                slant utf-8))
;;   (setq powerline-default-separator (seq-random-elt separators))
;;   (spaceline-compile)
;;   (message "set separator to %s" powerline-default-separator))
;;;; initialize modeline at startup
(defhook! oo-initialize-modeline-h (after-init-hook :depth 90)
  (spaceline-compile
    'main
    '((+evil-state :face (alet! (intern (format "spaceline-evil-%s" evil-state)) (if (facep it) it 'default-face)))
      ((+narrow +kbd-macro +buffer-read-only +buffer-modified buffer-id remote-host) :priority 98)
      (+version-control :face 'powerline-active0))
    '((+pomodoro :face 'powerline-active0) major-mode (+current-time :face (spaceline-highlight-face-evil-state))))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
;;; provide
(provide 'init-spaceline)
;;; init-spaceline.el ends here
