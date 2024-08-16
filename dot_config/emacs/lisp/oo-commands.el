;;; oo-commands.el --- Generic commands -*- lexical-binding: t; -*-
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
;; This file contains non-package specific commands that I use generally.  Some
;; of these commands will not be perfect in that they are specific to me instead
;; of generalized polished commands you might see in packages.  Instead,
;; these functions are very specific to me and my workflow.
;;
;;; Code:
(require 'base)
;;;; opening specific files
;; A complicating factor is the fact that I use the chezmoi directory as the
;; main way to edit these files.
(defun! oo--chezmoi-source-path (target-dir)
  "Get the source path for a given TARGET-DIR using chezmoi."
  (cl-assert (executable-find "chezmoi"))
  (set! command (format "chezmoi source-path %s" (shell-quote-argument target-dir)))
  (set! source-path (string-trim (shell-command-to-string command)))
  ;; Remove any trailing newlines from the output
  (and (not (string-empty-p source-path))
       source-path))

(defun oo-open-emacs-config ()
  "Open Emacs configuration."
  (interactive)
  (switch-to-buffer (dired (oo--chezmoi-source-path user-emacs-directory))))

(defun oo-open-emacs-init-file ()
  "Open init file."
  (interactive)
  (switch-to-buffer (find-file-noselect (oo--chezmoi-source-path user-init-file))))

(defun oo-open-emacs-lisp-dir ()
  "Open lisp directory."
  (interactive)
  (switch-to-buffer (find-file-noselect (oo--chezmoi-source-path oo-lisp-dir))))
;;;; window splitting
(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;;; font
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (set! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))
;;;; sorting
;; If I have a region selected, use that.
;; If it is interactive and I do not have a region selected use the beginning
;; of the current line and the end of the buffer.
(defun! oo-sort-dwim (beg end)
  "Sort lines the way I like it."
  (interactive
   (if (region-active-p)
	   (list (region-beginning) (region-end))
	 (list (line-beginning-position) (point-max))))
  (let! regexp (rx "(" (group (or "autoload" "require" "elpaca"))))
  (save-excursion
    (goto-char beg)
    (re-search-forward regexp end t nil))
  (cl-case (match-string 1)
	("autoload" (oo-sort-autoload-forms beg end))
	("require" (oo-sort-require-forms beg end))
	("elpaca" (oo-sort-elpaca-forms beg end))
	(t (error "No sorting method detected"))))

;; This is meant to sort the great number of install package forms I have in
;; `init-elpaca'.
(defun! oo-sort-elpaca-forms (beg end)
  "Sort elpaca forms lexicographically by package name."
  (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

(defun! oo-sort-autoload-forms (beg end)
  "Sort autoload forms lexicographically by package name."
  (set! rx "(autoload[[:blank:]]+#'[^[:space:]]+[[:blank:]]+\"\\(.+?\\)\".+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

;; This is meant to sort the great number of `require' forms in the init file.
(defun! oo-sort-require-forms (beg end)
  "Sort require forms lexicographically by feature name."
  (set! rx (rx (seq "(require" (one-or-more blank) "'" (group (1+ nonl))")")))
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))
;;;; miscellaneous
(defun oo-dwim-narrow (keep-narrowing-p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: narrow to region, outline heading, org-src-block, org-subtree, or
defun, whichever applies first.

With prefix KEEP-NARROWING-P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not keep-narrowing-p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((equal 'comment (oo-in-string-or-comment-p))
         (save-excursion (outli-toggle-narrow-to-subtree)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-narrow-to-block) t)
             (org-narrow-to-subtree)))
        (t
         (narrow-to-defun))))
;; You could actually do this via abbrev-mode as well.  And actually it might be
;; better in a sense because.
(defun! oo-dwim-space ()
  "Replace two consecutive spaces with a period."
  (interactive)
  (set! rx "\\([[:word:]]\\)\\([[:space:]][[:space:]]\\)\\([^[:space:]]+\\)")
  (cond ((and (or (derived-mode-p 'text-mode)
                  (oo-in-string-or-comment-p))
              (looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" nil))
         (replace-match "\\1.\s\s"))
        (t
         (insert "\s"))))

(defun! oo-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))

(defun oo-kill-emacs-no-confirm ()
  "Kill Emacs without confirmation."
  (let (confirm-kill-emacs)
    (call-interactively #'kill-emacs)))

;; Keep track of the themes that I have loaded and do not allow repetitions.
(defvar oo-loaded-themes nil
  "Themes that have already been loaded.")

(defun! oo-load-random-theme ()
  "Load a random theme."
  (interactive)
  (set! not-loaded (-difference (custom-available-themes) oo-loaded-themes))
  (set! theme (seq-random-elt not-loaded))
  (condition-case err
      (progn (load-theme theme)
             (push theme oo-loaded-themes)
             (message "Loaded theme `%s'..." theme))
    (signal (car err) (cdr err))))

;; This idea is based on the following link where xah lee talks about why the
;; scratch buffer is outdated.  It does not follow the trend of "untitled1",
;; "untitled2" as xah lee recommended because it is just easier and more
;; consistent to use Emacs's buffer naming style.
;; http://xahlee.info/emacs/emacs/modernization_scratch_buffer.html
(defun! oo-new-buffer ()
  "Create a new blank buffer."
  (interactive)
  (display-buffer (generate-new-buffer "untitled")))

;; This has to do with chezmoi.
(defun! update-emacs-config ()
  ;; Clear the existing files that are not part of chezmoi.
  ())
;;; provide
(provide 'oo-commands)
;;; oo-commands.el ends here
