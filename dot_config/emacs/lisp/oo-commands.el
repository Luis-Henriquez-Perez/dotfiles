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
  "Open init file."
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
(defun! oo-sort-elpaca-forms ()
  "Sort elpaca forms lexicographically by package name."
  (interactive)
  (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" (point-min) (point-max))))

(defun! oo-sort-autoload-forms ()
  "Sort autoload forms lexicographically by package name."
  (interactive)
  (set! rx "(autoload[[:blank:]]+#'[^[:space:]]+[[:blank:]]+\"\\(.+?\\)\".+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" (point-min) (point-max))))
;;;; custom functions
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

;; Helper for maintaining my Emacs configuration.
;; (defun! oo--dwim-rename-file (orig-fn old new &optional ok-p)
;;   (prog1 (apply orig-fn old new ok-p)
;;     (set! emacs-dir (f-full "~/.local/share/chezmoi/dot_config/emacs/"))
;;     (when (and (f-ancestor-of-p emacs-dir old) (equal "el" (f-ext old)))
;;       (oo-ensure-feature-matches-filename new))))

;; (advice-add 'rename-file :around #'oo--dwim-rename-file)
;; (advice-remove 'rename-file #'oo--dwim-rename-file)

;; (defun! oo-ensure-feature-matches-filename (file)
;;   "Change usages of feature in file to match filename."
;;   (with-temp-file file
;;     (insert-file-contents file)
;;     (set! feature (file-name-sans-extension (file-name-nondirectory (directory-file-name file))))
;;     (goto-char (point-min))
;;     (set! header "\\`;;;[[:blank:]]\\(?1:.+\\)\\.el")
;;     (if (re-search-forward header nil t nil)
;;         (replace-match feature nil nil nil 1)
;;       (message "NO MATCH FOR HEADER"))
;;     (set! footer "\\(?:^(provide[[:blank:]]'\\(?1:.+\\))\n;;;[[:blank:]]\\(?2:.+\\)\\.el ends here$\\)\n\\'")
;;     (nif! (re-search-forward footer nil t nil)
;;         (message "NO MATCH FOR FOOTER")
;;       (replace-match feature nil nil nil 1)
;;       (replace-match feature nil nil nil 2))))

;; (defun oo-bubble-up (item list)
;;   (cons item (-remove-item item list)))

;; (defun! oo-generate-requires ()
;;   (set! dir "~/.local/share/chezmoi/dot_config/emacs/lisp/")
;;   (set! files (directory-files dir t "\\`init-.+\\.el\\'"))
;;   (setq files (oo-bubble-up "init-no-littering.el" files))
;;   (dolist (file files)
;;     (set! feature (intern (f-no-ext (f-base file))))
;;     (collecting! forms `(require ',feature)))
;;   forms)

;; TODO: I want to do more complex things like loading a random theme with no
;; repetitions in the current session and marking certain themes as favorite
;; themes that have a greater likelihood of being displayed.
(defun! oo-load-random-theme ()
  "Load a random theme."
  (interactive)
  (set! theme (seq-random-elt (custom-available-themes)))
  (message "Loading theme %s..." theme)
  (load-theme theme))

;; This idea is based on the following link where xah lee talks about why the
;; scratch buffer is outdated.
;; http://xahlee.info/emacs/emacs/modernization_scratch_buffer.html
(defun! oo-new-buffer ()
  "Create a new blank buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled")))

;; This has to do with chezmoi.
(defun! update-emacs-config ()
  ;; Clear the existing files that are not part of chezmoi.
  ())
;;;; miscellaneous
(defun oo-kill-emacs-no-confirm ()
  "Kill Emacs without confirmation."
  (let (confirm-kill-emacs)
	(call-interactively #'kill-emacs)))
;;; provide
(provide 'oo-commands)
;;; oo-commands.el ends here
