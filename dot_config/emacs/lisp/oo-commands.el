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
(require 'oo-base)
;;;; custom functions
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (set! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

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
        ;; (()
        ;;  (shell-narrow-to-prompt))
        (t
         (narrow-to-defun))))

(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))

(defun oo-open-emacs-config ()
  "Open Emacs configuration."
  (interactive)
  (display-buffer (dired user-emacs-directory)))

(defun oo-open-emacs-init-file ()
  "Open init file."
  (interactive)
  (display-buffer (find-file-noselect user-init-file)))

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

;; I am aware that there is already a command to add abbreviations to their abbrev-file but I do
;; not use the abbreviation file partly because I do not think it lends itself
;; well for version control--which I want for my abbrevs--and because I do not
;; like the indentation and code format with which it saves the abbrev table.
(defun! oo-add-new-abbrev ()
  "Add abbreviation at point to `oo-abbrev-table-main'.
Prompt for the expansion and insert the abbreviation directly into
`oo-abbrev-table-main.el`.  Also evaluate the the file and expand the
abbreviation at point. This function assumes the abbreviations file
`oo-abbrev-table-main.el` is located at
'~/.local/share/chezmoi/dot_config/emacs/lisp/'."
  (interactive)
  (set! abbrev (downcase (substring-no-properties (thing-at-point 'word))))
  ;; Replace abbreviation?
  (set! existing-expansion (abbrev-expansion abbrev oo-abbrev-table-main))
  (set! prompt (format "Abbrev for %s already expands to %s, replace it?" abbrev existing-expansion))
  (nif! (or (not existing-expansion) (and existing-expansion (y-or-n-p prompt)))
      (message "O.K., cancelled replacing abbrev for %s." abbrev)
    (set! expansion (read-string (format "Expansion for '%s': " abbrev)))
    (message "Expansion for '%s': %s" abbrev expansion)
    (set! regexp "^(define-abbrev-table 'oo-abbrev-table-main\n\\(?:^\\)[[:blank:]]+'(")
    (set! file "~/.local/share/chezmoi/dot_config/emacs/lisp/oo-abbrev-table-main.el")
    (set! buffer (find-file-noselect file))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward regexp nil t nil)
        (insert (format "(%S %S)\n" abbrev expansion))
        (goto-char (match-beginning 0))
        (lisp-indent-line)
        (eval-buffer)))
    (expand-abbrev)
    (message "Mapped abbrev %S to expansion %S!" abbrev expansion)))

(defun! oo-generate-test-file ()
  "Generate a test file for current file."
  (set! test-dir )
  )
;; (defun! oo-rename-elisp-file ()
;;   "Rename emacs-lisp file completely."
;;   ;; Replace all references to file in project.
;;   ;; Change name of file.
;;   ()
;;   )
;;; provide
(provide 'oo-commands)
;;; oo-commands.el ends here
