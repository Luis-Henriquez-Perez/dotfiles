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

(defun! oo-sort-outli-headings ()
  "Sort outli headings lexicographically by title."
  (interactive)
  (set! rx ";;;;[[:blank:]]\\(.+\\)\n\\(?:\\(?:^;;[^;].+\\|^[^;].+\\)\n\\)+")
  (save-excursion (sort-regexp-fields nil rx "\\1" (point-min) (point-max))))

(defun! oo-make-init-files ()
  (interactive)
  (set! rx ";;;;[[:blank:]]\\(.+\\)\n\\(\\(?:\\(?:^;;[^;].+\\|^[^;].+\\)\n\\)+\\)")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward rx nil t nil)
      (set! package (match-string 1))
      (set! content (match-string 2))
      (oo--make-init-file package content))))

(defun! oo-make-init-file ()
  (interactive)
  (set! rx ";;;;[[:blank:]]\\(.+\\)\n\\(\\(?:\\(?:^;;[^;].+\\|^[^;].+\\)\n\\)+\\)")
  (goto-char (line-beginning-position))
  (nif! (looking-at rx)
      (message "Not at outline headline.")
    (set! package (match-string 1))
    (set! content (match-string 2))
    (oo--make-init-file package content)))

(defun! oo--make-init-file (package content)
  "Generate init files from outline package heading."
  ;; Point should be on the headline.
  ;; Insert the boilerplate.
  (set! dir "~/.local/share/chezmoi/dot_config/emacs/lisp/")
  (set! feature (format "init-%s" package))
  (set! filename (concat feature ".el"))
  (set! path (expand-file-name filename dir))
  (set! content
        (concat (format ";;; %s --- initialize %s -*- lexical-binding: t; -*-\n" filename package)
                (oo-copyright-license)
                (format ";;; Commentary:\n;;\n;; Initialize %s.\n;;\n" package)
                (format ";;; Code:\n(require 'oo-base)\n")
                content
                (format ";;; provide\n(provide '%s)\n" feature)
                (format ";;; %s ends here\n" filename)))
  (write-region content nil path))

(defun! oo-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))
;;; provide
(provide 'oo-commands)
;;; oo-commands.el ends here
