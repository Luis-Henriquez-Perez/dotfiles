;;; config-abbrev.el --- abbrev configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for abbrev.
;;
;;; Code:
(require 'abbrev)
(require 'base)
(require '+abbrev-table-main)
(require '+abbrev-table-wikipedia-misspellings)
;;;; abbrevs
;;;; set abbrevs my way
(defun! +abbrev-enable-text-abbrev-p ()
  "Return non-nil when text abbrevs should be enabled.
This is when the current major-mode is derived from text-mode or point is in a
string or comment."
  (or (derived-mode-p 'text-mode)
	  ;; These cases prevent abbreviation from expanding words outside of a
	  ;; string or comment when in some programming mode.
	  (acase (oo-in-string-or-comment-p)
		(string
		 (set! string-beg (car (bounds-of-thing-at-point 'string)))
		 (set! word-beg (save-excursion (backward-word) (point)))
		 (> word-beg string-beg))
		(comment
		 (set! comment-beg (save-excursion (comment-beginning) (point)))
		 (set! word-beg (save-excursion (backward-word) (point)))
		 (> word-beg comment-beg))
		(t
		 it))))
;;;; all abbrevs
(abbrev-table-put +abbrev-table-main :enable-function #'+abbrev-enable-text-abbrev-p)
(abbrev-table-put +abbrev-table-wikipedia-misspellings :enable-function #'+abbrev-enable-text-abbrev-p)

(alet (-snoc (abbrev-table-get global-abbrev-table :parents) +abbrev-table-main +abbrev-table-wikipedia-misspellings)
  (abbrev-table-put global-abbrev-table :parents it))
;;;; automatically add period
;; I do not like manually adding periods to the end of sentences.  Having moved
;; from using one space after a sentence to two, I find it particularl daunting
;; to type period, space, space whenever I am ending one sentence and starting a
;; new one.  With this customization when I type space, space, following a word
;; it is converted into period space space.  Additionally, if I end a sentence
;; line with two spaces and I press ESC, the trailing two spaces are replaced
;; with a period.
(defadvice! add-period-maybe (around abbrev--default-expand expand-fn)
  "Add a period when necessary."
  (prog1 (funcall expand-fn)
    (when (or (derived-mode-p 'text-mode) (oo-in-string-or-comment-p))
      (set! eol (line-beginning-position -1))
      (set! rx "\\([[:word:]]\\)\\([[:space:]][[:space:]]\\)\\([^[:space:]]+\\)")
      (cond ((looking-back rx eol)
             (replace-match "\\1.\\2\\3" nil nil nil 0))
            ((looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" eol)
             (replace-match "\\1."))))))
;; The behavior I want is if I type two spaces then replace with period
;; followed by two spaces.
;;;; add a new abbreviation
;; I am aware that there is already a command to add abbreviations to their abbrev-file but I do
;; not use the abbreviation file partly because I do not think it lends itself
;; well for version control--which I want for my abbrevs--and because I do not
;; like the indentation and code format with which it saves the abbrev table.
(defun! +abbrev-add-new-abbrev ()
  "Add abbreviation at point to `+abbrev-table-main'.
Prompt for the expansion and insert the abbreviation directly into
`+abbrev-table-main.el`.  Also evaluate the the file and expand the
abbreviation at point. This function assumes the abbreviations file
`+abbrev-table-main.el` is located at
'~/.local/share/chezmoi/dot_config/emacs/lisp/'."
  (interactive)
  (set! abbrev (downcase (substring-no-properties (thing-at-point 'word))))
  ;; Replace abbreviation?
  (set! existing-expansion (abbrev-expansion abbrev +abbrev-table-main))
  (set! prompt (format "Abbrev for %s already expands to %s, replace it?" abbrev existing-expansion))
  (nif! (or (not existing-expansion) (and existing-expansion (y-or-n-p prompt)))
      (message "O.K., cancelled replacing abbrev for %s." abbrev)
    (set! expansion (read-string (format "Expansion for '%s': " abbrev)))
    (message "Expansion for '%s': %s" abbrev expansion)
    (set! regexp "^(define-abbrev-table '\\+abbrev-table-main\n\\(?:^\\)[[:blank:]]+'(")
    (set! file "~/.local/share/chezmoi/dot_config/emacs/lisp/+abbrev-table-main.el")
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
;;; provide
(provide 'config-abbrev)
;;; config-abbrev.el ends here
