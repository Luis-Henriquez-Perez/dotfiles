;;; oo-init-abbrevs.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'abbrev)
(require 'oo-base)
(require 'oo-abbrev-table-main)
(require 'oo-abbrev-table-wikipedia-misspellings)
;;;; abbrevs
;;;;; set abbrevs my way
(defun oo-enable-text-abbrev-p ()
  "Return non-nil when text abbrevs should be enabled.
This is when the current major-mode is derived from text-mode or point is in a
string or comment."
  (or (derived-mode-p 'text-mode)
      (oo-in-string-or-comment-p)))
;;;;; all abbrevs
(abbrev-table-put oo-abbrev-table-main :enable-function #'oo-enable-text-abbrev-p)
(abbrev-table-put oo-abbrev-table-wikipedia-misspellings :enable-function #'oo-enable-text-abbrev-p)

(alet (-snoc (abbrev-table-get global-abbrev-table :parents) oo-abbrev-table-main oo-abbrev-table-wikipedia-misspellings)
  (abbrev-table-put global-abbrev-table :parents it))
;;;;; automatically add period
;; I do not like manually adding periods to the end of sentences.  Having moved
;; from using one space after a sentence to two, I find it particularl daunting
;; to type period, space, space whenever I am ending one sentence and starting a
;; new one.  With this customization when I type space, space, following a word
;; it is converted into period space space.  Additionally, if I end a sentence
;; line with two spaces and I press ESC, the trailing two spaces are replaced
;; with a period.
(defadvice! abbrev--default-expand@ARauto-add-periods (expand-fn)
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
;;; provide
(provide 'oo-init-abbrev)
;;; oo-init-abbrev.el ends here
