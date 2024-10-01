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
;;;; requirements
(require 'abbrev)
(require 'base)
(require '+abbrev-plain-text-abbrevs)
(require '+abbrev-emacs-lips-mode-abbrevs)
(require '+abbrev-python-mode-abbrevs)
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
      (set! rx "\\([[:word:]]\\)\\([[:blank:]][[:blank:]]\\)\\([^[:blank:]]+\\)")
      (cond ((looking-back rx eol)
             (replace-match "\\1.\\2\\3" nil nil nil 0))
            ((looking-back "\\([[:word:]]\\)[[:blank:]]\\{2,\\}" eol)
             (replace-match "\\1."))))))
;; The behavior I want is if I type two spaces then replace with period
;; followed by two spaces.
;;;; pulse expansion
;; You would be surprised at how much of an aesthetic improvement little things
;; like this can make a difference.
(defadvice! pulse-expansion (around abbrev--default-expand expand-fn)
  "Pulse around the expansion of an abbrev."
  (aprog1 (funcall expand-fn)
    (and it
         last-abbrev-location
         (require 'pulse nil t)
         (pulse-momentary-highlight-region last-abbrev-location (point)))))
;;;; ensure `post-self-insert-hook' is run after each word
;; Captain fails to capitalize the beginning of a sentence if the beginning of a
;; sentence was generated by the expansion of an abbrev because Captain
;; captializes a word during `post-insert-hook' and a multi-word expansion will
;; skip calling that hook after each word except the last one.  So here I call
;; the hook myself at the proper places.
(defadvice! ensure-self-insert (around abbrev--default-expand expand-fn)
  "Run `post-insert-hook' after each word in a multi-word expansion."
  (aprog1 (funcall expand-fn)
    (when (and it last-abbrev-location)
      (set! end (point))
      (save-excursion (goto-char last-abbrev-location)
                      (while (re-search-forward ".+?[[:blank:]]" end t nil)
                        (run-hooks 'post-self-insert-hook))))))
;;; provide
(provide 'config-abbrev)
;;; config-abbrev.el ends here
