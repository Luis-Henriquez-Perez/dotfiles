;;; oo-after-load-abbrev.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; At first I thought that abbrev-mode is obsoleted by things like yasnippet or
;; tempel, but after considering it I realized that it is very useful.  The
;; greatest weakness of abbrev is also its greatest strength--namely, the
;; ability to automatically expand.  With
;; this file I want to squeeze the most out of abbrev.
;;
;; GOTCHAs:
;; 1. (define-abbrev emacs-lisp-mode-abbrev-table ...) doesnt seem to work
;;    unless youve already enabled emacs-lisp-mode.
;; 2. (define-abbrev prog-mode-abbrev-table ...) doesnt seem to work at all even
;;    if you evaluate it after prog-mode has been run.
;; 3. It is a bit challenging to check a specific abbrev table because
;; 4. I expected abbrevs in prog-mode-abbrev-table to work in programming
;;    modes--not the case.
;;
;;; Code:
;; TODO: figure out a better way to handle misspellings.
;; (require '90-wikipedia-common-misspellings)
(require 'abbrev)
(require '04-base-custom)
(require 'oo-global-abbrev-table)
(require 'oo-wikipedia-misspellings-table)
;;;; abbrevs
;;;;; extend abbrev syntax to use "."
;; Allow the use of periods, colons, and underscores in global abbrevs.  The
;; point of doing this is to let me name certain abbrevs with easy to remember,
;; intuitive names while also preventing name clashes with the preceding
;; punctuation.
;; (abbrev-table-put global-abbrev-table :regexp "\\(?:\\`\\|^\\|[[:space:]]\\)\\(?1:\\.?[[:alpha:]]+\\)")
;; Do not adjust the abbrev syntax yet.
;; (abbrev-table-put global-abbrev-table :regexp nil)
;;;;; TODO: deal with problem of non-capitalization of mutliple words
;; When an abbrev expands to multiple words the initial word does not get
;; capitalized with captain.  But it does work when abbrev expands to just one
;; word.  So the first question is how to go about solving this problem.  As is
;; the case in emacs, there are multiple ways.  One way is changing the value
;; of.  The other way is using a hook for a multi-word expansion.  The hook would.
;;;;; text abbrevs
;; These are abbreviations that I want to be using.
;;;;;; general
;; ;; Most often, I want the abbrevs I define to be expanded in either plain text
;; ;; or in programming language comments.
;; (defun oo-text-abbrev (abbrev expansion)
;;   "Define an abbreviation."
;;   (define-abbrev global-abbrev-table abbrev expansion nil :enable-function #'oo-enable-global-abbrev-p))
;;;;;; set abbrevs my way
;; Only expand abbreviations in prog-mode string or comments.  Otherwise, they
;; could interfere with function names.
;; This is meant for use
(defun oo-enable-global-abbrev-p ()
  "Return non-nil when text-mode abbrevs should be enabled."
  (or (derived-mode-p 'text-mode)
      (oo-in-string-or-comment-p)))
;;;;; all abbrevs
(abbrev-table-put global-abbrev-table :enable-function #'oo-enable-global-abbrev-p)
(abbrev-table-put oo-wikipedia-misspellings-table :enable-function #'oo-enable-global-abbrev-p)
;; Technically I should insert the parents "safetly" by checking for existing parents.
(alet (-snoc (abbrev-table-get global-abbrev-table :parents) oo-wikipedia-misspellings-table)
  (abbrev-table-put global-abbrev-table :parents it))
;;; provide
(provide 'oo-after-load-abbrev)
;;; oo-after-load-abbrev.el ends here
