;;; +abbrev-enable-functions.el --- predicate functions for enabling abbrevs -*- lexical-binding: t; -*-
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
;; Here I define the predicate functions I need determining of abbrevs should be
;; enabled.  I actually did not want this to be in a separate file but things
;; were complicated.
;;
;;; Code:
;;;; emacs-lisp abbrevs
(defun +abbrev-enable-elisp-abbrevs-p ()
  "Return non-nil when emacs-lisp-mode abbrevs should expand.
This is when `emacs-lisp-mode' is enabled and point is not in a string or
comment."
  (and (derived-mode-p 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))))
;;;; plain text abbrevs
(defun! +abbrev-enable-plain-text-abbrevs-p ()
  "Return non-nil when text abbrevs should be enabled.
This is when the current major-mode is derived from text-mode or point is in a
string or comment."
  (or (derived-mode-p 'text-mode)
	  ;; These cases prevent abbreviation from expanding words outside of a
	  ;; string or comment when in some programming mode.
	  (cl-case (oo-in-string-or-comment-p)
		(string
		 (set! string-beg (car (bounds-of-thing-at-point 'string)))
		 (set! word-beg (save-excursion (backward-word) (point)))
		 (> word-beg string-beg))
		(comment
		 (set! comment-beg (save-excursion (comment-beginning) (point)))
		 (set! word-beg (save-excursion (backward-word) (point)))
         ;; The first word of a comment actually starts at `comment-beg' but
         ;; this never happens for a string.
         (>= word-beg comment-beg)))))
;;; provide
(provide '+abbrev-enable-functions)
;;; +abbrev-enable-functions.el ends here
