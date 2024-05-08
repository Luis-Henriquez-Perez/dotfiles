;;; oo-after-load-captain.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Man, this is more involved than I thought it would be at first because
;; capitalization rules are different depending on whether your in prog-mode or
;; not and whether you are in a doc-string, or comment, or just a normal string.
(require 'oo-base)
;;;; determine where I am
;; TODO: generalize regexp with `defun!', `cl-defun', etc.
(defvar oo-docstring-regexp "(defun[[:blank:]]\\([^[:space:]]+\\)[[:blank:]](\\(.*\\))\n[[:blank:]]*\"")

;; TODO: generalize this regexp for comments in different languages.
(defun oo--beg-comment-block-rx ()
  "Return a regular expression that matches the beginning of a comment block."
  (rx-to-string
   `(: (or bos
           ;; comments with more
           (: bol (>= 3 ,comment-start) (0+ any) eol "\n")
           ;; blank lines
           (: bol eol "\n")
           (: bol (not ,comment-start) (* any) eol "\n"))
       (: bol (zero-or-more blank) (= 2 ,comment-start) blank))))

(defun oo--prog-mode-should-capitalize-p ()
  "Return point if."
  (pcase (oo-in-string-or-comment-p)
    ('comment
     (or (save-match-data
           ;; Limit the look back to the start of the previous line.
           (and (looking-back (oo--beg-comment-block-rx) (line-beginning-position 0))
                (match-end 0)))
         (captain--default-sentence-start)))
    ('string
     ;; If it's a docstring capitalize the first word of the doc-string.
     (when (looking-back oo-docstring-regexp (line-beginning-position 0))
       (match-end 0)))))

(defhook! text-mode-hook&set-captain-local-vars ()
  (setq-local captain-predicate #'always)
  (setq-local captain-sentence-start-function #'captain--default-sentence-start))

;; TODO: figure out the best way to add these things.
(defhook! prog-mode-hook&set-captain-local-vars ()
  (setq-local captain-predicate #'oo-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'oo--prog-mode-should-capitalize-p))
;;; provide
(provide 'oo-after-load-captain)
;;; oo-after-load-captain.el ends here
