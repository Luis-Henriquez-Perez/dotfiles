;;; 99-after-load-abbrev.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;;; only use global abbrevs
;; Use only global abbrevs.  At first I had tried using mode-specific abbrevs,
;; but I encounted problems.  I found it much easier to just make them all
;; global abbrevs and to specify an "enable-function" if I want to be
;; conditional based on the mode.
(opt! only-global-abbrevs t)
;;;;; do not save abbrevs to a file (use this file instead)
;; Do not write/read abbrevs from a file.  I would rather just define them
;; here than to save them in the abbrev file.
;; It is more consistent with my config that way.  I especially do not want two
;; different files that code for the same thing.
(opt! save-abbrevs nil)
(advice-add #'read-abbrev-file :around #'ignore)
(advice-add #'write-abbrev-file :around #'ignore)
(advice-add #'abbrev--possibly-save :around #'ignore)
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
    (unless (not (oo-enable-global-abbrev-p))
      (set! eol (line-beginning-position -1))
      (set! rx "\\([[:word:]]\\)\\([[:space:]][[:space:]]\\)\\([^[:space:]]+\\)")
      (cond ((looking-back rx eol)
             (replace-match "\\1.\\2\\3" nil nil nil 0))
            ((looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" eol)
             (replace-match "\\1."))))))
;;;;; emacs-lisp-mode
;;;;;; callable names
(defun oo-tempel-abbrev-def (name)
  "Return a function that expands.."
  (let ((template (cdr (assoc name (tempel--templates))))
        (hook (make-symbol (format "tempel--%s-snippet" name))))
    (fset hook (apply-partially #'tempel--abbrev-hook (symbol-name name) template))
    (put hook 'no-self-insert t)
    hook))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  (list (list ".fun"   "" (oo-tempel-abbrev-def 'fun))
        (list ".let"   "" (oo-tempel-abbrev-def 'let))
        (list ".p"     "" (oo-tempel-abbrev-def 'pt))
        (list ".var"   "" (oo-tempel-abbrev-def 'var))
        (list ".al"    "" (oo-tempel-abbrev-def 'alias))
        (list ".alias" "" (oo-tempel-abbrev-def 'alias))))

(abbrev-table-put emacs-lisp-mode-abbrev-table :regexp "\\(?:^\\|[	 ]+\\)\\(?1:\\..*\\|.*\\)")
;;;;; eshell
;; (defun oo--enable-eshell-mode-abbrev-p ()
;;   "Return non-nil if elisp mode abbrev should be enabled."
;;   (equal major-mode 'eshell-mode))

;; ;; (define-abbrev global-abbrev-table ".edir" "~/.config/emacs/" :enable-function #'oo--enable-eshell-mode-abbrev-p)
;;;;; all abbrevs
(abbrev-table-put global-abbrev-table :enable-function #'oo-enable-global-abbrev-p)
(abbrev-table-put oo-wikipedia-misspellings-table :enable-function #'oo-enable-global-abbrev-p)
;; Technically I should insert the parents "safetly" by checking for existing parents.
(alet (-snoc (abbrev-table-get global-abbrev-table :parents) oo-wikipedia-misspellings-table)
  (abbrev-table-put global-abbrev-table :parents it))
;;; provide
(provide '99-after-load-abbrev)
;;; 99-after-load-abbrev.el ends here
