;;; 20-config-abbrev.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require '04-base-custom)
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
(advice-add #'read-abbrev-file :around #'ignore)
(advice-add #'write-abbrev-file :around #'ignore)
(advice-add #'abbrev--possibly-save :around #'ignore)
;;;;; set abbrevs my way
;; Only expand abbreviations in prog-mode string or comments.  Otherwise, they
;; could interfere with function names.
;; This is meant for use
(defun oo--abbrev-enable-fn ()
  "Return non-nil if abbrevs should be enabled."
  (or (not (derived-mode-p 'prog-mode))
      (oo--in-string-or-comment-p)))
;;;;; extend abbrev syntax
;; Allow the use of periods, colons, and underscores in global abbrevs.  The
;; point of doing this is to let me name certain abbrevs with easy to remember,
;; intuitive names while also preventing name clashes with the preceding
;; punctuation.
(abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[[:space:]]\\)\\(?1:[.:_]?[[:alpha:]]+\\)")
;;;;; general
;; Most often, I want the abbrevs I define to be expanded in either plain text
;; or in programming language comments.
(defun oo-text-abbrev (abbrev expansion)
  "Define an abbreviation."
  (define-abbrev global-abbrev-table abbrev expansion nil :enable-function #'oo--abbrev-enable-fn))
;;;;; TODO: deal with problem of non-capitalization of mutliple words
;; When an abbrev expands to multiple words the initial word does not get
;; capitalized with captain.  But it does work when abbrev expands to just one
;; word.  So the first question is how to go about solving this problem.  As is
;; the case in emacs, there are multiple ways.  One way is changing the value
;; of.  The other way is using a hook for a multi-word expansion.  The hook would.
;;;;; TODO: title this heading properly
(oo-text-abbrev ".name" "Luis Henriquez-Perez")
(defun oo--insert-time (format-string &optional timezone)
  (insert (format-time-string format-string timezone)))
(define-abbrev global-abbrev-table ".year" "" (-partial #'oo--insert-time "%Y"))
(define-abbrev global-abbrev-table ".monthday" "" (-partial #'oo--insert-time "%d"))
(define-abbrev global-abbrev-table ".mday" "" (-partial #'oo--insert-time "%d"))
(define-abbrev global-abbrev-table ".month" "" (-partial #'oo--insert-time "%m"))
(define-abbrev global-abbrev-table ".minute" "" (-partial #'oo--insert-time "%M"))
(define-abbrev global-abbrev-table ".sec" "" (-partial #'oo--insert-time "%S"))
(define-abbrev global-abbrev-table ".second" "" (-partial #'oo--insert-time "%S"))
;;;;; capitalize
(oo-text-abbrev "i" "I")

(oo-text-abbrev "luis" "Luis")
;;;;; abbrevs
(oo-text-abbrev ".mail" "luis@luishp.xyz")

(oo-text-abbrev "imho" "in my humble opinion")

(oo-text-abbrev "imo" "in my opinion")

(oo-text-abbrev "fyi" "for your information")

(oo-text-abbrev "lmk" "let me know")

(oo-text-abbrev "afaik" "as far as I know")

(oo-text-abbrev "idk" "I do not know")

(oo-text-abbrev "gonna" "going to")

(oo-text-abbrev "otc" "on the contrary")

(oo-text-abbrev "st" "sometimes")

(oo-text-abbrev "tbh" "to be honest")

(oo-text-abbrev "qwerty" "QWERTY")

(oo-text-abbrev "ngl" "not going to lie")

(oo-text-abbrev "tfb" "to be frank")

(oo-text-abbrev "ndo" "window")

(oo-text-abbrev "tis" "it is")

(oo-text-abbrev "amly" "automatically")

(oo-text-abbrev "rn" "right now")

(oo-text-abbrev "bly" "probably")

(oo-text-abbrev "lk" "like")

;; I want the word =config= to stay the same too much for this to be useful.
;; Right now it is frankly more of an impediment for me than something that
;; helps me.
;; (oo-text-abbrev "config" "configuration")
(oo-text-abbrev "obv" "obviously")

(oo-text-abbrev "appr" "appropriate")

(oo-text-abbrev "ofc" "of course")

(oo-text-abbrev "ok" "O.K.")

(oo-text-abbrev "iis" "it's")

(oo-text-abbrev "bc" "because")

(oo-text-abbrev "iow" "in other words")

(oo-text-abbrev "uun" "up until now")

(oo-text-abbrev "exwm" "EXWM")

(oo-text-abbrev "fe" "for example")

(oo-text-abbrev "evaled" "evaluated")

(oo-text-abbrev "imma" "I am going to")

;; From https://sachachua.com/blog/2015/01/developing-emacs-micro-habits-text-automation/

(oo-text-abbrev "hw" "however")

(oo-text-abbrev "otoh" "on the other hand")

(oo-text-abbrev "ohter" "other")

(oo-text-abbrev "fe" "for example")

(oo-text-abbrev "fi" "for instance")
;;;;; expand common abbreviations in english
;; Should I never use abbreviations.

(oo-text-abbrev "youve" "you have")

(oo-text-abbrev "youd" "you would")

(oo-text-abbrev "il" "I will")

(oo-text-abbrev "theres" "there is")

(oo-text-abbrev "itss" "it is")

(oo-text-abbrev "im" "I am")

(oo-text-abbrev "illl" "I will")

(oo-text-abbrev "cant" "cannot")

(oo-text-abbrev "idd" "I would")

(oo-text-abbrev "dont" "do not")

(oo-text-abbrev "wontt" "will not")

(oo-text-abbrev "doesnt" "does not")

;; Not sure I actually want this abbrev because =wont= is actually a word. 
;; (oo-text-abbrev "wont" nil)

(oo-text-abbrev "thats" "that is")

(oo-text-abbrev "u" "you")

(oo-text-abbrev "ul" "you'll")

(oo-text-abbrev "dont" "do not")

(oo-text-abbrev "dnt" "don't")

(oo-text-abbrev "ive" "I have")

(oo-text-abbrev "dstr" "doc-string")
(oo-text-abbrev "dstrs" "doc-strings")

(oo-text-abbrev "ik" "I know")

(oo-text-abbrev "ribe" "describe")

(oo-text-abbrev "iff" "if and only if")

(oo-text-abbrev "whats" "what is")

(oo-text-abbrev "havent" "have not")

(oo-text-abbrev "didnt" "did not")

(oo-text-abbrev "shouldnt" "should not")

(oo-text-abbrev "isnt" "is not")

(oo-text-abbrev "youre" "you are")

(oo-text-abbrev "wouldnt" "would not")

(oo-text-abbrev "woudnt" "would not")

(oo-text-abbrev "coudnt" "could not")

(oo-text-abbrev "couldnt" "could not")
;;;;; spelling mistakes
;; These abbrevs are focused on spelling mistakes.
;; Here I focus on fixing unambiguous spelling mistakes.
(oo-text-abbrev "frst" "first")

(oo-text-abbrev "edting" "editing")

(oo-text-abbrev "alread" "already")

(oo-text-abbrev "htats" "that is")

(oo-text-abbrev "whehther" "whether")

(oo-text-abbrev "somehting" "something")

(oo-text-abbrev "hte" "the")

(oo-text-abbrev "te" "the")

(oo-text-abbrev "th" "the")

(oo-text-abbrev "ot" "to")

(oo-text-abbrev "wnat" "want")

(oo-text-abbrev "stoped" "stopped")

;; Man, `completion-at-point-functions' is such a long variable name huh?
;; I definitely do not recommend writing all that out yourself.
(oo-text-abbrev "capfs" "completion-at-point-functions")

(oo-text-abbrev "suprise" "surprise")

(oo-text-abbrev "functoin" "function")

(oo-text-abbrev "refect" "reflect")

(oo-text-abbrev "dint" "did not")

(oo-text-abbrev "actioin" "action")

(oo-text-abbrev "actioins" "actions")

(oo-text-abbrev "orignal" "original")

(oo-text-abbrev "eachother" "each other")

(oo-text-abbrev "propogate" "propagate")
(oo-text-abbrev "pakcage" "package")
(oo-text-abbrev "pakcages" "packages")
;;;;; emacs-lisp-mode
;; Problems.
;; 1. the template starts with a space
;; 2. I am having trouble with the keybindings
(alet (lambda () (and (equal major-mode 'emacs-lisp-mode)
                      (not (oo--in-string-or-comment-p))))
  (define-abbrev global-abbrev-table ".let" "" (lambda () (tempel-insert 'let) (evil-insert-state 1)) :enable-function it)
  (define-abbrev global-abbrev-table ".fun" "" (-partial #'tempel-insert 'fun) :enable-function it))
;;; provide
(provide '20-config-abbrev)
;;; 20-config-abbrev.el ends here
