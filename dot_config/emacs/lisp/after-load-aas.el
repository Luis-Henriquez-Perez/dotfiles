;;; after-load-aas.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; At first glance I thought that aas was obviously better than abbrev because
;; it expands.  However, looking at the issue more closely makes it more and
;; more unclear.
;;
;; A discussion of the subtle differences between abbrev and `aas':
;;
;; 1. overriding snippets
;; A consequence of expanding snippets immediately is that they can override
;; another snippet.  For instance, using abbrev you could have BLANK1 and BLANK2
;; be non-overlapping snippets, but with `aas' BLANK1 would shadow BLANK2.  You
;; would type in ";" and it would immediately expand into ";; ".  Whereas with
;; abbrev, the expansion is triggered by a space, thereby giving you a way to
;; differentiate the two.
;;
;; 2. residue space
;; When using abbrev you end up with a residue space when
;; expanding snippets.  I am sure it is not hard to fix this but it does suggest
;; that using `aas' can be more convenient in at least some cases.  Since the
;; snippet would expand immediately.
;;
;; 3. not using regexps
;; One gotcha is that `aas' does *not* use regexps to match the previous thing,
;; it builds.  In practice this is a trade-off.
;;
;; 4.
;; You cannot write the same kind of abbrevs.
;;
;; TODO: find a use-case for aas in combination with abbrev
;; One thing you can do is use aas in combination with abbrev.  You can use
;; abbrev to handle most of the abbrevs.  One idea is to use it in cases where a
;; space might interfere with the abbrev itself.
;;
;;; Code:
(require '04-base-custom)
;;;; replace two spaces with a period
(defun oo--aas-text-enable-p ()
  (and (or (derived-mode-p 'text-mode)
           (oo--in-string-or-comment-p))
       (not (minibuffer-window-active-p (minibuffer-window)))))

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "\s\s" ".\s\s")

;; (setup! :snippet "asap" "A.S.A.P")
(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "imd" "immediately")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "asap" "A.S.A.P.")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "dsl" "Domain-Specific-Language")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "fn" "function")

;; "fns" "functions"

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ig" "instagram")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "econf" "emacs configuration")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "kbdm" "keyboard macro")

;; "kbdms" "keyboard macros"

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "msw" "Microsoft Windows")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p ".mail" "luis@luishp.xyz")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "imho" "in my humble opinion")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "imo" "in my opinion")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "fyi" "for your information")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "lmk" "let me know")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "afaik" "as far as I know")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "idk" "I do not know")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "gonna" "going to")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "otc" "on the contrary")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "st" "sometimes")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "tbh" "to be honest")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "qwerty" "QWERTY")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ngl" "not going to lie")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "tfb" "to be frank")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ndo" "window")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "tis" "it is")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "amly" "automatically")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "rn" "right now")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "bly" "probably")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "lk" "like")

;; I want the word =config= to stay the same too much for this to be useful.
;; Right now it is frankly more of an impediment for me than something that
;; helps me.
(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "config" "configuration")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "obv" "obviously")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "appr" "appropriate")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ofc" "of course")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ok" "O.K.")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "iis" "it's")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "bc" "because")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "iow" "in other words")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "uun" "up until now")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "exwm" "EXWM")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "fe" "for example")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "evaled" "evaluated")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "imma" "I am going to")

;; From https://sachachua.com/blog/2015/01/developing-emacs-micro-habits-text-automation/

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "hw" "however")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "otoh" "on the other hand")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ohter" "other")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "fe" "for example")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "fi" "for instance")

;;;; emacs-lisp-mode
;; Should make this the first non-whitespace.
;; (aas-set-snippets 'emacs-lisp-mode :cond #'bolp ";" ";;\s")
(aas-set-snippets 'emacs-lisp-mode ".fun" (-partial #'tempel-insert 'fun))
(aas-set-snippets 'emacs-lisp-mode ".let" (lambda () (tempel-insert 'let) (evil-insert-state 1)))
;;;; global snippet
(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "youve" "you have")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "youd" "you would")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ill" "I will")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "theres" "there is")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "itss" "it is")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "im" "I am")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "cant" "cannot")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "idd" "I would")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "dont" "do not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "wontt" "will not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "doesnt" "does not")

;; Not sure I actually want this abbrev because =wont= is actually a word.
;; "wont" nil)

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "thats" "that is")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "\su\s" "\syou\s")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "\sul\s" "\syou'll\s")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "dont" "do not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "\sdnt\s" "don't")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ive" "I have")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "dstr" "doc-string")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "dstrs" "doc-strings")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ik" "I know")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ribe" "describe")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "iff" "if and only if")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "whats" "what is")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "havent" "have not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "didnt" "did not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "shouldnt" "should not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "isnt" "is not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "youre" "you are")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "wouldnt" "would not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "woudnt" "would not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "coudnt" "could not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "couldnt" "could not")
;; ;;;; typos
;; ;; These abbrevs are focused on spelling mistakes.
;; ;; Here I focus on fixing unambiguous spelling mistakes.

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "abbev" "abbrev")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "onw" "own")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "frst" "first")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "edting" "editing")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "alread" "already")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "htats" "that is")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "whehther" "whether")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "somehting" "something")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "hte" "the")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "te" "the")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "th" "the")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "ot" "to")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "wnat" "want")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "stoped" "stopped")

;; ;; Man, `completion-at-point-functions' is such a long variable name huh?
;; ;; I definitely do not recommend writing all that out yourself.
(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "capfs" "completion-at-point-functions")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "suprise" "surprise")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "functoin" "function")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "refect" "reflect")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "dint" "did not")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "actioin" "action")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "actioins" "actions")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "orignal" "original")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "eachother" "each other")

(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "propogate" "propagate")
(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "pakcage" "package")
(aas-set-snippets 'global :cond #'oo--aas-text-enable-p "pakcages" "packages")
;;; provide
(provide 'after-load-aas)
;;; after-load-aas.el ends here
