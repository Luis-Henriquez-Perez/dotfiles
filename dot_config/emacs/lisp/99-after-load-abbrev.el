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
;; Most often, I want the abbrevs I define to be expanded in either plain text
;; or in programming language comments.
(defun oo-text-abbrev (abbrev expansion)
  "Define an abbreviation."
  (define-abbrev global-abbrev-table abbrev expansion nil :enable-function #'oo--enable-text-mode-abbrev-p))
;;;;;; set abbrevs my way
;; Only expand abbreviations in prog-mode string or comments.  Otherwise, they
;; could interfere with function names.
;; This is meant for use
(defun oo--enable-text-mode-abbrev-p ()
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
    (unless (not (oo--enable-text-mode-abbrev-p))
      (set! eol (line-beginning-position -1))
      (set! rx "\\([[:word:]]\\)\\([[:space:]][[:space:]]\\)\\([^[:space:]]+\\)")
      (cond ((looking-back rx eol)
             (replace-match "\\1.\\2\\3" nil nil nil 0))
            ((looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" eol)
             (replace-match "\\1."))))))
;;;;; emacs-lisp-mode
;;;;;; callable names
;; Function names.
(defun oo--expand-emacs-lisp-mode-funcall-abbrev-p ()
  "Return non-nil if elisp mode abbrev should be enabled."
  (and (equal major-mode 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))
       (save-match-data (looking-back (rx (or "(function\s" "#'" "(") (1+ letter) (0+ space)) 1))))

;; TODO: maybe it is better just to have these as snippets.
(define-abbrev global-abbrev-table "efn" "expand-file-name" nil :enable-function
  #'oo--expand-emacs-lisp-mode-funcall-abbrev-p)

(define-abbrev global-abbrev-table "wcb" "with-current-buffer" nil :enable-function
  #'oo--expand-emacs-lisp-mode-funcall-abbrev-p)

(defun oo--enable-emacs-lisp-mode-abbrev-p ()
  "Return non-nil if elisp mode abbrev should be enabled."
  (and (equal major-mode 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))))

;; (defalias 'oo--)
;; (defun oo--abbrev-insert-snippet (snippet-name)
;;   (tempel-insert snippet-name)
;;   ;; Delete the last space.
;;   (delete-backward-char 1)
;;   ;; Ensure we're in insert state.
;;   (when (bound-and-true-p evil-mode)
;;     (evil-insert-state 1)))
;; Problems.
;; 1. the template starts with a space
;; 2. I am having trouble with the keybindings
;; (define-abbrev global-abbrev-table ".let" "" (-partial #'oo--abbrev-insert-snippet 'let) :enable-function #'oo--enable-emacs-lisp-mode-abbrev-p)
;; (define-abbrev global-abbrev-table ".fun" "" (-partial #'oo--abbrev-insert-snippet 'fun) :enable-function #'oo--enable-emacs-lisp-mode-abbrev-p)
(define-abbrev global-abbrev-table "todo" ";; TODO:" nil :enable-function #'oo--enable-emacs-lisp-mode-abbrev-p)
;;;;; eshell
(defun oo--enable-eshell-mode-abbrev-p ()
  "Return non-nil if elisp mode abbrev should be enabled."
  (equal major-mode 'eshell-mode))

;; (define-abbrev global-abbrev-table ".edir" "~/.config/emacs/" :enable-function #'oo--enable-eshell-mode-abbrev-p)
;;;;; all abbrevs
;; (oo-text-abbrev "additionally" "additionally")
;; (oo-text-abbrev "addit" "additionally")
(oo-text-abbrev "configing" "configuring")
(oo-text-abbrev "ord" "order")
(oo-text-abbrev "cate" "category")
(oo-text-abbrev "nyt" "anything")
(oo-text-abbrev "mena" "mean")
(oo-text-abbrev "doin" "doing")
(oo-text-abbrev "na" "an")
(oo-text-abbrev "somethig" "something")
(oo-text-abbrev "iac" "in any case")
(oo-text-abbrev "di" "do it")
(oo-text-abbrev "intro" "introduction")
(oo-text-abbrev "tha" "that")
(oo-text-abbrev "char" "character")
(oo-text-abbrev "wt" "want to")
(oo-text-abbrev "hava" "have a")
(oo-text-abbrev "deflt" "default")
(oo-text-abbrev "fname" "filename")
(oo-text-abbrev "ta" "that")
(oo-text-abbrev "itl" "it will")
(oo-text-abbrev "itll" "it will")
(oo-text-abbrev "mott" "most of the time")
(oo-text-abbrev "sd" "should")
(oo-text-abbrev "shd" "should")
(oo-text-abbrev "emacs" "Emacs")
(oo-text-abbrev "alot" "a lot")
(oo-text-abbrev "moro" "moreover")
(oo-text-abbrev "orig" "original")
(oo-text-abbrev "iat" "in addition to")
(oo-text-abbrev "elisp" "emacs-lisp")
(oo-text-abbrev "idl" "I do not like")
(oo-text-abbrev "y" "why")
;; "id" is actually a word, but I use it so infrequently and I use the phrase I
;; would so frequently that it is worth having it be an abbrev for I.
;; (oo-text-abbrev "wm" "which")
(oo-text-abbrev "prec" "precedence")
(oo-text-abbrev "wdimbt" "what do I mean by this")
(oo-text-abbrev "wdim" "what do I mean")
(oo-text-abbrev "awn" "also worth noting")
(oo-text-abbrev "int" "I need to")
(oo-text-abbrev "wch" "which")
(oo-text-abbrev "ch" "which")
(oo-text-abbrev "igts" "I have got to say")
(oo-text-abbrev "ihtbs" "it has to be said")
(oo-text-abbrev "wd" "would")
(oo-text-abbrev "freql" "frequently")
(oo-text-abbrev "ifreql" "infrequently")
(oo-text-abbrev "infreql" "infrequently")
(oo-text-abbrev "id" "I would")
(oo-text-abbrev "wan" "want")
(oo-text-abbrev "idht" "I do not have to")
(oo-text-abbrev "hvae" "have")
(oo-text-abbrev "cann" "cannot")
(oo-text-abbrev "icj" "I can just")
(oo-text-abbrev "ic" "I can")
(oo-text-abbrev "ure" "you're")
(oo-text-abbrev "lhp" "Luis Henriquez-Perez")
(oo-text-abbrev "noly" "not only")
(oo-text-abbrev "specy" "specifically")
(oo-text-abbrev "rl" "really")
(oo-text-abbrev "args" "arguments")
(oo-text-abbrev "ppl" "people")
(oo-text-abbrev "o" "of")
(oo-text-abbrev "urself" "yourself")
(oo-text-abbrev "ab" "about")
(oo-text-abbrev "ret" "return")
(oo-text-abbrev "tn" "then")
(oo-text-abbrev "blv" "believe")
(oo-text-abbrev "nto" "not")
(oo-text-abbrev "mone" "money")
(oo-text-abbrev "b" "be")
(oo-text-abbrev "hve" "have")
(oo-text-abbrev "m" "my")
(oo-text-abbrev "swich" "switch")
(oo-text-abbrev "actuall" "actually")
(oo-text-abbrev "evalt" "evaluate")
(oo-text-abbrev "asap" "A.S.A.P.")
(oo-text-abbrev "econf" "emacs configuration")
(oo-text-abbrev "alth" "although")
(oo-text-abbrev "esp" "especially")
;; This clashes with the word "ill" but I barely use that word.  In the rare
;; event where I do need to use it I will unexpand it.
(oo-text-abbrev "ill" "I will")
;; This is a common abbrev for "by the way" but I feel like I do not really use
;; that phrase unless I am texting.
(oo-text-abbrev "imd" "immediately")
(oo-text-abbrev "exand" "expand")
(oo-text-abbrev "ppet" "snippet")
(oo-text-abbrev "tse" "these")
(oo-text-abbrev "usu" "usually")
(oo-text-abbrev "iprac" "in practice")
(oo-text-abbrev "efy" "efficiency")
(oo-text-abbrev "evr" "everything")
(oo-text-abbrev "sen" "seen")
(oo-text-abbrev "wat" "what")
(oo-text-abbrev "dfl" "default")
(oo-text-abbrev "dflt" "default")
(oo-text-abbrev "difr" "differ")
(oo-text-abbrev "btw" "between")
(oo-text-abbrev "dif" "difference")
(oo-text-abbrev "diff" "difference")
(oo-text-abbrev "htat" "that")
(oo-text-abbrev "rnt" "are not")
(oo-text-abbrev "arent" "are not")
(oo-text-abbrev "tt" "the")
(oo-text-abbrev "len" "length")
(oo-text-abbrev "bf" "before")
(oo-text-abbrev "af" "after")
(oo-text-abbrev "oft" "often")
(oo-text-abbrev "dsnt" "does not")
(oo-text-abbrev "ihb" "I had been")
(oo-text-abbrev "comptia" "CompTIA")
(oo-text-abbrev "gt" "get")
(oo-text-abbrev "wev" "whatever")
(oo-text-abbrev "jbos" "jobs")
(oo-text-abbrev "theyll" "they will")
(oo-text-abbrev "prevl" "previously")
(oo-text-abbrev "prev" "previous")
(oo-text-abbrev "ivs" "I have seen")
(oo-text-abbrev "rxp" "regular expression")
(oo-text-abbrev "lins" "lines")
;; I thought that this could be a bad abbrev because it is too much line a
;; spelling mistake of =the=, but then I thought if I do misspell "the" it is
;; very unlikely I will do so by pressing =t= again.
(oo-text-abbrev "tht" "thought")
(oo-text-abbrev "i" "I")
(oo-text-abbrev "luis" "Luis")
;; This will not work because ";" is not a work constituent.  I need to use
;; `aas' for this or come up with some other solution.
;; (oo-text-abbrev ";;" ".")
;; TODO: move to spell-fixing abbrev.
(oo-text-abbrev "ivb" "I have been")
(oo-text-abbrev "werent" "were not")
(oo-text-abbrev "rly" "really")
(oo-text-abbrev "aagp" "at any given point")
(oo-text-abbrev "begn" "beginning")
(oo-text-abbrev "idt" "I do not think")
(oo-text-abbrev "arent" "are not")
(oo-text-abbrev "ting" "thing")
(oo-text-abbrev "ned" "need")
(oo-text-abbrev "dir" "directory")
(oo-text-abbrev "ull" "you will")
(oo-text-abbrev "spst" "supposed")
(oo-text-abbrev "hv" "have")
(oo-text-abbrev "desiding" "deciding")
(oo-text-abbrev "pkgs" "packages")
(oo-text-abbrev "pkg" "package")
(oo-text-abbrev "ftmp" "for the most part")
(oo-text-abbrev "readme" "README")
(oo-text-abbrev "sdnt" "should not")
(oo-text-abbrev "cme" "come")
(oo-text-abbrev "imed" "immediately")
(oo-text-abbrev "cesly" "successfully")
(oo-text-abbrev "owoto" "one way or the other")
(oo-text-abbrev "configs" "configurations")
(oo-text-abbrev "espace" "escape")
(oo-text-abbrev "ijwi" "I just want it")
(oo-text-abbrev "ijw" "I just want")
(oo-text-abbrev "idw" "I do not want")
(oo-text-abbrev "ij" "I just")
(oo-text-abbrev "unesy" "unnecessarily")
(oo-text-abbrev "unes" "unnecessary")
(oo-text-abbrev "nes" "necessary")
(oo-text-abbrev "bec" "because")
(oo-text-abbrev "aswer" "answer")
(oo-text-abbrev "dlk" "do not like")
(oo-text-abbrev "tho" "though")
(oo-text-abbrev "idkw" "I do not know why")
(oo-text-abbrev "damw" "do not ask me why")
(oo-text-abbrev "idlk" "I do not like")
(oo-text-abbrev "ilk" "I like")
(oo-text-abbrev "auly" "automatically")
(oo-text-abbrev "dsl" "Domain-Specific-Language")
(oo-text-abbrev "fn" "function")
(oo-text-abbrev "fns" "functions")
(oo-text-abbrev "ig" "instagram")
(oo-text-abbrev "econf" "emacs configuration")
(oo-text-abbrev "kbdm" "keyboard macro")
(oo-text-abbrev "kbdms" "keyboard macros")
(oo-text-abbrev "msw" "Microsoft Windows")
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
(oo-text-abbrev "questioin" "question")
(oo-text-abbrev "ur" "your")
(oo-text-abbrev "wiht" "with")
(oo-text-abbrev "ith" "with")
(oo-text-abbrev "abilit" "ability")
(oo-text-abbrev "ko" "K.O.")
(oo-text-abbrev "dat" "that")
(oo-text-abbrev "incc" "increase")
(oo-text-abbrev "incs" "increase")
(oo-text-abbrev "decs" "deccrease")
;; TODO: Add a condition preventng it from expanding if I am writing a file path.
(oo-text-abbrev "config" "configuration")
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
(oo-text-abbrev "iwt" "I want to")
(oo-text-abbrev "hw" "however")
(oo-text-abbrev "ths" "this")
(oo-text-abbrev "otoh" "on the one hand")
(oo-text-abbrev "otth" "on the other hand")
(oo-text-abbrev "kinda" "kind of")
(oo-text-abbrev "ohter" "other")
(oo-text-abbrev "fe" "for example")
(oo-text-abbrev "fi" "for instance")
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
(oo-text-abbrev "ine" "one")
(oo-text-abbrev "mispelling" "misspelling")
(oo-text-abbrev "ry" "very")
(oo-text-abbrev "wont" "will not")
(oo-text-abbrev "thats" "that is")
(oo-text-abbrev "u" "you")
(oo-text-abbrev "ul" "you'll")
(oo-text-abbrev "dont" "do not")
(oo-text-abbrev "dnt" "don't")
(oo-text-abbrev "wo" "without")
(oo-text-abbrev "ive" "I have")
(oo-text-abbrev "ik" "I know")
(oo-text-abbrev "plx" "complex")
(oo-text-abbrev "ribe" "describe")
(oo-text-abbrev "iff" "if and only if")
(oo-text-abbrev "addd" "additionally")
(oo-text-abbrev "weve" "we have")
(oo-text-abbrev "whats" "what is")
(oo-text-abbrev "havent" "have not")
(oo-text-abbrev "didnt" "did not")
(oo-text-abbrev "shouldnt" "should not")
(oo-text-abbrev "isnt" "is not")
(oo-text-abbrev "fo" "for")
(oo-text-abbrev "ud" "you would")
(oo-text-abbrev "fwis" "from what I see")
(oo-text-abbrev "fwiu" "from what I understand")
(oo-text-abbrev "youre" "you are")
(oo-text-abbrev "wouldnt" "would not")
(oo-text-abbrev "woudnt" "would not")
(oo-text-abbrev "coudnt" "could not")
(oo-text-abbrev "couldnt" "could not")
;; These abbrevs are focused on spelling mistakes.
;; Here I focus on fixing unambiguous spelling mistakes.
(oo-text-abbrev "onw" "own")
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
(oo-text-abbrev "dn" "do not")
(oo-text-abbrev "dsn" "does not")
(oo-text-abbrev "fsr" "for some reason")
(oo-text-abbrev "propogate" "propagate")
(oo-text-abbrev "pakcage" "package")
(oo-text-abbrev "pakcages" "packages")
(oo-text-abbrev "motn" "more often than not")
(oo-text-abbrev "itc" "in that case")
(oo-text-abbrev "prob" "problem")
(oo-text-abbrev "dered" "considered")
;;; provide
(provide '99-after-load-abbrev)
;;; 99-after-load-abbrev.el ends here
