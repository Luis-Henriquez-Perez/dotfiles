;;; oo-abbrev-table-main.el -*- lexical-binding: t; -*-
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
;;  Define my main abbrev table, `oo-abbrev-table-main'.
;;
;;; Code:
(require 'abbrev)

(define-abbrev-table 'oo-abbrev-table-main
  '(("msg" "message")
    ("mesage" "message")
    ("sems" "seems")
    ("rem" "remove")
    ("bod" "body")
    ("defing" "defining")
    ("awkard" "akward")
    ("recos" "recommends")
    ("binds" "bindings")
    ("nd" "and")
    ("approp" "appropriate")
    ("aprop" "appropriate")
    ("lexi" "lexicographically")
    ("econfig" "Emacs configuration")
    ("consec" "consecutive")
    ("conseq" "consequence")
    ("paces" "spaces")
    ("beh" "behavior")
    ("comnt" "comment")
    ("keybind" "keybinding")
    ("kbind" "keybinding")
    ("keybinds" "keybindings")
    ("kbinds" "keybindings")
    ("abbrevs" "abbreviations")
    ("abbrev" "abbreviation")
    ("bcomp" "byte-compilation")
    ("unness" "unnecessary")
    ("coment" "comment")
    ("fror" "for")
    ("stup" "setup")
    ("inclinaition" "inclination")
    ("tring" "trying")
    ("wanna" "want to")
    ("effor" "effort")
    ("undef" "undefined")
    ("sym" "symbol")
    ("wre" "were")
    ("ws" "was")
    ("probs" "problems")
    ("syms" "symbols")
    ("tbs" "that being said")
    ("evalu" "evaluation")
    ("oside" "outside")
    ("bside" "beside")
    ("sumes" "assumes")
    ("evaling" "evaluating")
    ("res" "result")
    ("src" "source")
    ("sytle" "style")
    ("elt" "element")
    ("elts" "elements")
    ("gens" "generates")
    ("understandibly" "understandably")
    ("subborn" "stubborn")
    ("dever" "developer")
    ("html" "HTML")
    ("css" "CSS")
    ("dev" "development")
    ("mgiht" "might")
    ("despa" "desparate")
    ("prog" "programming")
    ("tti" "the thing is")
    ("paht" "path")
    ("val" "value")
    ("optioinal" "optional")
    ("homogenous" "homogeneous")
    ("awa" "as well as")
    ("simul" "simultaneously")
    ("simultanouesly" "simultaneously")
    ("struct" "structure")
    ("structs" "structures")
    ("acces" "access")
    ("comm" "common")
    ("corr" "corresponding")
    ("expr" "expression")
    ("builtin" "built-in")
    ("elems" "elements")
    ("elem" "element")
    ("nones" "nonessential")
    ("abt" "about")
    ("yt" "youtube")
    ("iadt" "in addition to this")
    ("tldr" "TLDR")
    ("mo" "more")
    ("opt" "option")
    ("thro" "through")
    ("alowed" "allowed")
    ("allowd" "allowed")
    ("pefect" "perfect")
    ("negy" "negatively")
    ("neg" "negative")
    ("eff" "effect")
    ("weras" "whereas")
    ("fol" "following")
    ("foll" "following")
    ("ooh" "out of hand")
    ("combi" "combination")
    ("ingen" "in general")
    ("gened" "generated")
    ("aload" "autoload")
    ("amt" "amount")
    ("amoutn" "amount")
    ("lvl" "level")
    ("cd" "could")
    ("cud" "could")
    ("infor" "information")
    ("isn" "is not")
    ("thhe" "the")
    ("ques" "question")
    ("arg" "argument")
    ("equiv" "equivalent")
    ("mmend" "recommend")
    ("del" "delete")
    ("desc" "description")
    ("hes" "he is")
    ("impressoin" "impression")
    ("n" "and")
    ("wsa" "was")
    ("var" "variable")
    ("vars" "variables")
    ("macroexp" "macro expansion")
    ("coments" "comments")
    ("unes" "unnecessary")
    ("uness" "unnecessary")
    ("dk" "do not know")
    ("emacss" "Emacs's")
    ("ess" "essentially")
    ("sist" "consistent")
    ("initing" "initializing")
    ("fnames" "filenames")
    ("misc" "miscellaneous")
    ("imm" "immediately")
    ("evv" "everything")
    ("abut" "about")
    ("movo" "moreover")
    ("dandylion" "dandelion")
    ("dandylions" "dandelions")
    ("bev" "bird's-eye view")
    ("thot" "thought")
    ("tmmw" "to make matters worse")
    ("freq" "frequency")
    ("pertubed" "perturbed")
    ("si" "is")
    ("vs" "versus")
    ("mor" "more")
    ("configing" "configuring")
    ("aume" "as you might expect")
    ("ayme" "as you might expect")
    ("tss" "timestamps")
    ("ts" "timestamp")
    ("obj" "object")
    ("oop" "object-oriented-programming")
    ("ood" "object-oriented-design")
    ("hwat" "what")
    ("sep" "separate")
    ("utube" "youtube")
    ("hav" "have")
    ("ootq" "out of the question")
    ("tte" "to this end")
    ("wasnt" "was not")
    ("ult" "ultimate")
    ("ultl" "ultimately")
    ("aia" "all in all")
    ("soly" "solely")
    ("idwt" "I do not want to")
    ("coll" "collection")
    ("hadnt" "had not")
    ("suces" "success")
    ("sucess" "success")
    ("accros" "across")
    ("resonable" "reasonable")
    ("sec" "second")
    ("secs" "seconds")
    ("reco" "recommended")
    ("sth" "something")
    ("ath" "anything")
    ("corsor" "cursor")
    ("sdb" "should not be")
    ("rihgt" "right")
    ("shdnt" "should not")
    ("aycs" "as you can see")
    ("twisi" "the way I see it")
    ("fnr" "for no reason")
    ("mk" "make")
    ("prases" "phrases")
    ("thse" "these")
    ("lang" "language")
    ("english" "English")
    ("nuf" "enough")
    ("idek" "I did not even know")
    ("cur" "current")
    ("curr" "current")
    ("onl" "only")
    ("uve" "you have")
    ("revaled" "re-evaluated")
    ("idid" "I did")
    ("tres" "interesting")
    ("ml" "modeline")
    ;; ("def" "definitely")
    ("docs" "documentation")
    ("doc" "documentation")
    ("nfo" "information")
    ("info" "information")
    ("correclty" "correctly")
    ("coudl" "could")
    ("oofone" "O(1)")
    ("iml" "immediately")
    ("theyre" "they are")
    ("punc" "punctuation")
    ("dling" "downloading")
    ("ez" "easy")
    ("fol" "follow")
    ("popl" "populate")
    ("als" "autoloads")
    ("al" "autoload")
    ("ootw" "out of the way")
    ("gen" "generate")
    ("aloads" "autoloads")
    ("alods" "autoloads")
    ("dls" "downloads")
    ("lize" "realize")
    ("hwo" "how")
    ("tbe" "to be exact")
    ("chars" "characters")
    ("heres" "here is")
    ("hesi" "hesitant")
    ("ex" "example")
    ("xample" "example")
    ("cna" "can")
    ("ord" "order")
    ("cate" "category")
    ("nyt" "anything")
    ("mena" "mean")
    ("doin" "doing")
    ("na" "an")
    ("somethig" "something")
    ("iac" "in any case")
    ("di" "do it")
    ("intro" "introduction")
    ("tha" "that")
    ("char" "character")
    ("wt" "want to")
    ("hava" "have a")
    ("deflt" "default")
    ("fname" "filename")
    ("ta" "that")
    ("itl" "it will")
    ("itll" "it will")
    ("mott" "most of the time")
    ("sd" "should")
    ("shd" "should")
    ("emacs" "Emacs")
    ("alot" "a lot")
    ("moro" "moreover")
    ("orig" "original")
    ("iat" "in addition to")
    ("elisp" "emacs-lisp")
    ("idl" "I do not like")
    ("y" "why")
    ("r" "are")
    ;; "id" is actually a word, but I use it so infrequently and I use the phrase I
    ;; would so frequently that it is worth having it be an abbrev for I.
    ;; ("wm" "which")
    ("prec" "precedence")
    ("wdimbt" "what do I mean by this")
    ("wdim" "what do I mean")
    ("awn" "also worth noting")
    ("int" "I need to")
    ("wch" "which")
    ("ch" "which")
    ("igts" "I have got to say")
    ("ihtbs" "it has to be said")
    ("wd" "would")
    ("freql" "frequently")
    ("ifreql" "infrequently")
    ("infreql" "infrequently")
    ("id" "ID")
    ("wan" "want")
    ("idht" "I do not have to")
    ("hvae" "have")
    ("cann" "cannot")
    ("icj" "I can just")
    ("ic" "I can")
    ("ure" "you are")
    ("lhp" "Luis Henriquez-Perez")
    ("noly" "not only")
    ("specy" "specifically")
    ("rl" "really")
    ("args" "arguments")
    ("ppl" "people")
    ("o" "of")
    ("urself" "yourself")
    ("ab" "about")
    ("ret" "return")
    ("tn" "then")
    ("blv" "believe")
    ("nto" "not")
    ("mone" "money")
    ("hve" "have")
    ("m" "my")
    ("swich" "switch")
    ("actuall" "actually")
    ("evalt" "evaluate")
    ("asap" "A.S.A.P.")
    ("econf" "emacs configuration")
    ("alth" "although")
    ("esp" "especially")
    ;; This clashes with the word "ill" but I barely use that word.  In the rare
    ;; event where I do need to use it I will unexpand it.
    ("ill" "I will")
    ;; This is a common abbrev for "by the way" but I feel like I do not really use
    ;; that phrase unless I am texting.
    ("imd" "immediately")
    ("exand" "expand")
    ("ppet" "snippet")
    ("tse" "these")
    ("usu" "usually")
    ("iprac" "in practice")
    ("efy" "efficiency")
    ("evr" "everything")
    ("sen" "seen")
    ("wat" "what")
    ("dfl" "default")
    ("dflt" "default")
    ("difr" "differ")
    ("btw" "between")
    ("dif" "difference")
    ("diff" "difference")
    ("htat" "that")
    ("rnt" "are not")
    ("arent" "are not")
    ("tt" "the")
    ("len" "length")
    ("bf" "before")
    ("af" "after")
    ("oft" "often")
    ("dsnt" "does not")
    ("ihb" "I had been")
    ("comptia" "CompTIA")
    ("gt" "get")
    ("wev" "whatever")
    ("jbos" "jobs")
    ("theyll" "they will")
    ("prevl" "previously")
    ("prev" "previous")
    ("ivs" "I have seen")
    ("rxp" "regular expression")
    ("rx" "regular expression")
    ("dl" "download")
    ("vc" "version control")
    ("wil" "will")
    ("shiftk" "SHIFT")
    ("nots" "number of times")
    ("num" "number")
    ("f" "of")
    ("lins" "lines")
    ;; I thought that this could be a bad abbrev because it is too much line a
    ;; spelling mistake of =the=, but then I thought if I do misspell "the" it is
    ;; very unlikely I will do so by pressing =t= again.
    ("tht" "thought")
    ("i" "I")
    ("luis" "Luis")
    ;; This will not work because ";" is not a work constituent.  I need to use
    ;; `aas' for this or come up with some other solution.
    ;; (";;" ".")
    ;; TODO: move to spell-fixing abbrev.
    ("ivb" "I have been")
    ("werent" "were not")
    ("rly" "really")
    ("aagp" "at any given point")
    ("begn" "beginning")
    ("idt" "I do not think")
    ("arent" "are not")
    ("ting" "thing")
    ("ned" "need")
    ("dir" "directory")
    ("ull" "you will")
    ("spst" "supposed")
    ("hv" "have")
    ("desiding" "deciding")
    ("pkgs" "packages")
    ("pkg" "package")
    ("ftmp" "for the most part")
    ("readme" "README")
    ("sdnt" "should not")
    ("cme" "come")
    ("imed" "immediately")
    ("cesly" "successfully")
    ("owoto" "one way or the other")
    ("configs" "configurations")
    ("espace" "escape")
    ("ijwi" "I just want it")
    ("ijw" "I just want")
    ("idw" "I do not want")
    ("ij" "I just")
    ("unesy" "unnecessarily")
    ("unes" "unnecessary")
    ("nes" "necessary")
    ("bec" "because")
    ("aswer" "answer")
    ("dlk" "do not like")
    ("tho" "though")
    ("idkw" "I do not know why")
    ("damw" "do not ask me why")
    ("idlk" "I do not like")
    ("ilk" "I like")
    ("auly" "automatically")
    ("dsl" "Domain-Specific-Language")
    ("fn" "function")
    ("fns" "functions")
    ("ig" "instagram")
    ("econf" "emacs configuration")
    ("kbdm" "keyboard macro")
    ("kbdms" "keyboard macros")
    ("msw" "Microsoft Windows")
    (".mail" "luis@luishp.xyz")
    ("imho" "in my humble opinion")
    ("imo" "in my opinion")
    ("fyi" "for your information")
    ("lmk" "let me know")
    ("afaik" "as far as I know")
    ("idk" "I do not know")
    ("gonna" "going to")
    ("otc" "on the contrary")
    ("st" "sometimes")
    ("tbh" "to be honest")
    ("qwerty" "QWERTY")
    ("ngl" "not going to lie")
    ("tfb" "to be frank")
    ("ndo" "window")
    ("tis" "it is")
    ("amly" "automatically")
    ("rn" "right now")
    ("bly" "probably")
    ("lk" "like")
    ;; I want the word =config= to stay the same too much for this to be useful.
    ;; Right now it is frankly more of an impediment for me than something that
    ;; helps me.
    ("questioin" "question")
    ("ur" "your")
    ("wiht" "with")
    ("ith" "with")
    ("abilit" "ability")
    ("ko" "K.O.")
    ("dat" "that")
    ("incc" "increase")
    ("incs" "increase")
    ("decs" "deccrease")
    ;; TODO: Add a condition preventng it from expanding if I am writing a file path.
    ("config" "configuration")
    ("obv" "obviously")
    ("appr" "appropriate")
    ("ofc" "of course")
    ("ok" "O.K.")
    ("iis" "it's")
    ("bc" "because")
    ("iow" "in other words")
    ("uun" "up until now")
    ("exwm" "EXWM")
    ("fe" "for example")
    ("evaled" "evaluated")
    ("imma" "I am going to")
    ;; From https://sachachua.com/blog/2015/01/developing-emacs-micro-habits-text-automation/
    ("iwt" "I want to")
    ("hw" "however")
    ("ths" "this")
    ("otoh" "on the one hand")
    ("otth" "on the other hand")
    ("kinda" "kind of")
    ("ohter" "other")
    ("fe" "for example")
    ("fi" "for instance")
    ("youve" "you have")
    ("youd" "you would")
    ("il" "I will")
    ("theres" "there is")
    ("itss" "it is")
    ("im" "I am")
    ("illl" "I will")
    ("cant" "cannot")
    ("idd" "I would")
    ("dont" "do not")
    ("wontt" "will not")
    ("doesnt" "does not")
    ;; Not sure I actually want this abbrev because =wont= is actually a word.
    ("ine" "one")
    ("mispelling" "misspelling")
    ("ry" "very")
    ("wont" "will not")
    ("thats" "that is")
    ("u" "you")
    ("ul" "you'll")
    ("dont" "do not")
    ("dnt" "don't")
    ("wo" "without")
    ("ive" "I have")
    ("ik" "I know")
    ("plx" "complex")
    ("ribe" "describe")
    ("iff" "if and only if")
    ("addd" "additionally")
    ("weve" "we have")
    ("whats" "what is")
    ("havent" "have not")
    ("didnt" "did not")
    ("shouldnt" "should not")
    ("isnt" "is not")
    ("fo" "for")
    ("ud" "you would")
    ("fwis" "from what I see")
    ("fwiu" "from what I understand")
    ("youre" "you are")
    ("wouldnt" "would not")
    ("woudnt" "would not")
    ("coudnt" "could not")
    ("couldnt" "could not")
    ;; These abbrevs are focused on spelling mistakes.
    ;; Here I focus on fixing unambiguous spelling mistakes.
    ("onw" "own")
    ("frst" "first")
    ("edting" "editing")
    ("alread" "already")
    ("htats" "that is")
    ("whehther" "whether")
    ("somehting" "something")
    ("hte" "the")
    ("te" "the")
    ("th" "the")
    ("ot" "to")
    ("wnat" "want")
    ("stoped" "stopped")
    ("capfs" "completion-at-point-functions")
    ("suprise" "surprise")
    ("functoin" "function")
    ("refect" "reflect")
    ("dint" "did not")
    ("orignal" "original")
    ("eachother" "each other")
    ("dn" "do not")
    ("dsn" "does not")
    ("fsr" "for some reason")
    ("propogate" "propagate")
    ("pakcage" "package")
    ("pakcages" "packages")
    ("motn" "more often than not")
    ("itc" "in that case")
    ("prob" "problem")
    ("dered" "considered"))
  "My main abbrev table for English text.")
;;; provide
(provide 'oo-abbrev-table-main)
;;; oo-abbrev-table-main.el ends here
