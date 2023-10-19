(provide 'oo-abbrevs)

;; *** abbrevs
;; :PROPERTIES:
;; :ID:       20230802T172218.869756
;; :END:
;; The abbrev system for emacs.
;; **** don't save my abbrevs to a file; I'd rather have them as lisp
;; :PROPERTIES:
;; :ID:       20230803T055416.342537
;; :END:
(set! abbrev-file-name null-device)
;; **** capitalize
;; :PROPERTIES:
;; :ID:       20230909T153643.051946
;; :END:
(define-global-abbrev "i" "I")
(define-global-abbrev "luis" "Luis")
;; **** imho                                                                       :abbrev:
;; :PROPERTIES:
;; :ID:       20230802T192737.209043
;; :END:
(define-global-abbrev "imho" "in my humble opinion")
;; **** imo                                                                        :abbrev:
;; :PROPERTIES:
;; :ID:       20230802T192722.138540
;; :END:
(define-global-abbrev "imo" "in my opinion")
;; **** fyi                                                                        :abbrev:
;; :PROPERTIES:
;; :ID:       20230802T192700.148484
;; :END:
(define-global-abbrev "fyi" "for your information")
;; **** lmk                                                                        :abbrev:
;; :PROPERTIES:
;; :ID:       20230802T171239.604183
;; :END:
(define-global-abbrev "lmk" "let me know")
;; **** afaik                                                                      :abbrev:
;; :PROPERTIES:
;; :ID:       20230802T163541.823256
;; :END:
(define-global-abbrev "afaik" "as far as I know")
;; **** idk                                                                        :abbrev:
;; :PROPERTIES:
;; :ID:       20230802T171437.846231
;; :END:
(define-global-abbrev "idk" "I do not know")
;; **** abbreviate =im= to =I'm=
;; :PROPERTIES:
;; :ID:       20230909T153859.786986
;; :END:
;; To press =I'm= on a QWERTY keyboard is too hard.  It's much easier to just type =im=.
(define-global-abbrev "im" "I am")
;; **** tbh
(define-global-abbrev "tbh" "to be honest")
;; **** abbreviate =qwerty= to =QWERTY=
;; :PROPERTIES:
;; :ID:       20230909T153936.924067
;; :END:
;; Haha.  As I was typing my commentary for [[][]] I realized how troublesome it is to type QWERTY and
;; I immediately made an abbrev for it.
(define-global-abbrev "qwerty" "QWERTY")
;; **** spell out all english abbreviation with ='=
;; :PROPERTIES:
;; :ID:       20230909T154712.166953
;; :END:
;; A continuation of my war on ='=.  I started this war with [[][I'm]].
(define-global-abbrev "dont" "do not")
;; **** convert =ive= to =I've=
;; :PROPERTIES:
;; :ID:       20230911T124829.255887
;; :END:
(define-global-abbrev "ive" "I've")
;; **** stop myself from misspelling =surprise= as =surprise=
;; I don't know why but I often tend to mispell =surprise= as =surprise=.  This
;; abbrev will fix this typo for me should I ever make this mistake in the
;; future.
(define-global-abbrev "suprise" "surprise")
;; **** another common misspell is for me to mix up the =i= and =o=
(define-global-abbrev "functoin" "function")
;; **** =gonna= - going to
;; :PROPERTIES:
;; :ID:       20230918T150556.925877
;; :END:
(define-global-abbrev "gonna" "going to")
;; **** =ngl= - not going to lie
;; :PROPERTIES:
;; :ID:       20230918T150535.294257
;; :END:
(define-global-abbrev "ngl" "not going to lie")
;; **** =tbf= - to be frank
;; :PROPERTIES:
;; :ID:       20230918T150442.391086
;; :END:
(define-global-abbrev "tfb" "to be frank")
;; **** more abbrevs
;; :PROPERTIES:
;; :ID:       20231010T112351.552472
;; :END:
(define-global-abbrev "ndo" "window")
(define-global-abbrev "tis" "it is")
(define-global-abbrev "amly" "automatically")
(define-global-abbrev "rn" "right now")
(define-global-abbrev "bly" "probably")
(define-global-abbrev "lk" "like")
;; **** abbreviate =configuration= with =config=
;; :PROPERTIES:
;; :ID:       20231006T144138.256297
;; :END:
;; I need to figure out how to only expand an abbrev based on certain condition
;; because it clashes with some things such as the =~/.config= directory.
(define-global-abbrev "config" "configuration")
;; **** abbreviate =obviously= with =obv=
;; :PROPERTIES:
;; :ID:       20231006T123058.805780
;; :END:
(define-global-abbrev "obv" "obviously")
;; **** abbreviate =appropriate= with =appr=
;; :PROPERTIES:
;; :ID:       20231006T123259.411709
;; :END:
(define-global-abbrev "appr" "appropriate")
;; **** abbreviate =don't= with =dnt=
;; :PROPERTIES:
;; :ID:       20231006T102442.829910
;; :END:
;; Prior to this I had =dont= expand to =don't=.  Why not remove one more vowel?
(define-global-abbrev "dnt" "don't")
;; **** abbreviate =of-course= with =ofc=
;; :PROPERTIES:
;; :ID:       20231006T102026.537852
;; :END:
;; I'll likely end up changing this.  I don't think I want this to be a global
;; abbrev.  Maybe I should make it just for =text-mode=.
(define-global-abbrev "ofc" "of course")
;; **** abbreviate =O.K.= with =ok=
;; :PROPERTIES:
;; :ID:       20231006T132227.798421
;; :END:
;; I would always just write =ok= in text but I think it's proper to actually write
;; out =O.K.=.
(define-global-abbrev "ok" "O.K.")
;; **** abbreviate =it's= with =iis=
;; :PROPERTIES:
;; :ID:       20231008T061714.575772
;; :END:
;; I don't like writing the ='=.  Naturally, I'm inclined to abbreviate =it's= with =its=
;; but its is actually a different word which I use often.  That's why I settled on
;; =iis=.
(define-global-abbrev "iis" "it's")
;; **** abbreviate =you'll= with =ul=
;; :PROPERTIES:
;; :ID:       20231008T161416.323520
;; :END:
;; This is continuation of my war against ='=.
(define-global-abbrev "ul" "you'll")
;; **** replace =bc= with =because=
;; :PROPERTIES:
;; :ID:       20231006T043735.171481
;; :END:
;; This is just a useful abbreviation.  I use the word "because" often.
(define-global-abbrev "bc" "because")
;; **** abbreviate =in other words= with =iow=
;; :PROPERTIES:
;; :ID:       20231007T131818.928017
;; :END:
(define-global-abbrev "iow" "in other words")
;; **** abbreviate =up until now= with =uun=
;; :PROPERTIES:
;; :ID:       20231008T061259.034293
;; :END:
(define-global-abbrev "uun" "up until now")
;; **** abbreviate =EXWM= with =exwm=
;; :PROPERTIES:
;; :ID:       20231009T142712.079778
;; :END:
(define-global-abbrev "exwm" "EXWM")
;; **** abbreviate "for example" with =fe=
;; :PROPERTIES:
;; :ID:       20231007T110531.380588
;; :END:
(define-global-abbrev "fe" "for example")
;; **** abbreviate =evaluated= with =evaled=
;; :PROPERTIES:
;; :ID:       20231006T120812.742130
;; :END:
(define-global-abbrev "evaled" "evaluated")
;; **** abbreviate =I'd= with =id=
;; :PROPERTIES:
;; :ID:       20231008T055429.648521
;; :END:
;; +I think =id= is [[https://www.verywellmind.com/what-is-the-id-2795275][actually a word]] but I never use it; however, I use =I'd= all the+
;; +time.  It's a worthwhile trade-off then to abbreviate =I'd= as =I'd=.+
(define-global-abbrev "idd" "I would")
;; **** abbreviate =that is= with =thats=
;; :PROPERTIES:
;; :ID:       20231014T193558.633337
;; :END:
(define-global-abbrev "thats" "that is")
;; **** abbreviate =I will= with =illl=
;; :PROPERTIES:
;; :ID:       20231014T195733.115935
;; :END:
(define-global-abbrev "illl" "I will")
;; **** abbreviate =doc-string= with =doc-string= and =dstr=
;; :PROPERTIES:
;; :ID:       20231015T125840.552335
;; :END:
(define-global-abbrev "docstring" "doc-string")
(define-global-abbrev "docstrings" "doc-strings")
(define-global-abbrev "dstr" "doc-string")
(define-global-abbrev "dstrs" "doc-strings")
;; **** abbreviate =I know= with =ik=
;; :PROPERTIES:
;; :ID:       20231015T151329.190932
;; :END:
(define-global-abbrev "ik" "I know")
;; **** abbreviate =describe= with =ribe=
;; :PROPERTIES:
;; :ID:       20231015T151617.394097
;; :END:
(define-global-abbrev "ribe" "describe")

(provide 'oo-abbrevs)
