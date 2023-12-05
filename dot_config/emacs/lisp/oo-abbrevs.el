(require 'abbrev)

(set! abbrev-file-name null-device)
(define-global-abbrev "i" "I")
(define-global-abbrev "luis" "Luis")

;; * common abbreviations
;; These are abbrevations.
(define-global-abbrev "imho" "in my humble opinion")
(define-global-abbrev "imo" "in my opinion")
(define-global-abbrev "fyi" "for your information")
(define-global-abbrev "lmk" "let me know")
(define-global-abbrev "afaik" "as far as I know")
(define-global-abbrev "idk" "I do not know")
(define-global-abbrev "im" "I am")
(define-global-abbrev "tbh" "to be honest")
(define-global-abbrev "qwerty" "QWERTY")
(define-global-abbrev "dont" "do not")
(define-global-abbrev "ive" "I've")
(define-global-abbrev "suprise" "surprise")
(define-global-abbrev "functoin" "function")
(define-global-abbrev "gonna" "going to")
(define-global-abbrev "ngl" "not going to lie")
(define-global-abbrev "tfb" "to be frank")
(define-global-abbrev "ndo" "window")
(define-global-abbrev "tis" "it is")
(define-global-abbrev "amly" "automatically")
(define-global-abbrev "rn" "right now")
(define-global-abbrev "bly" "probably")
(define-global-abbrev "lk" "like")
(define-global-abbrev "config" "configuration")
(define-global-abbrev "obv" "obviously")
(define-global-abbrev "appr" "appropriate")
(define-global-abbrev "bc" "because")

;; * eliminate
(define-global-abbrev "dnt" "don't")
(define-global-abbrev "ofc" "of course")
(define-global-abbrev "ok" "O.K.")
;; **** abbreviate =it's= with =iis=
(define-global-abbrev "iis" "it's")
(define-global-abbrev "ul" "you'll")
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
