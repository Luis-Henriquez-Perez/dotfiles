;;; 02-base-lib-test.el --- test `02-base-lib.el' -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz.io>
;;
;; Author: Luis Henriquez <luis@luishp.xyz.io>
;; Maintainer: Luis Henriquez <luis@luishp.xyz.io>
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains tests for `02-base-lib'.
;;
;;; Code:
;; [[https://scripter.co/quick-intro-to-emacs-lisp-regression-testing/][quick-intro-to-emacs-lisp-regression-testing]]

(require 'buttercup)
(require '02-base-lib)

(describe "oo--mf-flatten"
  (it "should flatten 2d lists"
    (expect (oo--mf-flatten '(a b c d)) :to-have-same-items-as '(a b c d)))
  (it "should flatten nested lists"
    (expect (oo--mf-flatten '(a (b g) (c (e f)) d)) :to-have-same-items-as
            '(a b g c e f d)))
  (it "should flatten improper lists"
    (expect (oo--mf-flatten '(a b . c)) :to-have-same-items-as '(a b c)))
  (it "should flatten vectors"
    (expect (oo--mf-flatten '(a [b c] d)) :to-have-same-items-as '(a b c d))
    (expect (oo--mf-flatten '[a [b c] d]) :to-have-same-items-as '(a b c d))
    (expect (oo--mf-flatten '[a b c d]) :to-have-same-items-as '(a b c d))))

(describe "oo-condition-case-fn"
  (let ((fn (oo-condition-case-fn (lambda (n) (/ 8 (- n 4))) (lambda (&rest _) 'foo))))
    (it "should behave like function when no error occurs"
      (expect 2 :to-be (funcall fn 8)))
    (it "should invoke action function when there is an error"
      (expect 'foo :to-be (funcall fn 4)))))

(describe "break!"
  (it "should exit a (catch 'break!) block"
    ;; (expect 1 :to-equal (catch 'foo (throw 'foo 1) 2))
    (expect 6 :to-equal (catch 'break! (break! 6) 7))))

(describe "continue!"
  (it "should exit a (catch 'continue!) block"
    ;; (expect 1 :to-equal (catch 'foo (throw 'foo 1) 2))
    (expect nil :to-equal (catch 'continue! (continue!) 7))))

(xdescribe "oo--parse-block"
  (expect '(:let ((a nil) (b nil))) :to-equal (car (oo--parse-block nil '((set! a 1) (set! b 2))))))

(describe "collecting!"
  (it "adds items to the end of the list"
    (let (collected)
      (collecting! collected 1)
      (expect '(1) :to-equal collected)
      (collecting! collected 2)
      (expect '(1 2) :to-equal collected))))

(describe "block!"
  (cl-flet* ((parse (apply-partially #'oo--parse-block nil))
             (body (body) (cadr (parse body)))
             (let-binds (body) (map-elt (car (parse body)) :let))
             (let-syms (body) (mapcar #'car (let-binds body)))
             (lvalue (sym body) (car (map-elt (let-binds body) sym))))
    (it "binds symbols to generated symbols when using `gensym!'"
      (expect (let-syms '((gensym! foo bar baz))) :to-have-same-items-as '(foo bar baz)))
    (it "ignores quoted forms"
      (expect nil :to-equal (let-binds '('(foo))))
      (expect '('(foo)) :to-equal (body '('(foo)))))
    (it "skips the rest of the loop body if `continue!' is invoked"
      (expect '(0 2) :to-equal (block! (for! (n 3)
                                         (and (= 1 n) (continue!))
                                         (collecting! nums n))
                                 nums)))
    (it "exits body when `return!' is invoked."
      (expect 2 :to-be (block! (when t (return! 2)) 3)))
    (it "exits from loops when `break!' is invoked"
      (expect '(nums) :to-equal (let-syms '(block! nil (for! (i 3) (collecting! nums i)) nums)))
      (expect 2 :to-be (block! 2))
      (expect 2 :to-be (catch 'break! (break! 2) 4))
      (expect 2 :to-equal (block! (for! (n 10) (return! 2)) 3))
      (expect 2 :to-be (block! nil (for! (x 3) (when (= x 2) (break! x)))))
      (expect 5 :to-equal (block! nil (for! (i 10) (when (= 5 i) (break! 5))))))
    (it "does not bind symbols marked for exclusion"
      (expect (parse '((exclude! a) (set! a 1))) :to-equal '((:nolet (a) :let ((a nil))) (nil (setq a 1))))
      (expect (macroexpand-1 '(block! (exclude! a) (set! a 1)))
              :to-equal
              '(catch 'return! (let nil nil (setq a 1))))
      ;; (expect (let-syms '((exclude! a) (set! a 1))) :to-equal nil)
      ;; (expect (let-syms '((exclude! a) (counting! a 1))) :to-equal nil)
      )
    (it "binds symbol specified by `maxing!' to `most-negative-fixnum'"
      (expect most-negative-fixnum :to-be (lvalue 'a '((maximizing! a 1))))
      (expect most-negative-fixnum :to-be (lvalue 'a '((maxing! a 1)))))
    (it "binds symbol specified by `counting!' to 0"
      (expect 0 :to-be (lvalue 'a '((counting! a 1)))))
    (it "binds symbol specified by `minning!' to `most-positive-fixnum'"
      (expect most-positive-fixnum :to-be (lvalue 'a '((minimizing! a 1))))
      (expect most-positive-fixnum :to-be (lvalue 'a '((minning! a 1)))))
    (it "binds symbol specified by other ING macros to nil"
      (expect '(a) :to-equal (let-syms '((collecting! a 1))))
      (expect '(a) :to-equal (let-syms '((appending! a 1))))
      (expect '(a) :to-equal (let-syms '((prepending! a 1))))
      (expect '(a) :to-equal (let-syms '((maxing! a 1)))))
    (it "binds symbol specified by set! to nil"
      (expect (let-syms '((set! a 1) (set! b 2))) :to-equal '(a b)))
    (it "binds the symbols in match-form specified by `set!' to nil"
      (expect (let-syms '((set! (a [b] [[c]] d) '(1 [2] [[3]] d)))) :to-equal '(a b c d)))
    (it "binds `it' to value specified by `alet!' or `aset!'."
      (pcase-let* ((`(,data ,body) (oo--parse-block nil '(alet! (+ 1 1)))))
        (should (equal (map-elt data :let) '((it nil))))))
    (it "binds it to value specified by `aprog1'"
      (let ((form '((aprog1! (+ 1 1)) 2 3)))
        (expect (body form) :to-equal '((prog1 (setq it (+ 1 1)) 2 3)))
        ;; (expect (not (let-syms form)))
        (expect (let-syms form) :to-contain 'it)))
    (it "wraps subsequent forms with lef!"
      (expect 10 :to-equal (block! nil (stub! plus (a b) (+ a (* 2 b))) (plus 6 2)))
      (expect 10 :to-equal (block! nil (flet! plus #'+) (plus 5 5)))
      (expect 10 :to-equal (block! nil
                             (nflet! + (a b) (funcall this-fn 1 (* a b)))
                             (+ 3 3))))))

(describe "oo-list-marker-p"
  (it "correctly identifies list markers"
    (expect (oo-list-marker-p '&as))
    (expect (oo-list-marker-p '&whole))
    (expect (oo-list-marker-p '&rest)))
  (it "correctly returns nil with non-list-markers"
    (should-not (oo-list-marker-p 'foo))
    (should-not (oo-list-marker-p '$as))))

(describe "oo-defun-components"
  (it "separates the docstring from the body omitting nils"
    (expect (equal '(("foo") ((+ 1 1)))
                   (oo-defun-components '("foo" (+ 1 1))))))
  (it "separates the docstring from the body without omitting nils"
    (expect (equal '(("foo" nil nil) ((+ 1 1)))
                   (oo-defun-components '("foo" (+ 1 1)) t))))
  (it "separates the interactive form from the body"
    (expect (equal '((nil nil (interactive)) (1))
                   (oo-defun-components '((interactive) 1) t)))
    (expect (equal '(("foo" nil (interactive)) (1))
                   (oo-defun-components '("foo" (interactive) 1) t)))))

(describe "with-map!"
  (it "correctly assigns bang symbols to map values"
    (expect (= 3 (with-map! '((a . 1) (b . 2)) (+ !a !b))))
    (expect (= 3 (with-map! '(a 1 b 2) (+ !a !b))))))

(describe "for!"
  (it "properly loops with predicate being (repeat N)"
    (expect 11 :to-equal (let ((n 1)) (for! (repeat 10) (cl-incf n)) n)))
  (it "destructures if predicate is (MATCH-FORM LIST)"
    (expect '(3 9) :to-equal (let ((list '((1 2) (4 5)))
                                   (result nil))
                               (for! ((a b) list)
                                 (push (+ a b) result))
                               (reverse result))))
  (it "properly loops with predicate being (VAR SEQUENCE)"
    (expect '(4 3 2 1) :to-equal (let (nums) (for! (n '(1 2 3 4)) (push n nums)) nums))
    (expect '(4 3 2 1) :to-equal (let (nums) (for! (n [1 2 3 4]) (push n nums)) nums))
    (expect '(111 108 108 101 104) :to-equal (let (chars) (for! (char "hello") (push char chars)) chars)))
  (it "properly loops with predicate being (VAR INTEGER)"
    (expect '(0 1 2 3) :to-equal (let (n) (for! (x 4) (collecting! n x)) n))
    (expect 11 :to-equal (let ((n 1)) (for! (x 10) (cl-incf n)) n)))
  (it "propertly loops with predicate being INTEGER"
    (expect 11 :to-equal (let ((n 1)) (for! 10 (cl-incf n)) n))))

(describe "oo-wrap-forms"
  (it "wrap forms around body"
    (expect (equal '(when 1 (save-excursion foo))
                   (oo-wrap-forms '((when 1) (save-excursion)) '(foo))))))

(describe "to--pcase"
  (it "leaves a symbol as is"
    (expect 'a :to-be (oo--to-pcase 'a)))
  (it "converts a to b"
    (expect (oo--to-pcase '(a [b (d) e])) :to-equal '`(,a [,b (,d) ,e]))
    (expect (oo--to-pcase '(a (b (d) e))) :to-equal '`(,a (,b (,d) ,e)))
    (expect (oo--to-pcase '(a b)) :to-equal '`(,a ,b))))

(describe "oo--&as-mf-p"
  (it "identifies a match form"
    (should (oo--&as-mf-p '(&as foo (a . b)))))
  (it "rejects a non-match form"
    (expect (oo--&as-mf-p '(a b c)) :to-be nil)))

(describe "oo--&as-mf-destruc"
  (it "destructures an &as match-form"
    (expect (cl-letf (((symbol-function #'cl-gensym) (lambda (&rest _) 'x)))
              (oo--&as-mf-destruc '(&as foo (a . b)) 'value))
            :to-equal
            '((x value) (foo x) ((a . b) x)))))

(describe "oo--mf-destruc"
  (it "destructures an &as match-form"
    (expect (cl-letf (((symbol-function #'cl-gensym) (lambda (&rest _) 'x)))
              (oo--mf-destruc '(&as foo (a . b)) 'value))
            :to-equal
            '((x value) (foo x) ((a . b) x)))))

(describe "oo--mf-match-p"
  (it "should match special match forms"
    (expect (oo--mf-match-p '(&as foo (c . d))) :to-be t))
  (it "should not match anything else"
    (expect (oo--mf-match-p '(a b (&as foo (c . d)))) :to-be nil)))

(xdescribe "oo--let-replace-mf"
  (it "should replace special match forms in"
    (expect (oo--mf-replace '(a b (&as foo (c . d))) '(1 2 (3 . 4)))
            :to-equal
            '((a b *match-form*) '(1 2 (3 . 4))))))

(xdescribe "oo--convert-pcase-to-let"
  (it "should"
    (should-not (oo--to-pcase-let '(a (&as foo (b c)) d) '(1 (2 3) 4)))))

(describe "let!"
  (it "allow binding to be vector if only one pattern"
    (expect '(3 4) :to-equal (let! [(a b) '(3 4)] (list a b))))
  (it "binds variables to values just like let*"
    (expect (= 3 (let! ((a 1) (b 2)) (+ a b)))))
  (it "can bind functions to symbols"
    (expect (= 3 (let! ((#'foo (lambda (a b) (+ a b)))) (foo 1 2))))
    (expect (= 1 (let! ((:flet foo #'car)) (foo '(1)))))
    (expect (= 1 (let! ((:noflet foo #'car)) (foo '(1)))))
    (expect (= 2 (let! ((:flet foo (a) (+ a a))) (foo 1)))))
  (describe "match forms"
    (it "destructures a \"normal\" match-form"
      (expect (= 1 (let! (((foo) '(1 2 3 4))) 1)))
      (expect (equal '(1 2 3) (let! (((a b . c) '(1 2 . 3))) (list a b c))))
      (expect (equal '(1 1 2) (let! ((foo 1) ((a b) '(1 2))) (list foo a b)))))
    (it "correctly destructures `&as' match-forms"
      (expect (equal '((1 . 2) 1 2) (let! (((&as foo (a . b)) '(1 . 2))) (list foo a b)))))
    ;; (it "correctly destructures `&map' match-forms")
    (it "destructures a match-form containing a vector"
      (expect (equal '(1 1 2 9 8) (let! ((foo 1) ([c d] [9 8]) ((a b) '(1 2)))
                                    (list foo a b c d)))))))

(describe "oo--definer-body"
  (it "it should return the body for a defun"
    ;; (expect '(defun foo (a) (block! foo (exclude! a) 2))
    ;;         :to-equal
    ;;         (oo--definer-body 'defun '(foo (a) 2)))
    (expect (oo--definer-body 'defun '(foo (a) 2))
            :to-equal '(defun foo (a) (block! (exclude! a) 2)))))

;; (describe "oo--get-symbols"
;;   (it "gets the symbols from"
;;     (expect (oo--get-symbols '(,a ,b [,c ,d])) :to-have-same-items-as '(a b c d))
;;     (expect (oo--get-symbols '(,a ,b ,c ,d)) :to-have-same-items-as '(a b c d))
;;     (expect (oo--get-symbols '(a b [c d])) :to-have-same-items-as '(a b c d))
;;     (expect (oo--get-symbols '(a b c d)) :to-have-same-items-as '(a b c d))))

(describe "set!"
  (it "should act like `setq' when given a symbol"
    (expect (let (a) (set! a 1) 1) :to-be 1))
  (it "should be able to destructure arguments passed in."
    ;; Works with improper lists.
    (expect (let (a b c) (set! (a b . c) '(1 3 . 4)) (list a b c)) :to-equal '(1 3 4))
    (expect (let (a b c) (set! (a (b . c)) '(1 (3 . 4)))))
    (expect (let (a b) (set! (a b) '(1 2)) (list a b)) :to-equal '(1 2)))
  (it "should work with vectors as well"
    (expect (let (a b c) (set! [a b c] [1 2 3]) (list a b c)) :to-equal '(1 2 3)))
  (it "should be able to use special patterns"
    (expect (let (a b c)
              (set! (a (&as foo (b . c))) '(1 (3 . 4)))
              (list a b c foo))
            :to-equal
            '(1 3 4 (3 . 4)))))

(provide '02-base-lib-test)
;;; 02-base-lib-test.el ends here
