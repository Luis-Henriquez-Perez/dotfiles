;;; oo-base-library-test.el --- `oo-base-library' tests   -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz.io>
;;
;; Author: Luis Henriquez <luis@luishp.xyz.io>
;; Maintainer: Luis Henriquez <luis@luishp.xyz.io>
;;
;; Created: 28 Jan 2024
;;
;;
;; License: GPLv3
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
;; This file contains tests for `oo-base-lib'.
;;
;;; Code:
;; [[https://scripter.co/quick-intro-to-emacs-lisp-regression-testing/][quick-intro-to-emacs-lisp-regression-testing]]

(setq load-prefer-newer t)
;; Recommended in emacs info so that forms are not abbreviated unless the are
;; really large.
(setq ert-batch-print-level 10)
(setq ert-batch-print-length 120)

(require 'oo-base-library)

(ert-deftest appending! ()
  (let (foo plist)
    (appending! foo '(1 2))
    (should (equal foo '(1 2)))
    (appending! foo '(3 4))
    (should (equal foo '(1 2 3 4)))
    (appending! (plist-get plist :a) '(1))
    (should (equal plist '(:a 1)))))

(ert-deftest with-map! ()
  ;; (should-not (macroexpand-1 '(with-map! '(a 1 b 2) (+ !a !b))))
  (should (= 3 (with-map! '(a 1 b 2) (+ !a !b)))))

(ert-deftest oo--map-let-binds ()
  (cl-letf (((symbol-function #'cl-gensym) (lambda (&rest _) 'var)))
    (let ((map '(!foo !bar (+ 1 !zap)))
          (regexp "!\\([^[:space:]]+\\)")
          (result1 '((var map)
                     (!foo (map-elt var ':foo))
                     (!bar (map-elt var ':bar))
                     (!zap (map-elt var ':zap))))
          (result2 '((var map)
                     (!foo (map-elt var 'foo))
                     (!bar (map-elt var 'bar))
                     (!zap (map-elt var 'zap)))))
      (should (equal result1 (oo--map-let-binds 'map map regexp :use-keywords)))
      (should (equal result2 (oo--map-let-binds 'map map regexp))))))

(ert-deftest oo--match-form-wrappers ()
  (cl-letf* (((symbol-function #'cl-gensym) (lambda (&rest _) 'var)))
    (cl-flet ((fn #'oo--match-form-wrappers))
      (should (equal '(((let! ((foo var) ((a . b) var)))) var)
                     (fn '(&as foo (a . b)))))
      (should (equal (list nil '`(,a ,b ,c ,d))
                     (fn '(a b c d))))
      ;; ;; Works with `&as' directive.
      (should (equal (list '((let! ((pair var) ((a . b) var))))
                           '`(,a ,b ,var ,d))
                     (fn '(a b (&as pair (a . b)) d))))
      ;; ;; Works with `&map' directive.
      (should (equal '(((let ((var mymap))) (with-map! var)) `(,a ,b ,var ,d))
                     (fn '(a b (&map mymap) d))))
      ;; ;; Works with vectors.
      (should (equal '(nil `(,a ,b [,c] ,d)) (fn '(a b [c] d))))
      ;; ;; Can do multiple things.
      (should (equal '(((let! ((pair var) ((a . b) var))))
                       `(,a ,b [,var] ,d))
                     (fn '(a b [(&as pair (a . b))] d)))))))

(ert-deftest oo-into-string ()
  (should (equal "foo" (oo-into-string 'foo)))
  (should (equal "1" (oo-into-string 1))))

(ert-deftest oo-into-symbol ()
  (should (equal 'foo (oo-into-symbol "foo"))))

(ert-deftest oo-into-keyword ()
  (should (equal :foo (oo-into-keyword "foo"))))

(ert-deftest oo-cons-cell-p ()
  (should     (oo-cons-cell-p (cons 1 2)))
  (should-not (oo-cons-cell-p (list 1 2)))
  (should-not (oo-cons-cell-p '(1 2 . 3)))
  (should-not (oo-cons-cell-p 1)))

(ert-deftest oo-proper-list-p ()
  (should     (oo-proper-list-p '(1 2)))
  (should-not (oo-proper-list-p '(1 . 2)))
  (should-not (oo-proper-list-p '(1 2 . 3)))
  (should-not (oo-proper-list-p 10.5)))

(ert-deftest oo-improper-list-p ()
  (should-not (oo-improper-list-p '(1 2)))
  (should (oo-improper-list-p '(1 . 2)))
  (should (oo-improper-list-p '(1 2 . 3)))
  (should-not (oo-improper-list-p 10.5)))

(ert-deftest oo-snoc ()
  (should (equal '(1 2 3 4 5) (oo-snoc '(1 2) 3 4 5)))
  (should (equal '(1 2 3) (oo-snoc '(1 2) 3))))

(ert-deftest oo-wrap-forms ()
  (should (equal '(when 1 (save-excursion foo)) (oo-wrap-forms '((when 1) (save-excursion)) '(foo)))))

(ert-deftest alet! ()
  (should (= 2 (alet! 1 (+ it it)))))

(ert-deftest aif! ()
  (should (= 1 (aif! 1 it 0))))

(ert-deftest awhen! ()
  (should (= 2 (awhen! 1 (+ it it)))))

(ert-deftest adjoining! ()
  (let (adjoined)
    (adjoining! adjoined 1)
    (adjoining! adjoined 1)
    (should (equal '(1) adjoined))
    ;; (should)
    ))

(ert-deftest collecting! ()
  (let (list)
    (collecting! list 1)
    (should (equal '(1) list))
    (collecting! list 2)
    (should (equal '(1 2) list))))

(ert-deftest for! ()
  "The for feature."
  ;; Not working.
  (should (equal '(((1 . 2) 1 2) ((3 . 4) 3 4))
                 (let (list) (for! ((&as foo (a . b)) '((1 . 2) (3 . 4)))
                               (push (list foo a b) list))
                      (reverse list))))
  ;; Works for the syntax =(repeat N)= where N is a positive integer.
  (should (= 11 (let ((n 1)) (for! (repeat 10) (cl-incf n)) n)))

  ;; Also works if you just pass in the raw number.
  ;; Not the most precise because I cannot specify that its the same symbol for
  ;; all of =,(pred symbolp)= but good enough for now.
  (should (= 11 (let ((n 1)) (for! 10 (cl-incf n)) n)))

  ;; Should allow me to loop through sequences.
  (should (equal '(111 108 108 101 104)
                 (let (chars) (for! (char "hello") (push char chars)) chars)))

  (should (equal '(4 3 2 1)
                 (let (nums) (for! (n [1 2 3 4]) (push n nums)) nums)))

  (should (equal '(4 3 2 1)
                 (let (nums) (for! (n '(1 2 3 4)) (push n nums)) nums)))

  ;; Should allow me to destructure arguments.
  (should (equal '(3 9) (let ((list '((1 2) (4 5)))
                              (result nil))
                          (for! ((a b) list)
                            (push (+ a b) result))
                          (reverse result)))))

(ert-deftest oo--interpret-block ()
  ;; Should not interpret quoted or backquoted forms.
  (should (equal (list nil '('(for! (n 10) (+ 1 1))))
                 (oo--interpret-block nil '('(for! (n 10) (+ 1 1))))))

  (should (equal '((:wrappers ((save-excursion))) (1))
                 (oo--interpret-block nil '((with! (save-excursion)) 1))))

  (should (equal '(nil ((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))
                 (oo--interpret-block nil '((for! (n 10) (+ 1 1))))))

  (should (equal '((:let ((foo nil))) ((collecting! foo 1)))
                 (oo--interpret-block nil '((collecting! foo 1)))))

  ;; Stubbing functions.
  (should (equal '(nil ((cl-flet ((foo nil (+ 1 1))) (+ 2 2))))
                 (oo--interpret-block nil '((stub! foo () (+ 1 1)) (+ 2 2)))))

  (should (equal '(nil ((cl-flet ((foo nil (+ 1 1))) (+ 2 2))))
                 (oo--interpret-block nil '((flet! foo () (+ 1 1)) (+ 2 2)))))

  (should (equal '(nil ((cl-flet ((foo #'+)) (+ 2 2))))
                 (oo--interpret-block nil '((flet! foo #'+) (+ 2 2)))))

  (should (equal '(nil ((lef! ((foo (lambda () (+ 1 1)))) (+ 2 2))))
                 (oo--interpret-block nil '((nflet! foo () (+ 1 1)) (+ 2 2)))))

  ;; Getting data from `without!'.
  (should (equal '((:no-let (a b c)) nil)
                 (oo--interpret-block nil '((without! a b c)))))
  
  ;; Getting data from `let!'.
  (should (equal `((:let ((foo nil))) ((setq foo 1)))
                 (oo--interpret-block nil '((set! foo 1)))))

  ;; (should (equal `((:let ((foo bar))) nil)
  ;;                (cl-letf (((symbol-function #'cl-gensym) (lambda (&rest _) 'foo)))
  ;;                  (oo--interpret-block nil '((gensym! bar))))))
  )

;; (ert-deftest block! ()
;;   (block! nil (+ 1 1) (maxing! foo 2)))

(ert-deftest oo--let-bind ()
  (should (equal '((lef! ((f (lambda nil nil))))) (oo--let-bind '(#'f (lambda () nil)))))
  (should (equal '((let* (a))) (oo--let-bind 'a)))
  (should (equal '((let* ((a 1)))) (oo--let-bind '(a 1))))
  (should (equal '((pcase-let* ((`(,a ,b) '(1 2))))) (oo--let-bind '((a b) '(1 2)))))
  (should (equal '((pcase-let* ((var (1 . 3)))) (let! ((foo var) ((a b) var))))
                 (cl-letf (((symbol-function #'cl-gensym) (lambda (&rest _) 'var)))
                   (oo--let-bind '((&as foo (a b)) (1 . 3)))))))

(ert-deftest let! ()
  ;; (should-not (macroexpand-1 '(let! ((:flet foo #'car)) (foo '(1)))))
  (should (= 1 (let! ((:flet foo #'car)) (foo '(1)))))
  (should (= 1 (let! ((:noflet foo #'car)) (foo '(1)))))
  (should (= 2 (let! ((:flet foo (a) (+ a a))) (foo '(1)))))
  ;; (should (macroexpand-1 '(let! (((foo) '(1 2 3 4))) 1)))
  (should (= 1 (let! (((foo) '(1 2 3 4))) 1)))
  ;; (should (macroexpand-1 '(let! ((#'foo (lambda (a b) (+ a b)))) (foo 1 2))))
  (should (= 3 (let! ((#'foo (lambda (a b) (+ a b)))) (foo 1 2))))
  (should (equal '((1 . 2) 1 2) (let! (((&as foo (a . b)) '(1 . 2))) (list foo a b))))
  (should (equal '(1 2 3) (let! (((a b . c) '(1 2 . 3))) (list a b c))))
  (should (equal '(1 1 2) (let! ((foo 1) ((a b) '(1 2))) (list foo a b))))
  (should (equal '(1 1 2 9 8) (let! ((foo 1) ([c d] [9 8]) ((a b) '(1 2))) (list foo a b c d)))))

(ert-deftest oo-list-marker-p ()
  (should-not (oo-list-marker-p 'foo))
  (should-not (oo-list-marker-p '$as))
  (should (oo-list-marker-p '&as))
  (should (oo-list-marker-p '&whole))
  (should (oo-list-marker-p '&rest)))

;; (ert-deftest block! ()
;;   (should (= 1 (block! nil (set! a 1) a)))
;;   ;; (should (= (block! nil (without! a) () a)))
;;   )

;; (ert-deftest with-map! ()
;;   (should (equal 2 (with-map! '((a . 1) (b . 2)) (+ !a !b))))
;;   (should (equal 2 (with-map! '(a 1 b 2) (+ !a !b)))))

(ert-deftest oo-defun-components ()
  (should (equal '(foo (a b) ("foo" nil nil) ((+ 1 1)))
                 (oo-defun-components '(foo (a b) "foo" (+ 1 1)))))
  (should (equal '(foo (a b c) ("foo" nil (interactive)) (1))
                 (oo-defun-components '(foo (a b c) "foo" (interactive) 1))))
  (should (equal '(foo (a b c) (nil nil (interactive)) (1))
                 (oo-defun-components '(foo (a b c) (interactive) 1)))))

(ert-deftest oo-rpartial ()
  ;; The tests taken from dash's example page.
  (should (= 3 (funcall (oo-rpartial '- 5) 8)))
  (should (= 3 (funcall (oo-rpartial '- 5 2) 10))))

(ert-deftest lef! ()
  (should (= 10 (lef! ((+ (x y) (funcall this-fn (* x y) 1))) (+ 3 3))))
  (should (equal '(-1 3)
                 (lef! ((a #'-) (b (x y) (+ x y))) (list (a 4 5) (b 1 2)))))

  (should (= 5 (lef! ((+ #'-)) (+ 10 5))))
  ;; I should be able to use this-fn.  I was worried this would not work with
  ;; lexical binding enabled, but it is looking like it does.
  (should (= 16 (lef! ((+ (lambda (&rest args) (1+ (apply this-fn args))))) (+ 10 5))))
  ;; Works with an unnamed function.
  (should (= 4 (lef! ((foo (lambda () 4))) (foo))))
  ;; Works with an existing function.
  (should (= 4 (lef! ((buffer-string (lambda () 4))) (buffer-string)))))

;; (ert-deftest collect! ()
;;   (should (equal '() (let (result) (collect! (n 4) n))))
;;   (should (equal 50 (let (result) (collect! 5 10)))))

(ert-deftest oo-const ()
  (should (equal "foo" (funcall (oo-const "foo"))))
  (should (equal '(1 2 3) (funcall (oo-const '(1 2 3)))))
  (should (= 4 (funcall (oo-const 4) 1 2)))
  (should (= 4 (funcall (oo-const 4) 1)))
  (should (= 4 (funcall (oo-const 4)))))

(provide 'oo-base-library-test)
