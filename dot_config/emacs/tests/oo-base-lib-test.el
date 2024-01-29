;;; oo-base-lib-test.el --- `oo-base-lib' tests   -*- lexical-binding: t; -*-
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
;; This file contains tests for `lib-string'.
;;
;;; Code:
;; [[https://scripter.co/quick-intro-to-emacs-lisp-regression-testing/][quick-intro-to-emacs-lisp-regression-testing]]
(require 'oo-base-lib)

;; If the directory happens to have both compiled and uncompiled
;; version, prefer to use the newer (typically the uncompiled) version.
(setq load-prefer-newer t)
(setq ert-batch-print-level 10)
(setq ert-batch-print-length 120)
;; (add-to-list 'load-path (locate-user-emacs-file "lisp"))
;; (add-to-list 'load-path (locate-user-emacs-file "tests"))

;; Does not need to be required because already autoloaded.

(require 'oo-base-lib)

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
  (should (equal '(1 2 3) (oo-snoc '(1 2) 3))))

(ert-deftest oo-wrap-forms ()
  (should (equal '(when 1 (save-excursion foo)) (oo-wrap-forms '((when 1) (save-excursion)) '(foo)))))

;; (ert-deftest oo-non-keyword-symbol-p ()
;;   (should (oo-non-keyword-symbol-p 'foo))
;;   (should-not (oo-non-keyword-symbol-p :foo))
;;   (should-not (oo-non-keyword-symbol-p "foo"))
;;   (should-not (oo-non-keyword-symbol-p 1)))

;; (ert-deftest oo-args-to-string ()
;;   (should (equal "foo" (oo-args-to-string 'foo)))
;;   (should (equal ":foo" (oo-args-to-string :foo)))
;;   (should (equal "foo" (oo-args-to-string "foo")))
;;   (should (equal "1" (oo-args-to-string 1))))

;; (ert-deftest oo-args-to-symbol ()
;;   (should (equal 'foo (oo-args-to-symbol 'foo)))
;;   (should (equal :foo (oo-args-to-symbol :foo)))
;;   (should (equal 'foo (oo-args-to-symbol "foo")))
;;   (should (equal '1 (oo-args-to-symbol 1))))

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

;; (ert-deftest collecting! ()
;;   (let (collected)
;;     (collecting! adjoined 1)
;;     (collecting! adjoined 1)
;;     (should (equal '(1) collected))
;;     ;; (should)
;;     ))

(ert-deftest for! ()
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

;; (ert-deftest take-while! ()
;;   (should (let ((foo '(a b 1 2 3))) (list (take-while! (symbolp foo) foo) foo))
;;           '((a b) (1 2 3)))
;;   ;; (should ())
;;   )

(ert-deftest oo-block-interpret-tree ()
  (should (equal '(nil ((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))
                 (oo-block-interpret-tree nil '((for! (n 10) (+ 1 1))))))

  (should (equal '((:let ((foo nil))) ((collecting! foo 1)))
                 (oo-block-interpret-tree nil '((collecting! foo 1)))))

  ;; Stubbing functions.
  (should (equal '(nil ((cl-flet ((foo nil (+ 1 1))) (+ 2 2))))
                 (oo-block-interpret-tree nil '((stub! foo () (+ 1 1)) (+ 2 2)))))

  ;; Getting data from `without!'.
  (should (equal (oo-block-interpret-tree nil '((without! a b c)))
                 '((:no-let (a b c)) (nil))))

  ;; Getting data from `let!'.
  (should (equal `((:let ((foo nil))) ((setq foo 1)))
                 (oo-block-interpret-tree nil '((let! foo 1)))))
  
  (should (equal '((:let ((gc-cons-threshold gc-cons-threshold))) ((setq gc-cons-threshold 100)))
                 (oo-block-interpret-tree nil '((let! gc-cons-threshold 100))))))
;; I want to see how it works with multiple data.

;; (ert-deftest block! ()
;;   (block! nil (+ 1 1) (maxing! foo 2))
;;   (should (pcase (block! nil (+ 1 1) (appending! foo 2))
;;             (`)
;;             (_))))

(ert-deftest oo-pcase-pattern ()
  (should (equal '`((,a (,b) ,c . ,d)) (oo-pcase-pattern '((a (b) c . d)))))
  (should (equal '`[,a ,b ,c] (oo-pcase-pattern [a b c])))
  (should (equal '`(,a [,b [,d]] ,e) (oo-pcase-pattern '(a [b [d]] e)))))

(ert-deftest let! ()
  (should (equal '((1 . 2) 1 2)
                 (let! (((&as foo (a . b)) '(1 . 2))) (list foo a b))))
  (should (equal '(1 2 3) (let! (((a b . c) '(1 2 . 3))) (list a b c))))
  (should (equal '(1 1 2) (let! ((foo 1) ((a b) '(1 2))) (list foo a b))))
  (should (equal '(1 1 2 9 8)
                 (let! ((foo 1) ([c d] [9 8]) ((a b) '(1 2)))
                   (list foo a b c d)))))

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

(provide 'test_oo-base-lib)
