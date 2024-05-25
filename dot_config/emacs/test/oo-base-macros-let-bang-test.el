;;; oo-base-macros-let-bang-test.el -*- lexical-binding: t; -*-
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
;; Test for `oo-base-macros-let-bang'.
;;
;;; Code:
(require 'oo-base-macros-let-bang)

(ert-deftest let!---allow-binding-to-be-vector-if-only-one-pattern ()
  (should (equal '(3 4) (let! [(a b) '(3 4)] (list a b)))))

(ert-deftest let!---binds-variables-to-values-just-like-let* ()
  (should (= 3 (let! ((a 1) (b 2)) (+ a b)))))

(ert-deftest let!---can-bind-functions-to-symbols ()
  (should (= 3 (let! ((#'foo (lambda (a b) (+ a b)))) (foo 1 2))))
  (should (= 1 (let! ((:flet foo #'car)) (foo '(1)))))
  (should (= 1 (let! ((:noflet foo #'car)) (foo '(1)))))
  (should (= 2 (let! ((:flet foo (a) (+ a a))) (foo 1)))))

(ert-deftest let!---destructures-a-normal-match-form ()
  (should (= 1 (let! (((foo) '(1 2 3 4))) 1)))
  (should (equal '(1 2 3) (let! (((a b . c) '(1 2 . 3))) (list a b c))))
  (should (equal '(1 1 2) (let! ((foo 1) ((a b) '(1 2))) (list foo a b)))))

(ert-deftest let!---correctly-destructures-&as-match-forms ()
  (should (equal '((1 . 2) 1 2) (let! (((&as foo (a . b)) '(1 . 2))) (list foo a b)))))

(ert-deftest let!---destructures-a-match-form-containing-a-vector ()
  (should (equal '(1 1 2 9 8) (let! ((foo 1) ([c d] [9 8]) ((a b) '(1 2)))
                                (list foo a b c d)))));;; provide

(provide 'oo-base-macros-let-bang-test)
;;; oo-base-macros-let-bang-test.el ends here
