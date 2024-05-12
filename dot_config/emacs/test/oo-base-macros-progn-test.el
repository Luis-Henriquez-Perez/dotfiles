;;; oo-base-macros-progn-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(require 'oo-base-macros-progn)
(require 'buttercup)

(describe "progn!"
  (cl-flet* ((parse (apply-partially #'oo--parse-progn-bang nil))
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
      (expect '(0 2) :to-equal (progn! (dotimes (n 3)
                                         (and (= 1 n) (continue!))
                                         (collecting! nums n))
                                       nums)))
    (it "exits body when `return!' is invoked."
      (expect 2 :to-be (progn! (when t (return! 2)) 3)))
    (it "exits from loops when `break!' is invoked"
      (expect '(nums) :to-equal (let-syms '(progn! nil (dotimes (i 3) (collecting! nums i)) nums)))
      (expect 2 :to-be (progn! 2))
      (expect 2 :to-be (catch 'break! (break! 2) 4))
      (expect 2 :to-equal (progn! (dotimes (n 10) (return! 2)) 3))
      (expect 2 :to-be (progn! nil (dotimes (x 3) (when (= x 2) (break! x)))))
      (expect 5 :to-equal (progn! nil (dotimes (i 10) (when (= 5 i) (break! 5))))))
    (it "does not bind symbols marked for exclusion"
      (expect (parse '((exclude! a) (set! a 1))) :to-equal '((:nolet (a) :let ((a nil))) (nil (setq a 1))))
      (expect (macroexpand-1 '(progn! (exclude! a) (set! a 1)))
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
      (pcase-let* ((`(,data ,body) (oo--parse-progn-bang nil '(alet! (+ 1 1)))))
        (should (equal (map-elt data :let) '((it nil))))))
    (it "binds it to value specified by `aprog1'"
      (let ((form '((aprog1! (+ 1 1)) 2 3)))
        (expect (body form) :to-equal '((prog1 (setq it (+ 1 1)) 2 3)))
        ;; (expect (not (let-syms form)))
        (expect (let-syms form) :to-contain 'it)))
    (it "wraps subsequent forms with lef!"
      (expect 10 :to-equal (progn! nil (stub! plus (a b) (+ a (* 2 b))) (plus 6 2)))
      (expect 10 :to-equal (progn! nil (flet! plus #'+) (plus 5 5)))
      (expect 10 :to-equal (progn! nil
                                   (nflet! + (a b) (funcall this-fn 1 (* a b)))
                                   (+ 3 3))))))
;;; provide
(provide 'oo-base-macros-progn-test)
;;; oo-base-macros-progn-test.el ends here
