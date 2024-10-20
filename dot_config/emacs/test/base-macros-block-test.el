;;; base-macros-block-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'base-macros-block)

;;;; helpers
(fset 'parse (apply-partially #'oo--parse-block nil))

(defun body (body) (cadr (parse body)))

(defun data (obj) (car obj))

(defun let-binds (body)
  (map-elt (car (parse body)) :let))

(defun let-syms (body)
  (mapcar #'car (let-binds body)))

(defun lvalue (sym body)
  (car (map-elt (let-binds body) sym)))
;;;; main
;; (ert-deftest oo--parse-block ()
;;   (should-not (oo--parse-block nil `(dotimes (n 3)
;;                                            (and (= 1 n) (continue!))
;;                                            (collecting! nums n)))))

(ert-deftest oo--generate-block-body ()
  (should (equal '(catch 'return! (let nil))  (oo--generate-block-body nil)))
  ;; (should-not (oo--generate-block-body '((+ 1 1))))
  )

(ert-deftest block!---ignores-quoted-forms ()
  (let ((quoted-form '('(foo))))
    (should (equal nil (let-binds quoted-form)))
    (should (equal quoted-form (body quoted-form)))))

(ert-deftest block!---skips-loops-with-continue ()
  (block! (dotimes (n 3)
            (and (= 1 n) (continue!))
            (collecting! nums n))
          (should (equal nums '(0 2)))))

(ert-deftest block!---exits-body-when-return-is-invoked ()
  (should (= 2 (block! (when t (return! 2)) 3)))
  (should (= 2 (block! (dotimes (n 10) (return! 2)) 3)))
  (should (= 2 (block! (when t (return! 2)) 3))))

(ert-deftest block!---exits-loop-if-break! ()
  (should (= 2 (block! 2)))
  (should (= 2 (catch 'break! (break! 2) 4)))
  (should (= 2 (block! nil (dotimes (x 3) (when (= x 2) (break! x))))))
  (should (= 2 (block! (dotimes (n 10) (return! 2)) 3)))
  (should (= 5 (block! (dotimes (i 10) (when (= 5 i) (break! 5))))))
  (should (equal '(nums) (let-syms '(block! nil (dotimes (i 3) (collecting! nums i)) nums))))
  )

(ert-deftest block!---handles-set-correctly ()
  (should (equal '(a b) (let-syms '((set! a 1) (set! b 2)))))
  (should (equal '((set! a 1)) (body '((set! a 1)))))
  (should (equal '(a b c d) (let-syms '((set! (a [b] [[c]] d) '(1 [2] [[3]] d))))))
  (should (equal '((a nil)) (let-binds '((set! a 1)))))
  (should (equal '((a nil) (b nil) (c nil)) (let-binds '((set! (a b c) 1)))))
  (should (equal '((set! (a b c) 1)) (body '((set! (a b c) 1))))))

(ert-deftest block!---binds-symbol-specified-by-minning-to-most-positive-fixnum ()
  "Binds symbol specified by `minning!' to `most-positive-fixnum'"
  (should (= most-negative-fixnum (lvalue 'a '((maximizing! a 1)))))
  (should (= most-negative-fixnum (lvalue 'a '((maxing! a 1)))))
  (should (equal '(a) (let-syms '((collecting! a 1)))))
  (should (equal '(a) (let-syms '((appending! a 1)))))
  (should (equal '(a) (let-syms '((prepending! a 1)))))
  (should (equal '(a) (let-syms '((maxing! a 1)))))
  (should (= 0 (lvalue 'a '((counting! a 1)))))
  (should (= most-positive-fixnum (lvalue 'a '((minimizing! a 1)))))
  (should (= most-positive-fixnum (lvalue 'a '((minning! a 1))))))

(ert-deftest block!---does-not-bind-symbols-marked-for-exclusion ()
  "does not bind symbols marked for exclusion"
  (should (equal '((set! a 1)) (body '((exclude! a) (set! a 1)))))
  (should (equal '(a) (map-elt (car (parse '((exclude! a) (set! a 1))))
                               :nolet))))

;; ;; (ert-deftest bind ()
;; ;;   (pcase-let* ((`(,data ,body) (oo--parse-block nil '(alet! (+ 1 1)))))
;; ;;     (should (equal (map-elt data :let) '((it bind () nil))))))

;; ;; (ert-deftest bind-to-value-specified-by-aprog1 ()
;; ;;   (let ((form '((aprog1! (+ 1 1)) 2 3)))
;; ;;     (should (body form) :to-equal '((prog1 (setq ert-deftest bind () (+ 1 1)) 2 3)))
;; ;;     (should (let-syms form) :to-contain 'it bind ())))

(ert-deftest block!---stubbing-macros-work ()
  "wraps subsequent forms with lef!"
  (should (equal '((cl-flet ((plus (a b) (+ a (* 2 b)))) (plus 1)))
                 (body '((stub! plus (a b) (+ a (* 2 b))) (plus 1)))))
  (should (equal '((lef! ((plus (a b) (+ a (* 2 b)))) (plus 1)))
                 (body '((nflet! plus (a b) (+ a (* 2 b))) (plus 1)))))
  (should (= 10 (block! (stub! plus (a b) (+ a (* 2 b))) (plus 6 2))))
  (should (= 10 (block! (flet! plus #'+) (plus 5 5))))
  (should (= 10 (block! (nflet! + (a b) (funcall this-fn 1 (* a b)))
                        (+ 3 3))))
  (should (= 9 (block!
                (flet! four () 4)
                (flet! five () 5)
                (+ (four) (five))))))

(ert-deftest block!---gensym ()
  "wraps subsequent forms with lef!"
  (should (equal '(foo bar baz) (let-binds '((gensym! foo bar baz)))))
  (should (equal '((gensym! foo)) (body '((gensym! foo))))))
;;; provide
(provide 'base-macros-block-test)
;;; base-macros-block-test.el ends here
