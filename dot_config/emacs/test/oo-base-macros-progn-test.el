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
(require 'ert)

;;;; helpers
;; (cl-flet* ((parse (apply-partially #'oo--parse-progn-bang nil))
;;            (body (body) (cadr (parse body)))
;;            (let-binds (body) (map-elt (car (parse body)) :let))
;;            (let-syms (body) (mapcar #'car (let-binds body)))
;;            (lvalue (sym body) (car (map-elt (let-binds body) sym)))))
;; (setq ert-batch-print-level 10)
;; (setq ert-batch-print-length 120)

(fset 'parse (apply-partially #'oo--parse-progn-bang nil))

(defun body (body) (cadr (parse body)))

(defun let-binds (body)
  (map-elt (car (parse body)) :let))

(defun let-syms (body)
  (mapcar #'car (let-binds body)))

(defun lvalue (sym body)
  (car (map-elt (let-binds body) sym)))
;;;; main
(ert-deftest progn!---ignore-quoted-forms ()
  (let ((quoted-form '('(foo))))
    (should (equal nil (let-binds quoted-form)))
    (should (equal quoted-form (body quoted-form)))))

(ert-deftest progn!---skip-loops-with-continue ()
  (progn! (dotimes (n 3)
            (and (= 1 n) (continue!))
            (collecting! nums n))
          (should (equal nums '(0 2)))))

(ert-deftest progn!---exit-body-when-return-is-invoked ()
  (should (= 2 (progn! (when t (return! 2)) 3)))
  (should (= 2 (progn! (dotimes (n 10) (return! 2)) 3)))
  (should (= 2 (progn! (when t (return! 2)) 3))))

(ert-deftest progn!---bind-symbol-specified-by-maxing-to-most-negative-fixnum ()
  (should (= most-negative-fixnum (lvalue 'a '((maximizing! a 1)))))
  (should (= most-negative-fixnum (lvalue 'a '((maxing! a 1))))))

(ert-deftest progn!---exit-loop-if-break! ()
  (should (= 2 (progn! 2)))
  (should (= 2 (catch 'break! (break! 2) 4)))
  (should (= 2 (progn! nil (dotimes (x 3) (when (= x 2) (break! x))))))
  (should (= 2 (progn! (dotimes (n 10) (return! 2)) 3)))
  (should (= 5 (progn! (dotimes (i 10) (when (= 5 i) (break! 5))))))
  (should (equal '(nums) (let-syms '(progn! nil (dotimes (i 3) (collecting! nums i)) nums))))
  )

(ert-deftest progn!---does-not-bind-symbols-marked-for-exclusion ()
  (should (equal '((set! a 1)) (body '((set! a 1)))))
  (should (equal '((a 1)) (let-binds '((set! a 1)))))
  ;; (should (equal '((:let ((a nil))) (nil (setq a 1))) (parse '((set! a 1)))))
  ;; (should (equal '((:nolet (a) :let ((a nil))) (nil (setq a 1))) (parse '((exclude! a) (set! a 1)))))
  ;; (should (equal '(catch 'return! (let nil nil (setq a 1))) (macroexpand-1 '(progn! (exclude! a) (set! a 1)))))
  ;; (should (equal nil (let-syms '((exclude! a) (set! a 1)))))
  ;; (should (equal nil (let-syms '((exclude! a) (counting! a 1)))))
  )

;; (ert-deftest progn!---bind-symbols-specified-by-counting-to-zero ()
;;   "Binds symbol specified by `counting!' to 0"
;;   (should (= 0 (lvalue 'a '((counting! a 1))))))

;; (ert-deftest progn!---bind-symbols-specified-by-minning-to-most-positive-fixnum ()
;;   "Binds symbol specified by `minning!' to `most-positive-fixnum'"
;;   (should (= most-positive-fixnum (lvalue 'a '((minimizing! a 1)))))
;;   (should (= most-positive-fixnum (lvalue 'a '((minning! a 1))))))

;; ;; (it "binds symbol specified by `maxing!' to `most-negative-fixnum'"
;; ;;   (expect most-negative-fixnum :to-be (lvalue 'a '((maximizing! a 1))))
;; ;;   (expect most-negative-fixnum :to-be (lvalue 'a '((maxing! a 1)))))
;; (ert-deftest bind-symbols-to-generated-symbols ()
;;   "binds symbols to generated symbols when using `gensym!'"
;;   (should (-same-items-p (let-syms '((gensym! foo bar baz))) '(foo bar baz))))

;; (ert-deftest bind ()
;;   "exits body when `return!' is invoked."
;;   (should (= 2 (progn! (when t (return! 2)) 3))))

;; (ert-deftest bind ()
;;   "does not bind symbols marked for exclusion"
;;   (should (parse '((exclude! a) (set! a 1))) :to-equal '((:nolet (a) :let ((a nil))) (nil (setq a 1))))
;;   (should (equal (macroexpand-1 '(progn! (exclude! a) (set! a 1)))
;;                  '(catch 'return! (let nil nil (setq a 1)))))
;;   (should (equal nil (let-syms '((exclude! a) (set! a 1)))))
;;   (should (equal nil (let-syms '((exclude! a) (counting! a 1))))))

;; (ert-deftest bind-symbol-specified-by-other-ing-macros-to-nil ()
;;   "binds symbol specified by other ING macros to nil"
;;   (should (equal '(a) (let-syms '((collecting! a 1)))))
;;   (should (equal '(a) (let-syms '((appending! a 1)))))
;;   (should (equal '(a) (let-syms '((prepending! a 1)))))
;;   (should (equal '(a) (let-syms '((maxing! a 1))))))

;; (ert-deftest bind-symbol-specified-by-set-to-nil ()
;;   (should (equal '(a b) (let-syms '((set! a 1) (set! b 2))))))

;; (ert-deftest bind-symbols-in-match-form-specified-by-set-to-nil ()
;;   (should (equal '(a b c d) (let-syms '((set! (a [b] [[c]] d) '(1 [2] [[3]] d)))))))

;; ;; (ert-deftest bind ()
;; ;;   (pcase-let* ((`(,data ,body) (oo--parse-progn-bang nil '(alet! (+ 1 1)))))
;; ;;     (should (equal (map-elt data :let) '((it bind () nil))))))

;; ;; (ert-deftest bind-to-value-specified-by-aprog1 ()
;; ;;   (let ((form '((aprog1! (+ 1 1)) 2 3)))
;; ;;     (should (body form) :to-equal '((prog1 (setq ert-deftest bind () (+ 1 1)) 2 3)))
;; ;;     (should (let-syms form) :to-contain 'it bind ())))

;; (ert-deftest wrap-subsequent-forms-with-letf ()
;;   "wraps subsequent forms with lef!"
;;   (should (= 10 (progn! (stub! plus (a b) (+ a (* 2 b))) (plus 6 2))))
;;   (should (= 10 (progn! (flet! plus #'+) (plus 5 5))))
;;   (should (= 10 (progn! (nflet! + (a b) (funcall this-fn 1 (* a b)))
;;                         (+ 3 3)))))
;;; provide
(provide 'oo-base-macros-progn-test)
;;; oo-base-macros-progn-test.el ends here
