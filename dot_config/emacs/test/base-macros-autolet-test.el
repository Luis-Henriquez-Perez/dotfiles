;;; base-macros-block-autolet.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'pcase)
(require 'dash)
(require 'base-macros-autolet)

;; (macroexpand-1 '(autolet! :noinit (foo) (set! foo 2)))
;; ;; => (catch 'return! (set! foo 2))
;; (macroexpand-1 '(autolet! :init ((b 1)) (set! foo 2)))
;; ;; => (catch 'return! (let ((b 1) (foo nil)) (set! foo 2)))
;; (macroexpand-1 '(autolet! (setq foo 2)))
;; ;; => (catch 'return! (setq foo 2))
;; (macroexpand-1 '(autolet! (set! (a . b) '(1 . 2))))
;; ;; => (catch 'return! (let ((b nil) (a nil)) (set! (a . b) '(1 . 2))))
;; (macroexpand-1 '(autolet! (appending! foo 2)))
;; ;; => (catch 'return! (let ((foo nil)) (appending! foo 2)))
;; (macroexpand-1 '(autolet! (collecting! foo 2)))
;; ;; => (catch 'return! (let ((foo nil)) (collecting! foo 2)))
;; (macroexpand-1 '(autolet! (maximizing! foo 2)))
;; ;; => (catch 'return! (let ((foo most-negative-fixnum)) (maximizing! foo 2)))
;; (macroexpand-1 '(autolet! (minimizing! foo 2)))
;; ;; => (catch 'return! (let ((foo most-positive-fixnum)) (minimizing! foo 2)))
;; (macroexpand-1 '(autolet! (counting! foo 2)))
;; ;; => (catch 'return! (let ((foo 0)) (counting! foo 2)))
;; (macroexpand-1 '(autolet! (while t (+ 1 1))))
;; ;; => (catch 'return! (catch 'break! (while t (catch 'continue! (+ 1 1)))))
;; (macroexpand-1 '(autolet! (dolist (foo '(1 2 3)) (+ 1 1))))
;; ;; => (catch 'return! (catch 'break! (dolist (foo '(1 2 3)) (catch 'continue! (+ 1 1)))))
;; (macroexpand-1 '(autolet! (stub! foo (a b) (+ 1 2))))
;; ;; => (catch 'return! (cl-flet ((foo (a b) (+ 1 2)))))
;; (macroexpand-1 '(autolet! (stub! foo (a b) (+ 1 2)) x y z))
;; ;; => (catch 'return! (cl-flet ((foo (a b) (+ 1 2))) x y z))
;; (macroexpand-1 '(autolet! (noflet! foo (a b) (+ 1 2)) x y z))
;; ;; => (catch 'return! (lef! ((foo (a b) (+ 1 2))) x y z))
;; (macroexpand-1 '(autolet! (labels! foo (a b) (+ 1 2)) (stub! foo (a b) (+ 1 2)) x y z))
;; ;; => (catch 'return! (cl-labels ((foo (a b) (+ 1 2))) (cl-flet ((foo (a b) (+ 1 2))) x y z)))
;; (macroexpand-1 '(autolet! '(labels! foo (a b) (+ 1 2))))
;; ;; => (catch 'return! '(labels! foo (a b) (+ 1 2)))
;;;; helpers
(defun expand (form) (macroexpand-1 form))
(defun lets (form) (mapcar #'car (macroexpand-1 form)))
(defun letbind (sym form) (assoc sym (cl-second (macroexpand-1 form))))
(defun letbinds (form) (cl-second (macroexpand-1 form)))
(defun body (form) (cddr (cl-third (macroexpand-1 form))))
;;;; main
(defmacro autolet? (a b)
  `(should (null (cl-set-difference ,a (car (oo--autolet-data ,b)) :test #'equal))))

(ert-deftest autolet!---correctly-processes-keywords ()
  (autolet? '((a 10) (b nil)) '(:init ((a 10)) (set! a 1) (set! b 1)))
  (autolet? '((a 10)) '(:init ((a 10)) (set! a 1)))
  (autolet? nil '(autolet! :noinit (a) (set! a 1))))

(ert-deftest autolet!---skips-loops-with-continue ()
  (autolet! (dotimes (n 3)
              (and (= 1 n) (continue!))
              (collecting! nums n))
            (should (equal nums '(0 2)))))

(ert-deftest autolet!---exits-body-when-return-is-invoked ()
  (should (= 2 (autolet! (when t (return! 2)) 3)))
  (should (= 2 (autolet! (dotimes (n 10) (return! 2)) 3)))
  (should (= 2 (autolet! (when t (return! 2)) 3))))

(ert-deftest autolet!---exits-loop-if-break ()
  (should (= 2 (autolet! 2)))
  (should (= 2 (catch 'break! (break! 2) 4)))
  (should (= 2 (autolet! (dolist (x 3) (when (= x 2) (break! x))))))
  (should (= 2 (autolet! (dotimes (x 3) (when (= x 2) (break! x))))))
  (should (= 2 (autolet! (dotimes (n 10) (return! 2)) 3)))
  (should (= 5 (autolet! (dotimes (i 10) (when (= 5 i) (break! 5)))))))

(ert-deftest autolet!---handles-set-correctly ()
  (should (set-difference '((a nil) (b nil)) (car (oo--autolet-data '(autolet! (set! a 1) (set! b 2))))))
  (should (equal '((a nil)) (letbinds '(autolet! (set! a 1)))))
  (let ((bindings (letbinds '(autolet! (set! (a [b] [[c]] d) '(1 [2] [[3]] d))))))
    (should (and (member '(a nil) bindings)
                 (member '(b nil) bindings)
                 (member '(c nil) bindings)
                 (member '(d nil) bindings)))))

(ert-deftest autolet!---binds-symbol-specified-by-minning-to-most-positive-fixnum ()
  "Binds symbol specified by `minning!' to `most-positive-fixnum'"
  ;; (should-not (expand '(autolet! (maximizing! a 1))))
  (should (equal '((a most-negative-fixnum)) (car (oo--autolet-data '((maximizing! a 1))))))
  (should (equal '((a most-positive-fixnum)) (letbinds '(autolet! (minimizing! a 1)))))
  (should (equal '((minimizing! a 1)) (body '(autolet! (minimizing! a 1)))))
  (should (equal '((a 0)) (letbinds '(autolet! (counting! a 1)))))
  (should (equal '((a nil)) (letbinds '(autolet! (collecting! a 1)))))
  (should (equal '((a nil)) (letbinds '(autolet! (appending! a 1)))))
  (should (equal '((a nil)) (letbinds '(autolet! (prepending! a 1))))))

;; (ert-deftest autolet!---ignores-quoted-forms ()
;;   (autolet! '(set! foo 1)))

(ert-deftest autolet!---stubbing-macros-work ()
  "wraps subsequent forms with lef!"
  (should (equal '((cl-flet ((plus (a b) (+ a (* 2 b)))) (plus 1)))
                 (body '(autolet! (stub! plus (a b) (+ a (* 2 b))) (plus 1)))))
  (should (equal '((lef! ((plus (a b) (+ a (* 2 b)))) (plus 1)))
                 (body '(autolet! (nflet! plus (a b) (+ a (* 2 b))) (plus 1)))))
  (should (= 10 (autolet! (stub! plus (a b) (+ a (* 2 b))) (plus 6 2))))
  (should (= 10 (autolet! (flet! plus #'+) (plus 5 5))))
  (should (= 10 (autolet! (nflet! + (a b) (funcall this-fn 1 (* a b)))
                          (+ 3 3))))
  (should (= 9 (autolet!
                (flet! four () 4)
                (flet! five () 5)
                (+ (four) (five))))))

;; (ert-deftest autolet!---gensym ()
;;   "wraps subsequent forms with lef!"
;;   (should (equal '(foo bar baz) (let-binds '((gensym! foo bar baz)))))
;;   (should (equal '((gensym! foo)) (body '((gensym! foo))))))
;;; provide
(provide 'base-macros-block-autolet)
;;; base-macros-block-autolet.el ends here
