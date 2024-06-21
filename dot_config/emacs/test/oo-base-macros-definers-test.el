;;; oo-base-macros-definers-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Test `oo-base-macro-definers'.
;;
;;; Code:
(require 'oo-base-macros-definers)

(defun oo--map-contains-and-equal-p (map key value)
  (and (map-contains-key map key)
       (equal (map-elt map key) value)))

(defun oo-map-same-elements-p (map1 map2)
  "Return non-nil if MAP1 has the same elements as MAP2."
  (map-every-p (apply-partially #'oo--map-contains-and-equal-p map2) map1))

(defun oo-map-exactly-same-elements-p (map1 map2)
  (and (oo-map-same-elements-p map1 map2)
       (oo-map-same-elements-p map2 map1)))

;; (should (oo-map-same-elements-p '(:a 1 :b 2 :c 3) '(:a 1 :b 2 :c 3)))
;; (should-not (oo-map-same-elements-p '(:a 1 :b 2 :c 3 :d 4) '(:a 1 :b 2 :c 3)))

(ert-deftest oo--arglist ()
  (should (equal '(a b c) (oo--arglist '(a b &rest c))))
  (should (equal '(a b c d) (oo--arglist '(a b c d)))))

(ert-deftest oo--interactive-p ()
  (should (oo--interactive-p '(interactive)))
  (should-not (oo--interactive-p '(declare (indent)))))

(ert-deftest oo--declaration-p ()
  (should-not (oo--declaration-p '(interactive)))
  (should (oo--declaration-p '(declare (indent)))))

;; (ert-deftest oo--extract-options ()
;;   (should (equal '((:a 1 :b 2) ((+ 1 1) 2)) (oo--extract-options '(:a 1 :b 2 (+ 1 1) 2))))
;;   (should (equal '(nil (+ 1 1)) (oo--extract-options '(+ 1 1))))
;;   (should (equal '((:a 1 :b 2 :c 3) ((+ 1 1))) (oo--extract-options '(:a 1 :b 2 :c 3 (+ 1 1))))))

(ert-deftest oo--definer-components-1 ()
  (should (equal '("foo" nil nil ((+ 1 1)))                 (oo--definer-components-1 '("foo" (+ 1 1)))))
  (should (equal '("foo" (declare) nil ((+ 1 1)))           (oo--definer-components-1 '("foo" (declare) (+ 1 1)))))
  (should (equal '("foo" nil (interactive) ((+ 1 1)))       (oo--definer-components-1 '("foo" (interactive) (+ 1 1)))))
  (should (equal '("foo" (declare) (interactive) ((+ 1 1))) (oo--definer-components-1 '("foo" (declare) (interactive) (+ 1 1)))))
  (should (equal '(nil nil (interactive) ((+ 1 1)))         (oo--definer-components-1 '((interactive) (+ 1 1))))))

(ert-deftest oo--definer-components ()
  (should (equal '(:name foo :arglist (a b c) :docstring "do" :declaration nil :interactive nil :body ((+ 1 1)))
                 (oo--definer-components '(foo (a b c) "do" (+ 1 1)))))
  (should (equal '(:name foo :arglist (a b c) :docstring "do" :declaration nil :interactive nil :body ((+ 1 1)))
                 (oo--definer-components '(foo (a b c) "do" (+ 1 1))))))

(ert-deftest oo--prognify-components ()
  (should (equal '(:name foo :arglist (a b c) :docstring "do" :declaration nil :interactive nil :body ((catch 'return! (let nil (collecting! a 1) (+ 1 1)))))
                 (oo--prognify-components (oo--definer-components '(foo (a b c) "do" (collecting! a 1) (+ 1 1))))))
  (should (equal (oo--prognify-components (oo--definer-components '(foo (a b c) "do" (+ 1 1))))
                 '(:name foo :arglist (a b c) :docstring "do" :declaration nil :interactive nil :body ((catch 'return! (let nil (+ 1 1))))))))

(ert-deftest oo--finalize-components ()
  (should (equal '(foo (a b c) "do" (catch 'return! (let nil (collecting! a 1) (+ 1 1))))
                 (oo--finalize-components '(:name foo :arglist (a b c) :docstring "do" :declaration nil :interactive nil :body ((catch 'return! (let nil (collecting! a 1) (+ 1 1))))))))
  ;; (should-not (oo--prognify-components (oo--definer-components '(foo (a b c) "do" (+ 1 1)))))
  )
;;; provide
(provide 'oo-base-macros-definers-test)
;;; oo-base-macros-definers-test.el ends here
