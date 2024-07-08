;;; oo-base-macros-after-bang-test.el --- test `oo-base-macros-after-bang.el' -*- lexical-binding: t; -*-
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
;;
;;; Code:
(require 'oo-base-macros-after-bang)

(ert-deftest oo-after ()
  (progn! (set! result nil)
          (after! foo (push 1 result))
          (provide 'foo)
          (should (equal '(1) result))
          (provide 'foo)
          ;; It should only be evaluated once.
          (should (equal '(1) result))
          ))

;; (ert-deftest oo--after-bang-body ()
;;   (should (equal (oo--after-bang-forms nil '((+ 1 1))) '((+ 1 1))))
;;   (should (equal (oo--after-bang-forms 'a '((+ 1 1))) '((+ 1 1))))
;;   (should (equal (oo--after-bang-body nil '(+ 1 1))))
;;   (should (equal (oo--after-bang-body 'foo '(+ 1 1))))
;;   (should (equal (oo--after-bang-body '(:or foo bar) '(+ 1 1))))
;;   (should (equal (oo--after-bang-body '(:and foo bar) '(+ 1 1))))
;;   )
;;; provide
(provide 'oo-base-macros-after-bang-test)
;;; oo-base-macros-after-bang-test.el ends here
