;;; oo-base-macros-with-map-bang-test.el -*- lexical-binding: t; -*-
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
;; Test `oo-base-macros-with-map-bang'.
;;
;;; Code:
(require 'ert)
(require 'oo-base-macros-with-map-bang)

(ert-deftest with-map!--correctly-assigns-bang-symbols-to-map-values ()
  (should (= 3 (with-map! '((a . 1) (b . 2)) (+ !a !b))))
  (should (= 3 (with-map! '(a 1 b 2) (+ !a !b)))))
;;; provide
(provide 'oo-base-macros-with-map-bang-test)
;;; oo-base-macros-with-map-bang-test.el ends here
