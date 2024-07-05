;;; oo-base-macros-hook-bang-test.el --- Test `oo-base-macros-hook-bang' -*- lexical-binding: t; -*-
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
;; Test `oo-base-macros-hook-bang'.
;;
;;; Code:
(require 'oo-base-macros-hook-bang)

(ert-deftest hook! ()
  (progn!
    (set! run-list nil)
    (defvar fake-hook nil)
    (should-not fake-hook)
    (nflet! a () (push 'a run-list))
    (nflet! c () (push 'c run-list))
    (nflet! b-error () (push 'b-error run-list) (/ 2 0))
    (hook! fake-hook&a)
    (hook! fake-hook&b-error)
    (hook! fake-hook&c)
    ;; All the hooks have been added.
    (should (equal fake-hook '(fake-hook&c fake-hook&b-error fake-hook&a)))
    (run-hooks 'fake-hook)
    ;; Even the hook with the error should be in the list.
    (should (equal run-list '(a b-error c)))))
;;; provide
(provide 'oo-base-macros-hook-bang-test)
;;; oo-base-macros-hook-bang-test.el ends here
