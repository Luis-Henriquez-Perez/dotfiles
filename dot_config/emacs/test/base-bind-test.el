;;; base-bind-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'ert)
(require 'base-bind)

(ert-deftest oo--kbd-forms ()
  ;; (bind! global-map "d" #'foo)
  (should (equal '(oo-kbd :keymap global-map :key "d" :def #'foo)
                 (oo--kbd-forms '(global-map "d" #'foo))))
  ;; (bind! insert "d" #'foo)
  (should (equal '(oo-kbd :states 'insert :key "d" :def #'foo)
                 (oo--kbd-forms '(insert "d" #'foo))))
  ;; (bind! i org-mode-map "d" #'foo)
  (should (equal '(oo-kbd :states ?i :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(i org-mode-map "d" #'foo))))
  ;; (bind! insert org-mode-map "d" #'foo)
  (should (equal '(oo-kbd :states 'insert :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(insert org-mode-map "d" #'foo))))
  ;; (bind! i "d" #'foo)
  (should (equal '(oo-kbd :states ?i :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(i org-mode-map "d" #'foo))))
  ;; (bind! (n m v) "d" #'foo)
  (should (equal '(oo-kbd :states '(?n ?m ?v) :key "d" :def #'foo)
                 (oo--kbd-forms '((n m v) "d" #'foo))))
  )
;;; provide
(provide 'base-bind-test)
;;; base-bind-test.el ends here
