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
  ;; 1 (bind! global-map "d" #'foo)
  (should (equal '(oo-kbd :keymap global-map :key "d" :def #'foo)
                 (oo--kbd-forms '(global-map "d" #'foo))))
  ;; 2 (bind! insert "d" #'foo)
  (should (equal '(oo-kbd :states 'insert :key "d" :def #'foo)
                 (oo--kbd-forms '(insert "d" #'foo))))
  ;; 3 (bind! i org-mode-map "d" #'foo)
  (should (equal '(oo-kbd :states ?i :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(i org-mode-map "d" #'foo))))
  ;; 4 (bind! insert org-mode-map "d" #'foo)
  (should (equal '(oo-kbd :states 'insert :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(insert org-mode-map "d" #'foo))))
  ;; 5 (bind! i "d" #'foo)
  (should (equal '(oo-kbd :states ?i :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(i org-mode-map "d" #'foo))))
  ;; 6 (bind! (n m v) "d" #'foo)
  (should (equal '(oo-kbd :states '(?n ?m ?v) :key "d" :def #'foo)
                 (oo--kbd-forms '((n m v) "d" #'foo))))
  ;; 7 (bind! org-mode-map i "d" #'foo)
  (should (equal '(oo-kbd :states ?i :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(org-mode-map i "d" #'foo))))
  ;; 8 (bind! (n m v) "d" #'foo)
  (should (equal '(oo-kbd :states '(?n ?m ?v) :key "d" :def #'foo)
                 (oo--kbd-forms '((n m v) "d" #'foo))))
  ;; 9 (bind! (normal insert visual) "d" #'foo)
  (should (equal '(oo-kbd :states '(normal insert visual) :key "d" :def #'foo)
                 (oo--kbd-forms '((normal insert visual) "d" #'foo))))
  ;; 10 (bind! org-mode-map (n m v) "d" #'foo)
  (should (equal `(oo-kbd :states '(?n ?m ?v) :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(org-mode-map (n m v) "d" #'foo))))
  ;; 11 (bind! org-mode-map (normal motion visual) "d" #'foo)
  (should (equal `(oo-kbd :states '(normal motion visual) :keymap org-mode-map :key "d" :def #'foo)
                 (oo--kbd-forms '(org-mode-map (normal motion visual) "d" #'foo))))
  ;; 12 (bind! (n m v) org-mode-map "d" #'foo)
  (should (equal ))
  )
;;; provide
(provide 'base-bind-test)
;;; base-bind-test.el ends here
