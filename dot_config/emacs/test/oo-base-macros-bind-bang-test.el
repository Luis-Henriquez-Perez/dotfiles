;;; oo-base-macros-bind-bang-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-base-macros-bind-bang)

(ert-deftest oo--build-steps ()
  (should (equal '(oo--build-define-key
                   oo--build-kbd
                   oo--build-let-binds
                   oo--build-defer-keymap)
                 (oo--build-steps '(:state g))))
  (should (equal '(oo--build-evil-define-key
                   oo--build-kbd
                   oo--build-let-binds
                   oo--build-defer-keymap
                   oo--build-defer-evil-state-char)
                 (oo--build-steps '(:state i))))
  (should (equal '(oo--build-evil-define-key
                   oo--build-kbd
                   oo--build-let-binds
                   oo--build-defer-keymap
                   oo--build-defer-evil-state)
                 (oo--build-steps '(:state normal)))))

(ert-deftest oo--build-metadata ()
  ;; (bind! "d" #'foo)
  (should (equal (oo--build-metadata '("d" #'foo))
                 '(:keymap global-map :key "d" :def #'foo)))
  ;; (bind! insert "d" #'foo)
  ;; (bind! i "d" #'foo)
  (should (equal (oo--build-metadata '(insert "d" #'foo))
                 '(:state insert :keymap global-map :key "d" :def #'foo)))
  (should (equal (oo--build-metadata '(i "d" #'foo))
                 '(:state i :keymap global-map :key "d" :def #'foo)))
  ;; (bind! insert org-mode-map "d" #'foo)
  ;; (bind! i org-mode-map "d" #'foo)
  (should (equal (oo--build-metadata '(insert org-mode-map "d" #'foo))
                 '(:state insert :keymap org-mode-map :key "d" :def #'foo)))
  (should (equal (oo--build-metadata '(i org-mode-map "d" #'foo))
                 '(:state i :keymap org-mode-map :key "d" :def #'foo)))
  ;; (bind! org-mode-map insert "d" #'foo)
  ;; (bind! org-mode-map i "d" #'foo)
  (should (equal (oo--build-metadata '(org-mode-map insert "d" #'foo))
                 '(:state insert :keymap org-mode-map :key "d" :def #'foo)))
  (should (equal (oo--build-metadata '(org-mode-map i "d" #'foo))
                 '(:state i :keymap org-mode-map :key "d" :def #'foo)))
  ;; (bind! (normal insert visual) "d" #'foo)
  ;; (bind! (n m v) "d" #'foo)
  (should (equal (oo--build-metadata '((normal insert visual) "d" #'foo))
                 '(:states (normal insert visual) :keymap global-map :key "d" :def #'foo)))
  (should (equal (oo--build-metadata '((n m v) "d" #'foo))
                 '(:states (n m v) :keymap global-map :key "d" :def #'foo)))
  ;; (bind! org-mode-map (normal motion visual) "d" #'foo)
  ;; (bind! org-mode-map (n m v) "d" #'foo)
  (should (equal (oo--build-metadata '(global-map (normal insert visual) "d" #'foo))
                 '(:states (normal insert visual) :keymap global-map :key "d" :def #'foo)))
  (should (equal (oo--build-metadata '(global-map (n m v) "d" #'foo))
                 '(:states (n m v) :keymap global-map :key "d" :def #'foo)))
  ;; (bind! (normal motion visual) org-mode-map "d" #'foo)
  ;; (bind! (n m v) org-mode-map "d" #'foo)
  (should (equal (oo--build-metadata '(global-map (normal insert visual) "d" #'foo))
                 '(:states (normal insert visual) :keymap global-map :key "d" :def #'foo)))
  (should (equal (oo--build-metadata '(global-map (n m v) "d" #'foo))
                 '(:states (n m v) :keymap global-map :key "d" :def #'foo))))

(ert-deftest oo--let-binds ()
  (should-not (oo--let-binds '(:a 1 :b 2 :c 3))))
;;; provide
(provide 'oo-base-macros-bind-bang-test)
;;; oo-base-macros-bind-bang-test.el ends here
