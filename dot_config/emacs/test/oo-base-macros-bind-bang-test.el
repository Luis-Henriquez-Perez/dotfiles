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
                 (oo--build-steps '(:state normal))))
  ;; (should (equal '(oo)))
  )

;; (ert-deftest oo--bind-build-metadata ()
;;   ;; Just a key and a definition.
;;   ;; (bind! "d" #'ho)
;;   (should (equal '(:keymap global-map :key "d" :def #'ho)
;;                  (oo--bind-build-metadata '("d" #'ho))))
;;   ;; => (:keymap global-map :key "d" :def #'ho)

;;   ;; A state letter with a key and definition.
;;   ;; (bind! i "d" #'ho)
;;   (should (equal '(:evil-state-letters (i) :keymap global-map :key "d" :def #'ho)
;;                  (oo--bind-build-metadata '(i "d" #'ho))))
;;   ;; => (:evil-states (i) :keymap global-map :key "d" :def #'ho)

;;   ;; A list of state letter with a key and definition.
;;   ;; (bind! (n v) "d" #'ho)
;;   (should '(:evil-state-letters (n v) :key "d" :def #'ho)
;;           (oo--bind-build-metadata '((n v) "d" #'ho)))
;;   ;; => (:evil-states (n v) :key "d" :def #'ho)

;;   ;; A state symbol with a key and definition.
;;   ;; (bind! normal "d" #'ho)
;;   (should '(:evil-states (normal) :keymap global-map :key "d" :def #'ho)
;;           (oo--bind-build-metadata '(normal "d" #'ho)))
;;   ;; => (:evil-states normal :keymap global-map :key "d" :def #'ho)
;;   )
;;; provide
(provide 'oo-base-macros-bind-bang-test)
;;; oo-base-macros-bind-bang-test.el ends here
