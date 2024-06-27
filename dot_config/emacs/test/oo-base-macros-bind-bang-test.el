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
;; (require 'evil)
;; (require 'oo-test-utils)
(require 'oo-base-macros-bind-bang)

;; (defmacro pcase-match-p! (pattern matched)
;;   `(pcase ,matched
;;      (,pattern t)
;;      (_ nil)))
;; (pcase-match-p! `(,_ ,_ ,_ 1) '(1 2 3 1))

;; I want to know whether the arguments I pass in as metadata is actually
;; reaching the functions they should be reaching.  To that end, I do not want
;; any deferral to be happening.

;; Instead or perhaps in addition to the expansion I want to check whether the
;; correct keys are actually bound.  I want to check whether the propagation of
;; the values in the functions and the let-bindings are working properly.  In
;; other words, I want to check for the functionality as opposed to the specific
;; macro expansion.
(ert-deftest bind! ()
  (progn!
    (flet! oo-call-after-evil-state-char (_ fn) (funcall fn 'normal))
    (flet! oo-call-after-load (_ fn) (funcall fn))
    (flet! oo-call-after-bound (_ fn) (funcall fn))
    (flet! evil-define-key* (&rest args) (cons 'evil-define-key* args))
    (flet! evil-define-minor-mode-key (&rest args) (cons 'evil-define-minor-mode-key args))
    (flet! define-key (&rest args) (cons 'define-key args))
    (set! global-map 'global-map)
    ;; Binds normal define-key-like binding.
    (should (equal (list 'define-key global-map "d" #'foo) (bind! global-map "d" #'foo)))
    ;; Binds key when the evil state is explicitly provided.
    (should (equal (list 'evil-define-key* 'normal 'global-map "d" #'foo)
                   (bind! global-map "d" #'foo :evil-state 'normal)))
    ;; Specifies an evil state as a keyword.
    (should (equal (list 'evil-define-key* 'normal 'global-map "d" #'foo)
                   (bind! global-map :n "d" #'foo)))
    ;; Does it work with `evil-define-minor-mode-key'?
    (should (equal '(evil-define-minor-mode-key normal org-mode "d" foo)
                   (bind! global-map :n "d" #'foo :mode 'org-mode)))
    ;; Handles a binding that is not global-map.
    (set! org-mode-map 'org-mode-map)
    (should (equal (list 'evil-define-key* 'normal 'org-mode-map "d" #'foo)
                   (bind! org-mode-map :n "d" #'foo)))
    ;; Handles alternate bindings properly.
    (set! oo-alternate-commands (make-hash-table))
    (should (equal
             '(define-key global-map [remap consult-buffer] (menu-item "" switch-to-buffer :filter oo-alternate-command-choose-fn))
             (bind! :alt consult-buffer switch-to-buffer)))

    ;; Handles multiple states specified by keyword.
    (set! results nil)
    (flet! evil-define-key* (&rest args) (push (cons 'evil-define-key* args) results))
    (bind! global-map :nv "d" #'foo)
    (should (equal '((evil-define-key* normal global-map "d" foo)
                     (evil-define-key* normal global-map "d" foo))
                   results))
    ;; Treats the :g keyword as a non-evil binding.
    (set! results nil)
    (flet! define-key (&rest args) (push (cons 'define-key args) results))
    (bind! global-map :g "d" #'foo)
    (should (equal '((define-key global-map "d" foo)) results))
    (set! results nil)
    ;; Works with a :g keyword intermingled with state keywords.
    (bind! global-map :ngv "d" #'foo)
    (should (equal '((evil-define-key* normal global-map "d" foo)
                     (define-key global-map "d" foo)
                     (evil-define-key* normal global-map "d" foo))
                   results))))

;; (ert-deftest oo--bind-step-defer-keymap ()
;;   (should (equal '((oo-call-after-keymap 'org-mode-map (lambda nil))) (oo--bind-step-defer-keymap '(:keymap org-mode-map) nil)))
;;   (should-not (oo--bind-step-defer-keymap '(:keymap global-map) nil)))

;; (ert-deftest oo--bind-lambda ()
;;   (should (equal (eval `(let ((foo 1) (bar 2)) ,(oo--bind-lambda nil '(:env (:a :b) :a foo :b bar) nil)) t)
;;                  '(lambda nil (let ((foo 1) (bar 2)))))))

;; (ert-deftest oo--bind-step-evil ()
;;   (progn!
;;     (flet! fn #'oo--bind-step-evil)
;;     (should-not (fn '(:evil-symbol g) nil))
;;     (set! metadata '(:evil-symbol g :keymap org-mode-map :key "d" :def #'foo))
;;     (should (equal (fn metadata nil) nil))
;;     (setf (map-elt metadata :evil-symbol) 'n)
;;     (set! result (fn metadata nil))
;;     (should (pcase result
;;               (`((oo--eval-after-evil-state 110 . ,rest)) t)
;;               (_ nil)))
;;     ;; (should (pcase-match-p! `((oo--eval-after-evil-state 110 . ,lambda-form)) result))
;;     ;; (pcase result
;;     ;;   (
;;     ;;    (should-not (eval lambda-form))
;;     ;;    t)
;;     ;;   (_
;;     ;;    nil))
;;     )
;;   ;; (should-not (oo--bind-step-evil '(:evil-symbol g) nil))
;;   ;; (lef! ((gensym (-const 'state)))
;;   ;;   (should-not (equal (oo--bind-step-evil '(:evil-symbol n) nil)
;;   ;;                      '((oo-eval-after-evil-state ?n (lambda (state))))))
;;   ;;   (should (equal (oo--bind-step-evil '(:evil-symbol nm) nil)
;;   ;;                  '((oo-eval-after-evil-state ?n (lambda (state)))
;;   ;;                    (oo-eval-after-evil-state ?m (lambda (state))))))
;;   ;;   (should (equal (oo--bind-step-evil '(:evil-symbol nmg) nil)
;;   ;;                  '((oo-eval-after-evil-state ?n (lambda (state)))
;;   ;;                    (oo-eval-after-evil-state ?m (lambda (state)))))))
;;   )

;; (ert-deftest oo--bind-step-evil ()
;;   ;; (should (equal '((define-key keymap key def))
;;   ;;                (oo--bind-step-define-key '(:keymap keymap :key key :def def) nil)))
;;   (without-gensyms!
;;    (should-not (oo--bind-step-evil '(:keymap keymap :def def) nil))
;;    ;; ((oo-call-after-evil-state '() (lambda (state232))))
;;    (should-not (oo--bind-step-evil '(:evil-state 'normal :keymap keymap :key "d" :def def) nil))))

;; (ert-deftest oo--bind-step-define-key ()
;;   (should (equal `((define-key keymap key def))
;;                  (oo--bind-step-define-key '(:keymap keymap :key key :def def) nil)))
;;   (should-not (oo--bind-step-define-key '(:keymap keymap :def def) nil)))

;; (ert-deftest oo--bind-step-define-key ()
;;   (should (equal `((define-key keymap key def))
;;                  (oo--bind-step-define-key '(:keymap keymap :key key :def def) nil)))
;;   (should-not (oo--bind-step-define-key '(:keymap keymap :def def) nil)))

;; (ert-deftest oo--evil-letters ()
;;   (should (equal '(n m i) (oo--evil-letters 'nmi))))

;; (ert-deftest oo--standardize-args ()
;;   (should (equal '(:keymap org-mode-map :key "d" :def #'foo)
;;                  (oo--standardize-args '(org-mode-map "d" #'foo))))

;;   (should (equal '(:keymap org-mode-map :key "d" :def #'foo :wk "hi")
;;                  (oo--standardize-args '(org-mode-map "d" #'foo :wk "hi"))))

;;   (should (equal '(:key "d" :def #'foo)
;;                  (oo--standardize-args '("d" #'foo))))

;;   (should (equal '(:key "d" :def #'foo :wk "hi")
;;                  (oo--standardize-args '("d" #'foo :wk "hi"))))

;;   (should (equal '(:evil-symbol nm :key "d" :def #'foo)
;;                  (oo--standardize-args '(:nm "d" #'foo))))

;;   (should (equal '(:keymap org-mode-map :evil-symbol nm :key "d" :def #'foo)
;;                  (oo--standardize-args '(org-mode-map :nm "d" #'foo)))))

;; (deftest! oo--bind-let-bind-fn---let-binds ()
;;   (should (oo--bind-let-bind-fn)))

;; (deftest! oo--bind-step-evil-define ()
;;   (set!))

;; (deftest! oo--bind-step-which-key ()
;;   (set! metadata '(:keymap keymap :key key :def def :which-key desc))
;;   (flet! fn #'oo--bind-step-which-key)
;;   (should (equal '(()) (funcall fn)))
;;   (should (null ())))

;; (deftest! oo--bind-step-evil-define-key ()
;;   (set! metadata '(:keymap keymap :key key :def def))
;;   (flet! fn #'oo--bind-step-evil-define-key)
;;   (should (equal '() (funcall fn metadata)))
;;   (set! metadata '(:keymap keymap :key key :def def))
;;   (should (equal '((evil-define-key* states keymap key def)) (funcall fn metadata))))

;; (deftest! oo--bind-step-define-key ()
;;   (set! metadata '(:keymap keymap :key key :def def))
;;   (flet! fn #'oo--bind-step-define-key)
;;   (should (equal '((define-key keymap key def)) (funcall fn metadata))))

;; (deftest! bind!---convert-evil-keyword-to-state ()
;;   (should (equal nil (oo-evil-state nil)))
;;   ;; Unrecognized state keyword.
;;   (should (equal '(normal motion insert) (oo-evil-state :nti)))
;;   ;; All recognized.
;;   (should (equal '(normal motion insert) (oo-evil-state :nmi))))
;;; provide
(provide 'oo-base-macros-bind-bang-test)
;;; oo-base-macros-bind-bang-test.el ends here
