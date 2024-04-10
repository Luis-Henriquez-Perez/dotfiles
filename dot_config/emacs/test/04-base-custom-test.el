;;; 04-base-custom-test.el --- test `04-base-custom.el' -*- lexical-binding: t -*-

;; Author: Luis Henriquez-Perez
;; Version: 0.1
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Test `04-base-custom.el'.
;;
;;; Code:

(require 'buttercup)
(require 'init)

(describe "oo-first-success"
  (it "should return nil if (fn elt) is never non-nil"
    (expect (oo-first-success #'not '(1 2 3 4)) :to-be nil))
  (it "should return the first non-nil (fn elt) in list"
    (expect (oo-first-success #'not '(1 2 3 nil 4)) :to-equal t)))

(describe "oo-advice-components"
          (it "should return nil if a symbol is not an advice symbol"
              (expect (oo-advice-components 'not-advice) :to-be nil))
          (it "should return (ADVISEE ADVICE-ABBREV FUNCTION) if given an advice symbol"
              (expect (oo-advice-components 'advisee@FAfunction) :to-equal '(advisee FA function))))

(describe "oo-once-only-fn"
          :var ((it (oo-only-once-fn (-const 1))))
          (it "the function should return 1 the first time"
              (expect (funcall it) :to-equal 1))
          (it "should return nil from that point on"
              (should-not (funcall it))
              (should-not (funcall it))))

(describe "if-not!"
  (it "should invert the places of then and else"
    (expect (if-not! nil 0 1) :to-equal 0)
    (expect (if-not! t 0 1) :to-equal 1)))

(describe "oo-hook"
  (it "should return nil when symbol has no hook"
    (expect nil :to-be (oo-hook 'foo)))
  (it "should return hook if symbol has a hook"
    (expect 'prog-mode-hook :to-be (oo-hook 'prog-mode-hook&foo))))

;; (xdescribe "oo-add-hook"
;;   ;; (oo-add-hook 'test-hook #'foo)
;;   ;; (expect )
;;   ;; (block!
;;   ;;   (gensym! fake-hook fake-fn)
;;   ;;   (set! expected-name (format "%s&%s" fake-hook fake-fn))
;;   ;;   (cl-progv (list fake-hook) (list nil)
;;   ;;     (set! hook-sym (oo-add-hook fake-hook fake-fn))
;;   ;;     (it "should return the new hook symbol"
;;   ;;       (expect (symbolp hook-sym))
;;   ;;       (expect (symbol-name hook-sym) :to-equal expected-name))
;;   ;;     ;; (describe "oo-remove-hook"
;;   ;;     ;;   (it "should also infer hook if named"
;;   ;;     ;;     (expect (oo-remove-hook hook-sym)))
;;   ;;     ;;   (it "should remove hook normally when given"
;;   ;;     ;;     (expect (oo-remove-hook 'foo-mode-hook #'fake-fn))))
;;   ;;     ))
;;   )

;; (xdescribe "oo-autoload-action-function"
;;   (it "should call a function"))

;; (xdescribe "oo-add-advice"
;;   (set! fake-hook (cl-gensym "fake-hook"))
;;   (set! oo-errors nil)
;;   (cl-progv (list fake-hook) (list nil)
;;     (set! return-value (oo-add-advice))
;;     (expect return-value :to-equal ')
;;     (should (symbol-value fake-hook))))

;; (xdescribe "oo-advice"
;;   (it "should return the advice"
;;     (expect (oo-advice 'some-fn@funcall-quietly) :to-be 'funcall-quietly))
;;   (it "should"
;;     (expect nil :to-be (oo-advice 'some-fn))))

;; (xdescribe "oo-remove-advice"
;;   (it "should remove advice normally"
;;     (expect (oo-remove-advice)))
;;   (it "should also infer if advice is named"
;;     (expect (oo-remove-advice 'some-fn@funcall-quietly))))

;; ;; The problem is that `it' expands to a function.  For sure I am conflicted
;; ;; about using `buttercup' because it does not allow me to let bind
;; ;; freely since `it' and `expect' expand to functions.  Instead I have to use
;; ;; its own Domain-Specific-Language mechanism for defining variables which I do
;; ;; not like.  Whereas if I were just using `ert' I could just have `should'
;; ;; forms arbitrarily nested within the lisp forms.
;; (xdescribe "oo-candidate-features"
;;   :var ((fake-path '("a/b/c/evil" "a/b/c/helm" "a/b/c/ivy")))
;;   ;; Build a list of fake package paths.
;;   (it "should return a list of candidate features"
;;     (expect (oo-candidate-features 'evil-insert-state fake-path) :to-equal '(evil)))
;;   (push "a/b/c/evil-horse" fake-path)
;;   (push "a/b/c/evil-mouse-state" fake-path)
;;   (it "should sort the resulting list by shortest"
;;     (expect (oo-candidate-features 'evil-insert-state fake-path) :to-equal '(evil))))

;; (xdescribe "opt!"
;;   (block!
;;     (it "should set the value of variable variable is unbound"
;;       (expect (opt! foo 1)))
;;     (it "should not set anything if the feature is not loaded"
;;       (opt! two 2)
;;       (expect two))
;;     (it "should have added to the")
;;     (it "should n")
;;     (opt! foo 1)
;;     (expect )))

;; (xdescribe "oo-report-error-fn"
;;   (it "should report an error when")
;;   (it "should"))

;; (xdescribe "defhook!"
;;   ;; (it "should report an error when")
;;   (it "should"
;;     (oo-add-hook)))

(provide '04-base-custom-test)
;;; 04-base-custom-test.el ends here
