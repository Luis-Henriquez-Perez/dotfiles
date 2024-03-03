;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'init)

(xdescribe "opt!"
  (block!
    (it "should set the value of variable variable is unbound"
      (expect (opt! foo 1)))
    (it "should not set anything if the feature is not loaded"
      (opt! two 2)
      (expect two))
    (it "should have added to the")
    (it "should n")
    (opt! foo 1)
    (expect )))

(xdescribe "oo-report-error-fn"
  (it "should report an error when")
  (it "should"))

(xdescribe "defhook!"
  ;; (it "should report an error when")
  (it "should"
    (oo-add-hook)))

(xdescribe "oo-advice-parts"
  (it "should return nil when symbol has no advice"
    (expect nil :to-be (oo-advised 'foo)))
  (it "should return advice if symbol has a hook"
    (expect (recentf-mode OV suppress-output) :to-be (oo-advice-parts 'recentf-mode@OVsuppress-output))))

;; (describe "oo-advised"
;;   (it "should return nil when symbol has no advice"
;;     (expect nil :to-be (oo-advised 'foo)))
;;   (it "should return function advised if called on an advice symbol"
;;     (expect (oo-advised 'recentf-mode@OVsuppress-output) :to-be 'recentf-mode)))

(describe "oo-hook"
  (it "should return nil when symbol has no hook"
    (expect nil :to-be (oo-hook 'foo)))
  (it "should return hook if symbol has a hook"
    (expect 'prog-mode-hook :to-be (oo-hook 'prog-mode-hook&foo))))

(xdescribe "oo-add-hook"
  (block! nil
    (gensym! fake-hook fake-fn)
    (set! expected-name (format "%s&%s" fake-hook fake-fn))
    (cl-progv (list fake-hook) (list nil)
      (set! hook-sym (oo-add-hook fake-hook fake-fn))
      (it "should return the new hook symbol"
        (expect (symbolp hook-sym))
        (expect (symbol-name hook-sym) :to-equal expected-name))
      ;; (describe "oo-remove-hook"
      ;;   (it "should also infer hook if named"
      ;;     (expect (oo-remove-hook hook-sym)))
      ;;   (it "should remove hook normally when given"
      ;;     (expect (oo-remove-hook 'foo-mode-hook #'fake-fn))))
      )))

;; (xdescribe "oo-add-advice"
;;   (set! fake-hook (cl-gensym "fake-hook"))
;;   (set! oo-errors nil)
;;   (cl-progv (list fake-hook) (list nil)
;;     (set! return-value (oo-add-advice))
;;     (expect return-value :to-equal ')
;;     (should (symbol-value fake-hook))))

(xdescribe "oo-advice"
  (it "should return the advice"
    (expect (oo-advice 'some-fn@funcall-quietly) :to-be 'funcall-quietly))
  (it "should"
    (expect nil :to-be (oo-advice 'some-fn))))

(xdescribe "oo-remove-advice"
  (it "should remove advice normally"
    (expect (oo-remove-advice)))
  (it "should also infer if advice is named"
    (expect (oo-remove-advice 'some-fn@funcall-quietly))))

(describe "oo-advice-components"
          (it "should return nil if a symbol is not an advice symbol"
              (expect (oo-advice-components 'not-advice) :to-be nil))
          (it "should return (ADVISEE ADVICE-ABBREV FUNCTION) if given an advice symbol"
              (expect (oo-advice-components 'advisee@FAfunction) :to-equal '(advisee FA function))))

(describe "oo-repeat-list"
          (it "should populate list with result of funcall invokations"
              (expect (oo-repeat-list 4 (-const 4)) :to-equal '(4 4 4 4))))

(describe "oo-once-only-fn"
          :var ((it (oo-only-once-fn (-const 1))))
          (it "the function should return 1 the first time"
              (expect (funcall it) :to-equal 1))
          (it "should return nil from that point on"
              (should-not (funcall it))
              (should-not (funcall it))))

(provide '04-base-custom-test)
