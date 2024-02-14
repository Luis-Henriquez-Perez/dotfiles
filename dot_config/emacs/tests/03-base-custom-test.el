(require 'buttercup)

(describe "startup-set!"
  ;; Simulate startup.
  (it "should set a variable before startup time"
    (startup-set! foo 1))
  (it "should"
    (startup-set! foo 1))
  (it "should do nothing if its after startup time"
    (startup-set! foo 1)
    ))

(describe "oo-report-error"
  ;; (it "should report an error when")
  ;; (it "should")
  )

(describe "defhook!"
  ;; (it "should report an error when")
  ;; (it "should")
  )
;; (describe "require!"
;;   (expect (require! "") :to-expand-into
;;           (progn (require foo)
;;                  (require foo)
;;                  ())))

;; (describe "oo-get-hook"
;;   (it "should return nil when symbol has no hook"
;;     (expect nil :to-be (oo-get-hook 'foo)))
;;   (it "should return hook if symbol has a hook"
;;     (expect 'prog-mode-hook :to-be (oo-get-hook 'prog-mode-hook&foo))))

(describe "oo-add-hook"
  (let (fake-hook)
    (it "should return the hook created"
      (expect (oo-get-hook (oo-add-hook 'fake-hook symbol))))
    (it "should work with hook symbol as an argument"
      (expect (oo-add-hook 'fake-hook&some-fn)))
    (it "should"
      (expect (oo-get-hook )))))

;; (describe "oo-add-advice"
;;   (it "should"
;;     (expect))
;;   (it ""
;;     (expect)))

;; (describe "oo-remove-hook"
;;   (it "should also infer hook if named"
;;     (expect (oo-remove-hook 'foo-mode-hook&)))
;;   (it "should remove hook normally"
;;     (expect (oo-remove-hook 'foo-mode-hook #'foo))))

;; (describe "oo-advice"
;;   (it "should return the advice"
;;     (expect (oo-advice 'some-fn@funcall-quietly) :to-be 'funcall-quietly))
;;   (it "should"
;;     (expect nil :to-be (oo-advice 'some-fn))))

;; (describe "oo-remove-advice"
;;   (it "should remove advice normally"
;;     (expect (oo-remove-advice)))
;;   (it "should also infer if advice is named"
;;     (expect (oo-remove-advice 'some-fn@funcall-quietly))))

(provide '03-base-custom-test)
