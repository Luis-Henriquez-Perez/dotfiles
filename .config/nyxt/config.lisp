;;; nyxt
;; https://nyxt.atlas.engineer/documentation#configuration
;; https://nyxt.atlas.engineer/article/agora.org
;;;; use vi style bindings

;; Set vi mode as default
;; you can set `emacs-mode` and `CUA-mode` too
(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))
