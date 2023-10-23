;; [[https://scripter.co/quick-intro-to-emacs-lisp-regression-testing/][quick-intro-to-emacs-lisp-regression-testing]]

;; If the directory happens to have both compiled and uncompiled
;; version, prefer to use the newer (typically the uncompiled) version.
(setq load-prefer-newer t)

(require 'oo-base-utils-test)
(require 'oo-bind-functions-test)
(require 'oo-modifications-macro)
