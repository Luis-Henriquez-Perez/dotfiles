;; [[https://scripter.co/quick-intro-to-emacs-lisp-regression-testing/][quick-intro-to-emacs-lisp-regression-testing]]

;; If the directory happens to have both compiled and uncompiled
;; version, prefer to use the newer (typically the uncompiled) version.
(setq load-prefer-newer t)
(setq ert-batch-print-level 10)
(setq ert-batch-print-length 120)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "tests"))

(require 'test_oo-base-lib)
