(require 'oo-base-utils)

(ert-deftest oo-ampersand-symbol-p ()
  (should (oo-ampersand-symbol-p '&foo))
  (should-not (oo-ampersand-symbol-p 'foo)))

;; (ert-deftest oo-sharp-quoted-p ()
;;   (should (oo-sharp-quoted-p '#foo)))

;; (ert-deftest oo-non-keyword-symbol-p ()
;;   (should (oo-non-keyword-symbol-p)))

;; (ert-deftest oo-args-to-keyword ()
;;   (should (oo-args-to-keyword-p)))

;; (ert-deftest loop! ()
;;   (should))

(provide 'oo-base-utils-test)
