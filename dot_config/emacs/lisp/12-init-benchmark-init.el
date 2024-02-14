(require 'benchmark-init)

(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(oo-add-hook 'after-init-hook 'benchmark-init/deactivate)

(provide 'oo-init-benchmark-init)
