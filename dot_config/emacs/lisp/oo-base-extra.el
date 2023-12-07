;; I will be honest, I had no idea what to name this file.  Here, I will load
;; the patches and the extensions lazily.

(oo-call-after-load 'outline #'require 'evil)
(oo-call-after-load 'outshine #'require)
(oo-call-after-load '(outline evil) #'require 'oo-evil-headline-state)

(provide 'oo-base-extra)
