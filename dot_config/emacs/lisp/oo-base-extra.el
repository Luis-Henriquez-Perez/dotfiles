;; I will be honest, I had no idea what to name this file.  Here, I will load
;; the patches and the extensions lazily.

;; I had been thinking that maybe with an org file this would be easier as I
;; could just wrap the code for the patches in an =after!= block.  But it is not
;; so simple because if there's multiple code blocks it could become an issue.
;; I had also considered specifying the package in the name so that I can
;; automate loading them.

(oo-call-after-load '(outline evil) #'require 'oo-evil-headline-state)
(oo-call-after-load 'outline #'require 'oo-outline-patch)
(oo-call-after-load 'outshine #'require 'oo-outshine-bullets)

(provide 'oo-base-extra)
