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
;; This is redundant because I [[][already load]] the patch in [[][]].  But for
;; the sake of consistency and to account for the event where I either don't use
;; treepy for the block macro or don't use the block macro I include this here.
(oo-call-after-load 'treepy #'require 'oo-treepy-patch)

(provide 'oo-base-extra)
