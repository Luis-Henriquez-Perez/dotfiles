(set! which-key-sort-uppercase-first nil)
(set! which-key-max-display-columns nil)
(set! which-key-add-column-padding 1)
(set! which-key-min-display-lines 6)
(set! which-key-side-window-slot -10)
(set! which-key-sort-order #'which-key-prefix-then-key-order)
(set! which-key-popup-type 'side-window)
(set! which-key-idle-delay 0.8)

;; (set! line-spacing 3 :hook which-key-init-buffer-hook :local t)

(set! which-key-show-transient-maps t)
(set! which-key-show-operator-state-maps t)

(provide 'oo-which-key-config)
