(oo-add-hook 'emacs-startup-hook #'which-key-mode)

(opt! which-key-sort-uppercase-first nil)
(opt! which-key-max-display-columns nil)
(opt! which-key-add-column-padding 1)
(opt! which-key-min-display-lines 1)
(opt! which-key-side-window-slot -10)
(opt! which-key-sort-order #'which-key-prefix-then-key-order)
(opt! which-key-popup-type 'side-window)
(opt! which-key-idle-delay 0.8)
;; (opt! line-spacing 3 :hook which-key-init-buffer-hook :local t)

(opt! which-key-show-transient-maps t)
(opt! which-key-show-operator-state-maps t)

(opt! which-key-show-prefix 'top)

(provide '19-init-which-key)
