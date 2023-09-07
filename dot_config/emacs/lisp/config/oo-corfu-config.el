(setq corfu-bar-width 0)

(set! corfu-auto-prefix 1)

(set! corfu-quit-at-boundary nil)
(set! corfu-auto t)
(set! corfu-auto-delay 0.1)

(set! corfu-quick1 "abcdefghijklmnopqrstuvxyz")

(oo-add-hook 'corfu-mode #'corfu-history-mode)

(bind! (:map corfu-map)
       ("<tab>" #'corfu-next)
       ([backtab] #'corfu-previous)
       ("C-;" #'corfu-quick-complete)
       ("C-j" #'corfu-next)
       ;; Works
       ("M-k" #'corfu-previous)
       ;; For some reason I can't bind these.
       (:ieg "C-k" #'corfu-previous)
       (:ieg "C-p" #'corfu-previous))

(provide 'oo-corfu-config)
