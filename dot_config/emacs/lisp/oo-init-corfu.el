(oo-bind 'corfu-map "<tab>" #'corfu-next)
(oo-bind 'corfu-map [backtab] #'corfu-previous)
(oo-bind 'corfu-map "C-;" #'corfu-quick-complete)
(oo-bind 'corfu-map "C-j" #'corfu-next)
(oo-bind 'corfu-map "M-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-p" #'corfu-previous)

(set! corfu-quick1 "abcdefghijklmnopqrstuvxyz")

(oo-add-hook 'corfu-mode #'corfu-history-mode)

(set! corfu-quit-at-boundary nil)

(set! corfu-auto t)

(set! corfu-auto-delay 0.1)

(set! corfu-auto-prefix 1)

(set! corfu-bar-width 0)
