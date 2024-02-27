(oo-bind 'corfu-map "<tab>" #'corfu-next)
(oo-bind 'corfu-map [backtab] #'corfu-previous)
(oo-bind 'corfu-map "C-;" #'corfu-quick-complete)
(oo-bind 'corfu-map "C-j" #'corfu-next)
(oo-bind 'corfu-map "M-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-p" #'corfu-previous)

(opt! corfu-quick1 "abcdefghijklmnopqrstuvxyz")

(oo-add-hook 'prog-mode-hook #'corfu-mode)
(oo-add-hook 'corfu-mode-hook #'corfu-history-mode)

(opt! corfu-quit-at-boundary nil)

(opt! corfu-auto t)

(opt! corfu-auto-delay 0.1)

(opt! corfu-auto-prefix 1)

(opt! corfu-bar-width 0)

(provide '19-init-corfu)
