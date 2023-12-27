(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
(oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

(oo-add-hook 'emacs-startup-hook #'vertico-mode)

(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

(set! vertico-quick1 "asdf")
(set! vertico-quick2 "jkl;")

(set! vertico-count-format '("%-6s " . "%2$s"))
(set! vertico-count 15)

(oo-popup-at-bottom "\\*Vertico")

(oo-bind 'vertico-map :i "TAB" #'vertico-next)
(oo-bind 'vertico-map :i "C-k" #'vertico-previous)
(oo-bind 'vertico-map :i "C-j" #'vertico-next)
(oo-bind 'vertico-map :i ";" #'vertico-quick-exit)
(oo-bind 'vertico-map :i "C-;" #'vertico-quick-exit)
(oo-bind 'vertico-map :i [backtab] #'vertico-previous)

(oo-bind 'vertico-map :i "C-o" #'embark-act)
(oo-bind 'vertico-map :i "C-o" #'embark-collect)
(oo-bind 'vertico-map :n "C-o" #'embark-collect)
