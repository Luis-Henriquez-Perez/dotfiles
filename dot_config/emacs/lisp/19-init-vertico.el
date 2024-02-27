(require 'on)

(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

;; Try to split up loading.
(defhook! emacs-startup-hook&require-vertico ()
  (require 'vertico nil t))

(oo-add-hook 'on-first-input-hook #'vertico-mode)

(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

(opt! vertico-quick1 "asdf")
(opt! vertico-quick2 "jkl;")

(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)

(defhook! vertico-mode-hook&enable-orderless ()
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))
    (aset! '(orderless-strict-leading-initialism orderless-initialism orderless-regexp))
    (set! orderless-matching-styles it)))

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

(provide '19-init-vertico)
