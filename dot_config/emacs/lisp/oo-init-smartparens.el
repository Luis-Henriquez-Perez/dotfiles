(set! sp-highlight-wrap-tag-overlay nil)

(set! sp-highlight-pair-overlay nil)

(set! sp-highlight-wrap-overlay nil)

(set! sp-show-pair-delay 0.2)

(defhook! enable-smartparens-maybe (minibuffer-setup-hook)
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(oo-add-hook 'text-mode-hook 'smartparens-mode)

(oo-add-hook 'prog-mode-hook #'smartparens-mode)
