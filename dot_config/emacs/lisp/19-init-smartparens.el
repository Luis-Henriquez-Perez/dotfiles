(opt! sp-highlight-wrap-tag-overlay nil)

(opt! sp-highlight-pair-overlay nil)

(opt! sp-highlight-wrap-overlay nil)

(opt! sp-show-pair-delay 0.2)

(defhook! minibuffer-setup-hook&enable-smartparens-maybe ()
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(oo-add-hook 'text-mode-hook #'smartparens-mode)

(oo-add-hook 'prog-mode-hook #'smartparens-mode)

(oo-bind 'oo-toggle-map "s" #'smartparens-mode)

(provide '19-init-smartparens)
;; 19-init-smartparens.el ends here
