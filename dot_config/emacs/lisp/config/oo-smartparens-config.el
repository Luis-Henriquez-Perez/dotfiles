(set! sp-highlight-wrap-tag-overlay nil)
(set! sp-highlight-pair-overlay nil)
(set! sp-highlight-wrap-overlay nil)

(set! sp-show-pair-delay 0.2)

(sp-local-pair sp-lisp-modes "'" nil :actions nil)
(sp-local-pair sp-lisp-modes "`" "'" :when '(sp-in-string-p sp-in-comment-p))

(defhook! enable-smartparens-maybe (minibuffer-setup-hook)
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(provide 'oo-smartparens-config)
