(require 'savehist)

(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))

(opt! savehist-additional-variables (cl-adjoin 'register-alist savehist-additional-variables))

(defadvice! savehist-save@BFremove-kill-ring-properties (&rest _)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))

(oo-add-hook 'on-first-input-hook #'savehist-mode)
