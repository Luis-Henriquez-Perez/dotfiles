(oo-add-hook 'on-first-input-hook #'savehist-mode)

(set! savehist-save-minibuffer-history t)
(set! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(set! savehist-autosave-interval (* 60 5))
(set! savehist-file (concat oo-cache-dir "savehist"))

(adjoin! savehist-additional-variables 'register-alist)

(defadvice! remove-properties-from-kill-ring (before savehist-save)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))
