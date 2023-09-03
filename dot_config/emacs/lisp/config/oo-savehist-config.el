(set! savehist-save-minibuffer-history t)
(set! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(set! savehist-autosave-interval (* 60 5))
(set! savehist-file (concat oo-cache-dir "savehist"))

(adding-to-list! savehist-additional-variables 'dogears-list)

(adding-to-list! savehist-additional-variables 'register-alist)

(provide 'oo-savehist-config)

