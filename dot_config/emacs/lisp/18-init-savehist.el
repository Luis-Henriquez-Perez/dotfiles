(oo-add-hook 'on-first-input-hook #'savehist-mode)

(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))
(opt! savehist-file (concat oo-cache-dir "savehist"))

(cl-pushnew 'register-alist savehist-additional-variables)

(defadvice! remove-properties-from-kill-ring (before savehist-save)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))

(provide '80-init-savehist)
