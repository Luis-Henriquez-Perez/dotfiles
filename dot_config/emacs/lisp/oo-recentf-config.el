(set! recentf-max-menu-items 0)
(set! recentf-max-saved-items 700)
(set! recentf-save-file (concat oo-cache-dir "recentf"))
(set! recentf-auto-cleanup (* 60 10))
(set! recentf-filename-handlers '(file-truename))

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(oo-silence #'recentf-mode #'recentf-cleanup #'recentf-save-list)

(adding-to-list! recentf-filename-handlers #'abbreviate-file-name)

(adding-to-list! recentf-filename-handlers #'substring-no-properties)

(provide 'oo-recentf-config)
