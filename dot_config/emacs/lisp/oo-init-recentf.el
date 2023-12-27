(oo-add-hook 'emacs-startup-hook #'recentf-mode)

(set! recentf-max-menu-items 0)

(set! recentf-max-saved-items 700)

(set! recentf-save-file (concat oo-cache-dir "recentf"))

(set! recentf-auto-cleanup (* 60 10))

(set! recentf-filename-handlers '(file-truename))

(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)

(oo-add-advice #'recentf-mode :around #'oo-funcall-silently)
(oo-add-advice #'recentf-cleanup :around #'oo-funcall-silently)
(oo-add-advice #'recentf-save-list :around #'oo-funcall-silently)

;; TODO: Add back =adjoin!= if I removed it.
(adjoin! recentf-filename-handlers #'abbreviate-file-name)

(adjoin! recentf-filename-handlers #'substring-no-properties)
