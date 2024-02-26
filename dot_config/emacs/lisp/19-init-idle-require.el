(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)

(oo-add-advice #'idle-require-load-next :around #'oo-funcall-silently)

(opt! idle-require-load-break 5)

(opt! idle-require-idle-delay 10)

(provide '19-init-idle-require)
;; 19-idle-require.el ends here
