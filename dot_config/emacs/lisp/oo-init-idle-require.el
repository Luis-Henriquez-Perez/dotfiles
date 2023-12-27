;; (defrecipe! idle-require :fetcher github :repo "nschum/idle-require.el" :ref "33592bb")

(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)

(oo-add-advice #'idle-require-load-next :around #'oo-funcall-silently)

(set! idle-require-load-break 5)

(set! idle-require-idle-delay 10)
