;; ;;;; setup hooks
;; ;;;;; hooks
;; ;;;;;; prog-mode-hook
;; (oo-add-hook 'prog-mode-hook #'smartparens-strict-mode)
;; (oo-add-hook 'prog-mode-hook #'corfu-mode)
;; (oo-add-hook 'prog-mode-hook #'lispyville-mode)
;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; (oo-add-hook '(prog-mode-hook reb-mode-hook) #'rainbow-delimiters-mode)
;; (oo-add-hook 'prog-mode-hook #'hs-minor-mode)
;; (oo-add-hook 'prog-mode-hook #'auto-fill-mode)
;; ;;;;;; emacs-startup-hook
;; (oo-add-hook 'emacs-startup-hook #'which-key-mode)
;; (oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)
;; (oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)
;; (oo-add-hook 'emacs-startup-hook #'recentf-mode)
;; ;;;;;; on-first-input-hook
;; (oo-add-hook 'on-first-input-hook #'vertico-mode)
;; (oo-add-hook 'on-first-input-hook #'savehist-mode)
;; (oo-add-hook 'on-first-input-hook #'dogears-mode)
;; (oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)
;; ;;;;;; after-init-hook
;; ;; (oo-call-hook 'after-init-hook #'require 'evil :depth 10)
;; (oo-add-hook 'after-init-hook #'evil-mode :depth 90)
;; (oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)
;; (oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)
;; (oo-add-hook 'after-init-hook #'load-theme 'modus-operandi)
;; (oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)
;; ;;;;;; on-first-file-hook
;; ;; (oo-add-hook 'on-first-file-hook #'save-place-mode)
;; ;; (oo-add-hook 'on-first-file-hook #'super-save-mode)
;; ;;;;;; emacs-lisp-mode-hook
;; ;; (oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
;; ;; (oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

;; ;; (oo-add-hook 'vertico-mode-hook #'marginalia-mode)
;; ;; (oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)
;; ;;;;;; text-mode-hook
;; ;; (oo-add-hook '(prog-mode-hook text-mode-hook) #'evil-surround-mode)
;; ;; (oo-add-hook 'text-mode-hook #'flyspell-mode)
;; ;; (oo-add-hook 'text-mode-hook #'auto-fill-mode)
;; ;; (oo-add-hook 'text-mode-hook #'visual-line-mode)
