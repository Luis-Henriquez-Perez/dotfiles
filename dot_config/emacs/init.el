;; -*- lexical-binding: t -*-

;; This variable controls how often.  Setting it to =most-positive-fixnum=, a very big
;; number, essentially disables garbage collection.  The garbage collection is later
;; reset to a reasonable value.
(setq gc-cons-threshold most-positive-fixnum)

(when (not (or (daemonp) noninteractive init-file-debug))
  ;; During startup emacs renders several messages.  The messages may be important so
  ;; we don't want to get rid of them altogether.  This code prevents these message
  ;; from flashing on the screen.  However they are still logged to the =*messages*=
  ;; buffer.
  (when (display-graphic-p)
    (setq-default inhibit-redisplay t)
    (setq-default inhibit-message t)
    (defun reset-inhibit-vars ()
      (setq-default inhibit-redisplay nil)
      (setq-default inhibit-message nil)
      (redraw-frame))
    (add-hook 'window-setup-hook #'reset-inhibit-vars)
    (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
      (and init-file-had-error (reset-inhibit-vars))))

  ;; Suppress file handlers operations at startup. The value of `file-name-handler-alist' is
  ;; consulted on each call to `require' and `load'. Here I disable it (set it to nil) and schedule it
  ;; to be re-enabled after startup. I got this from centaur emacs.
  (defvar original-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (defun emacs-startup-hook&restore-file-name-handler-alist ()
    (setq file-name-handler-alist original-file-name-handler-alist)
    (makunbound 'original-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'emacs-startup-hook&restore-file-name-handler-alist))

;; Put the base directory into the `load-path', making sure it's at the front.
(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'oo-bootstrap-elpaca)

(let (font)
  (setq font (or (font-spec :name "Iosevka Comfy Wide"
			                :weight 'normal
			                :slant 'normal
			                :size 15)
	             (font-spec :name "SpaceMono Nerd Font"
			                :weight 'normal
			                :slant 'normal
			                :size 15)
		         (font-spec :name "iMWritingMono Nerd Font Mono"
			                :weight 'normal
			                :slant 'normal
			                :size 15)))
  (set-face-attribute 'default nil :font font))

;; The package `el-init' is one that I consider underused.
(require 'el-init)
(setq el-init-lazy-init-regexp "^oo-\\(.+\\)-config$")

(el-init-load (locate-user-emacs-file "lisp/")
              :subdirectories '("base" "config")
              :wrappers '(el-init-require/lazy el-init-require/record-error el-init-require/benchmark))


;; (setq-default fill-column 100)

;; (require 'log4e)
;; (log4e:deflogger "oo" "%t [%l] %m" "%H:%M:%S")

;; (defalias 'oo-log 'oo--log-info)
;; (defalias 'oo-log-info 'oo--log-info)
;; (defalias 'oo-log-warn 'oo--log-warn)
;; (defalias 'oo-log-debug 'oo--log-debug)
;; (defalias 'oo-log-error 'oo--log-error)
;; (defalias 'oo-log-fatal 'oo--log-fatal)
;; (defalias 'oo/open-log 'oo--log-open-log)

;; (oo--log-set-level 'trace)

;; (oo--log-enable-logging)

;; ;; `prog-mode-hook'
;; (oo-add-hook 'prog-mode-hook #'smartparens-strict-mode)
;; (oo-add-hook 'prog-mode-hook #'corfu-mode)
;; (oo-add-hook 'prog-mode-hook #'lispyville-mode)
;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; (oo-add-hook '(prog-mode-hook reb-mode-hook) #'rainbow-delimiters-mode)
;; (oo-add-hook 'prog-mode-hook #'hs-minor-mode)
;; (oo-add-hook 'prog-mode-hook #'auto-fill-mode)

;; ;; `emacs-startup-hook'
;; (oo-add-hook 'emacs-startup-hook #'which-key-mode)
;; (oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)
;; (oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)
;; (oo-add-hook 'emacs-startup-hook #'recentf-mode)

;; ;; `on-first-input-hook'
;; (oo-add-hook 'on-first-input-hook #'vertico-mode)
;; (oo-add-hook 'on-first-input-hook #'savehist-mode)
;; (oo-add-hook 'on-first-input-hook #'dogears-mode)

;; (oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

;; ;; `after-init-hook'
;; (oo-add-hook 'after-init-hook #'require 'evil :depth 10)
;; (oo-add-hook 'after-init-hook #'evil-mode :depth 90)
;; (oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)
;; (oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)
;; (oo-add-hook 'after-init-hook #'load-theme 'modus-operandi)
;; (oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)

;; ;; `on-first-file-hook'
;; (oo-add-hook 'on-first-file-hook #'save-place-mode)
;; (oo-add-hook 'on-first-file-hook #'super-save-mode)

;; ;; `emacs-lisp-mode-hook'
;; (oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
;; (oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

;; (oo-add-hook 'vertico-mode-hook #'marginalia-mode)
;; (oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

;; ;; `text-mode-hook'
;; (oo-add-hook '(prog-mode-hook text-mode-hook) #'evil-surround-mode)
;; (oo-add-hook 'text-mode-hook #'flyspell-mode)
;; (oo-add-hook 'text-mode-hook #'auto-fill-mode)
;; (oo-add-hook 'text-mode-hook #'visual-line-mode)

;; Note that this can't work with `on-first-input-hook' because which-key
;; doesn't happen on first keypress.  It needs to be in the startup hook.

;; (set! consult-preview-key nil)

;; (when (display-graphic-p)
;;   (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode))
;; ;; Declaratively add hooks.
;; ;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

;; (set! crm-separator ",")

;; (oo-popup-at-bottom "\\*Vertico")

;; (defun oo-set-window-divider-face (&rest _)
;;   "Set the window divider face."
;;   (set-face-foreground 'window-divider "black"))

;; (oo-add-advice #'load-theme :after #'oo-set-window-divider-face)

;; (set! window-divider-default-places t)

;; (set! lambda-themes-set-italic-comments nil)
;; (set! lambda-themes-set-italic-keywords nil)

;; (set! lambda-themes-set-variable-pitch nil)

;; (set! modus-themes-headings '((1 . (rainbow light 1.4))
;; 			                  (2 . (rainbow light 1.3))
;; 			                  (3 . (rainbow light 1.2))
;; 			                  (4 . (rainbow light 1.1))
;; 			                  (t . (rainbow light 1.0))))

;; (oo-add-hook 'dired-mode-hook #'dired-omit-mode)

;; (set! dired-recursive-copies 'always)
;; (set! dired-recursive-deletes 'always)

;; (oo-call-after-load 'dirvish #'dirvish-override-dired-mode 1)

;; (set! dirvish-use-mode-line nil)

;; (set! dirvish-attributes '(file-size all-the-icons subtree-state))

;; (bind! (:alt dired dirvish))

;; (set! dirvish-default-layout nil)

;; (set! idle-require-symbols (append '(em-alias em-banner em-basic em-cmpl em-glob em-hist em-ls em-prompt em-term em-unix)
;;                                    idle-require-symbols))

;; (oo-popup-at-bottom "\\*[Hh]elp")

;; ;; By default this variable is set to =t= which means, save bookmarks whenever emacs is killed.  An
;; ;; integer mans to save my bookmarks whenever I set a bookmark.  That way I minimize the chances of
;; ;; losing them if emacs crashes.
;; (set! bookmark-save-flag 1)

;; (set! avy-style 'pre)
;; (set! avy-keys (number-sequence 97 122))

;; (set! rainbow-delimiters-max-face-count 9)

;; (set! save-place-file (concat oo-cache-dir "saveplace"))
;; (set! save-place-limit nil)

;; (adding-to-list! dogears-ignore-modes 'dashboard-mode)

;; (after! org
;;   (defun oo-org-src-buffer-p () (bound-and-true-p org-src-mode))
;;   (adding-to-list! dogears-ignore-places-functions #'oo-org-src-buffer-p))

;; (oo-silence #'flyspell-mode)

;; (setq-default bookmark-default-file (expand-file-name "bookmarks" oo-cache-dir))

;; (set! ispell-program-name (or (executable-find "hunspell") (executable-find "ispell")))
