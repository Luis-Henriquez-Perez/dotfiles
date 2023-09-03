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

(require 'oo-base-vars)

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

(require 'cl-lib)

;; Set the settings as soon as possible so we can avoid any GUI display.
;; (require 'oo-base-settings)

;; ;; Add the base directory to the load-path.
;; (require 'oo-base-font)

;; (oo-initialize-base-font oo-font-file)

;; (require 'oo-base-packages)
;; (oo-bootstrap-packages oo-package-dir oo-recipe-file)

;; (require 'oo-base-library)

;; ;; The package `el-init' is one that I consider underused.
;; (require 'el-init)
;; (setq el-init-lazy-init-regexp "^oo-\\(.+\\)-config$")

;; (el-init-load oo-lisp-dir
;;               :subdirectories nil
;;               :wrappers (when oo-debug-p '(el-init-require/record-error el-init-require/benchmark)))
(require 'oo-base-settings)

(log4e:deflogger "oo" "%t [%l] %m" "%H:%M:%S")

(defalias 'oo-log 'oo--log-info)
(defalias 'oo-log-info 'oo--log-info)
(defalias 'oo-log-warn 'oo--log-warn)
(defalias 'oo-log-debug 'oo--log-debug)
(defalias 'oo-log-error 'oo--log-error)
(defalias 'oo-log-fatal 'oo--log-fatal)
(defalias 'oo/open-log 'oo--log-open-log)

(oo--log-set-level 'trace)

(oo--log-enable-logging)

(require 'oo-base-utils)

(require 'oo-modification-macros)

(require 'oo-autoload)

(require 'oo-block-macro)

(require 'oo-base-definers)

(require 'oo-set)

(require 'oo-after-load)

(require 'oo-hook)

(require 'oo-advice)

;; `prog-mode-hook'
(oo-add-hook 'prog-mode-hook #'smartparens-strict-mode)
(oo-add-hook 'prog-mode-hook #'corfu-mode)
(oo-add-hook 'prog-mode-hook #'lispyville-mode)
(oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
(oo-add-hook '(prog-mode-hook reb-mode-hook) #'rainbow-delimiters-mode)
(oo-add-hook 'prog-mode-hook #'hs-minor-mode)
(oo-add-hook 'prog-mode-hook #'auto-fill-mode)

;; `emacs-startup-hook'
(oo-add-hook 'emacs-startup-hook #'which-key-mode)
(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)
(oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)
(oo-add-hook 'emacs-startup-hook #'recentf-mode)

;; `on-first-input-hook'
(oo-add-hook 'on-first-input-hook #'vertico-mode)
(oo-add-hook 'on-first-input-hook #'savehist-mode)
(oo-add-hook 'on-first-input-hook #'dogears-mode)

(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

;; `after-init-hook'
(oo-add-hook 'after-init-hook #'require 'evil :depth 10)
(oo-add-hook 'after-init-hook #'evil-mode :depth 90)
(oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)
(oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)
(oo-add-hook 'after-init-hook #'load-theme 'modus-operandi)
(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)

;; `on-first-file-hook'
(oo-add-hook 'on-first-file-hook #'save-place-mode)
(oo-add-hook 'on-first-file-hook #'super-save-mode)

;; `emacs-lisp-mode-hook'
(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

;; `text-mode-hook'
(oo-add-hook '(prog-mode-hook text-mode-hook) #'evil-surround-mode)
(oo-add-hook 'text-mode-hook #'flyspell-mode)
(oo-add-hook 'text-mode-hook #'auto-fill-mode)
(oo-add-hook 'text-mode-hook #'visual-line-mode)

(require 'oo-bind-macro)

(require 'oo-base-leaders)

;; Note that this can't work with `on-first-input-hook' because which-key
;; doesn't happen on first keypress.  It needs to be in the startup hook.

(bind! (:map oo-override-mode-map)
       (:g   oo-emacs-leader-key  #'oo/leader-prefix-command)
       (:i   oo-insert-leader-key #'oo/leader-prefix-command)
       (:nmv oo-normal-leader-key #'oo/leader-prefix-command))

(set! idle-require-load-break 5)
(set! idle-require-idle-delay 10)

(set! gcmh-high-cons-threshold (* 8 1024 1024))
(set! gcmh-low-cons-threshold (* 4 1024 1024))
(set! gcmh-idle-delay 'auto)

(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
	          (get-buffer-create "*scratch*"))
    (oo-log-info "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)

;; (defhook! minibuffer-setup-hook&boost-garbage-collection ()
;;   "Boost garbage collection settings to `gcmh-high-cons-threshold"
;;   (setq gc-cons-threshold gcmh-high-cons-threshold))

;; (defhook! minibuffer-exit-hook&defer-garbage-collection (minibuffer-exit-hook :append t)
;;   "Reset garbage collection settings to `gcmh-low-cons-threshold'."
;;   (setq gc-cons-threshold gcmh-low-cons-threshold))

(oo-call-after-load 'helm #'require 'oo-helm-config)

(set! consult-preview-key nil)

(when (display-graphic-p)
  (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode))
;; Declaratively add hooks.
;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

(set! crm-separator ",")

(set! vertico-quick1 "abcdefghijklmnopqrstuvwxyz")

(set! vertico-count-format '("%-6s " . "%2$s"))
(set! vertico-count 15)

(oo-popup-at-bottom "\\*Vertico")

(defun oo-set-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

(oo-add-advice #'load-theme :after #'oo-set-window-divider-face)

(set! window-divider-default-places t)

(set! lambda-themes-set-italic-comments nil)
(set! lambda-themes-set-italic-keywords nil)

(set! lambda-themes-set-variable-pitch nil)

(set! modus-themes-headings '((1 . (rainbow light 1.4))
			                  (2 . (rainbow light 1.3))
			                  (3 . (rainbow light 1.2))
			                  (4 . (rainbow light 1.1))
			                  (t . (rainbow light 1.0))))

(oo-add-hook 'dired-mode-hook #'dired-omit-mode)

(set! dired-recursive-copies 'always)
(set! dired-recursive-deletes 'always)

(oo-call-after-load 'dirvish #'dirvish-override-dired-mode 1)

(set! dirvish-use-mode-line nil)

(set! dirvish-attributes '(file-size all-the-icons subtree-state))

(bind! (:alt dired dirvish))

(set! dirvish-default-layout nil)

(set! idle-require-symbols (append '(em-alias em-banner em-basic em-cmpl em-glob em-hist em-ls em-prompt em-term em-unix)
                                   idle-require-symbols))

(oo-popup-at-bottom "\\*[Hh]elp")

(oo-call-after-load 'org #'require 'oo-org-config)

(setq corfu-bar-width 0)

(set! corfu-auto-prefix 1)

(set! corfu-quit-at-boundary nil)
(set! corfu-auto t)
(set! corfu-auto-delay 0.1)

(setq-default fill-column 100)

(defadvice! disable-old-themes (around load-theme)
  "Disable old themes before loading new ones."
  (:args orig-fn &rest args)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(set! bookmark-save-flag 1)

(set! avy-style 'pre)
(set! avy-keys (number-sequence 97 122))

(defsubst +captain-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss (point))))

(defun! +captain-prog-mode-sentence-start-function ()
  "Return point at the start of the last sentence.
Mean to be used as the value of `captain-predicate'."
  (cl-assert (require 'smartparens nil 'noerror))
  (awhen (car (bounds-of-thing-at-point 'sentence))
    (pushing! points it))
  (acond ((save-excursion (and (comment-beginning) (point)))
          (pushing! points it))
         ((and (nth 8 (syntax-ppss (point))) (sp-in-docstring-p nil nil 'string))
          (pushing! points it)))
  (apply #'max points))

(defhook! auto-capitalize-sentences-in-docstrings-and-comments (prog-mode-hook)
  (captain-mode 1)
  (setq-local captain-predicate #'+captain-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'+captain-prog-mode-sentence-start-function))

(defhook! auto-capitalize-sentences (text-mode-hook)
  (captain-mode 1)
  (setq-local captain-predicate (lambda () t)))

(set! rainbow-delimiters-max-face-count 9)

(set! save-place-file (concat oo-cache-dir "saveplace"))
(set! save-place-limit nil)

(set! super-save-auto-save-when-idle t)
(set! super-save-idle-duration 5)

(oo-call-after-load 'evil #'require 'oo-evil-config)

(oo-call-after-load 'evil-easymotion #'oo-evil-easymotion-config)

(adding-to-list! dogears-ignore-modes 'dashboard-mode)

(defun oo-org-src-buffer-p () (bound-and-true-p org-src-mode))
(adding-to-list! dogears-ignore-places-functions #'oo-org-src-buffer-p)

(oo-silence #'flyspell-mode)

(setq-default bookmark-default-file (expand-file-name "bookmarks" oo-cache-dir))

(defun! oo/set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (let! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

(set! transient-levels-file (concat oo-cache-dir "transient/levels"))
(set! transient-values-file (concat oo-cache-dir "transient/values"))
(set! transient-history-file (concat oo-cache-dir "transient/history"))

(set! ispell-program-name (or (executable-find "hunspell") (executable-find "ispell")))
