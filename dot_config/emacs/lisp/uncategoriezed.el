
;; (require 'el-init)
;; (el-init-load (expand-file-name "lisp" user-emacs-directory)
;;               :subdirectories '(".")
;;               :wrappers '(el-init-require/record-error))
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

;; (defvar oo-window-map (make-sparse-keymap))
;; (define-prefix-command 'oo/window-prefix-command 'oo-window-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "window" "w" #'oo/window-prefix-command))

;; (bind! (:map oo-window-map)
;;        ("M" #'maximize-window)
;;        ("m" #'minimize-window)
;;        ("b" #'balance-windows)
;;        ("z" #'zoom-window-zoom))

;; (bind! (:map oo-window-map)
;;        (:wk "left"         "h" #'windmove-left)
;;        (:wk "down"         "j" #'windmove-down)
;;        (:wk "up"           "k" #'windmove-up)
;;        (:wk "right"        "l" #'windmove-right)
;;        (:wk "ace"          "o" #'ace-window)
;;        (:wk "delete"       "d" #'delete-window)
;;        (:wk "delete others""D" #'delete-other-windows)
;;        (:wk "vsplit"       "v" #'split-window-horizontally)
;;        (:wk "split"        "s" #'split-window-vertically)
;;        (:wk "vsplit+focus" "V" #'oo/split-window-right-and-focus)
;;        (:wk "split+focus"  "v" #'oo/split-window-below-and-focus)
;;        (:wk "transpose"    "t" #'transpose-frame))

;; (defvar oo-buffer-map (make-sparse-keymap))
;; (define-prefix-command 'oo/buffer-prefix-command 'oo-buffer-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "buffer" "b" #'oo/buffer-prefix-command))

;; (bind! (:map oo-buffer-map)
;;        ("w" #'save-buffer)
;;        ("x" #'buffer-expose)
;;        ((:wk "kill" :prefix "k")
;; 	("c" #'kill-current-buffer))
;;        ("j" #'pop-to-buffer)
;;        ("b" #'switch-to-buffer)
;;        ("n" #'next-buffer)
;;        ("p" #'previous-buffer)
;;        ("d" #'display-buffer))

;; (defvar oo-quit-map (make-sparse-keymap))
;; (define-prefix-command 'oo/quit-prefix-command 'oo-quit-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "quit" "q" #'oo/quit-prefix-command))

;; (defvar oo-quick-map (make-sparse-keymap))
;; (define-prefix-command 'oo/quick-prefix-command 'oo-quick-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "quick" "j" #'oo/quick-prefix-command))

;; (defvar oo-app-map (make-sparse-keymap))
;; (define-prefix-command 'oo/app-prefix-command 'oo-app-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "app" "a" #'oo/app-prefix-command))

;; (defvar oo-toggle-map (make-sparse-keymap))
;; (define-prefix-command 'oo/toggle-prefix-command 'oo-toggle-map)
;; (bind! (:map oo-leader-map "t" #'oo/toggle-prefix-command))

;; (defvar oo-help-map (make-sparse-keymap))
;; (define-prefix-command 'oo/help-prefix-command 'oo-help-map)
;; (bind! (:map oo-leader-map "h" #'oo/help-prefix-command))

;; (defvar oo-package-map (make-sparse-keymap))
;; (define-prefix-command 'oo/package-prefix-command 'oo-package-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "package" "p" #'oo/package-prefix-command))

;; (bind! (:map oo-quit-map)
;;        (:wk "quit emacs" "q" #'save-buffers-kill-emacs))

;; (bind! (:map oo-quit-map)
;;        (:wk "quit and restart" "r" #'restart-emacs))

;; ;; Note that this can't work with `on-first-input-hook' because which-key
;; ;; doesn't happen on first keypress.  It needs to be in the startup hook.
;; (oo-add-hook 'emacs-startup-hook #'which-key-mode)

;; (set! which-key-sort-uppercase-first nil)
;; (set! which-key-max-display-columns nil)
;; (set! which-key-add-column-padding 1)
;; (set! which-key-min-display-lines 6)
;; (set! which-key-side-window-slot -10)
;; (set! which-key-sort-order #'which-key-prefix-then-key-order)
;; (set! which-key-popup-type 'side-window)
;; (set! which-key-idle-delay 0.8)

;; ;; (set! line-spacing 3 :hook which-key-init-buffer-hook :local t)

;; (set! which-key-show-transient-maps t)
;; (set! which-key-show-operator-state-maps t)

;; (bind! (:map oo-override-mode-map)
;;        (:g   oo-emacs-leader-key  #'oo/leader-prefix-command)
;;        (:i   oo-insert-leader-key #'oo/leader-prefix-command)
;;        (:nmv oo-normal-leader-key #'oo/leader-prefix-command))

;; (oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)

;; (set! idle-require-load-break 5)
;; (set! idle-require-idle-delay 10)

;; (set! gcmh-high-cons-threshold (* 8 1024 1024))
;; (set! gcmh-low-cons-threshold (* 4 1024 1024))

;; (set! gcmh-idle-delay 'auto)

;; (oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)

;; (defvar oo-initial-buffer-choice-hook nil
;;   "Hook run to choose initial buffer.
;; Each hook should return either a buffer to be displayed or a boolean.
;; For what buffer is displayed in the case of a boolean see
;; `initial-buffer-choice'.")

;; (defun oo-run-initial-buffer-choice-hook ()
;;   "Run `oo-initial-buffer-choice-hook'."
;;   (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
;; 	      (get-buffer-create "*scratch*"))
;;     (oo-log-info "set initial buffer to %s" (buffer-name))))

;; (setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)

;; (defhook! minibuffer-setup-hook&boost-garbage-collection ()
;;   "Boost garbage collection settings to `gcmh-high-cons-threshold"
;;   (setq gc-cons-threshold gcmh-high-cons-threshold))

;; (defhook! minibuffer-exit-hook&defer-garbage-collection (minibuffer-exit-hook :append t)
;;   "Reset garbage collection settings to `gcmh-low-cons-threshold'."
;;   (setq gc-cons-threshold gcmh-low-cons-threshold))

;; (set! consult-preview-key nil)

;; (oo-add-hook 'emacs-startup-hook #'recentf-mode)

;; (set! recentf-max-menu-items 0)
;; (set! recentf-max-saved-items 700)
;; (set! recentf-save-file (concat oo-cache-dir "recentf"))
;; (set! recentf-auto-cleanup (* 60 10))
;; (set! recentf-filename-handlers '(file-truename))

;; (oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
;; (oo-add-hook 'kill-emacs-hook #'recentf-save-list)

;; (oo-silence #'recentf-mode #'recentf-cleanup #'recentf-save-list)

;; (oo-add-hook 'vertico-mode-hook #'marginalia-mode)
;; (when (display-graphic-p)
;;   (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode))
;; ;; Declaratively add hooks.
;; ;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

;; (set! crm-separator ",")

;; (set! vertico-quick1 "abcdefghijklmnopqrstuvwxyz")

;; (set! vertico-count-format '("%-6s " . "%2$s"))
;; (set! vertico-count 15)

;; (oo-add-hook 'on-first-input-hook #'vertico-mode)

;; (oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

;; (oo-popup-at-bottom "\\*Vertico")

;; (defhook! enable-orderless (vertico-mode-hook :expire t)
;;   (when (require 'orderless nil t)
;;     (setq completion-styles '(orderless))
;;     (setq completion-category-defaults nil)
;;     (setq completion-category-overrides '((file (styles partial-completion))))
;;     (alet '(orderless-strict-leading-initialism orderless-initialism orderless-regexp)
;;       (set! orderless-matching-styles it))))

;; (defun oo-dashboard-init-info (&rest _)
;;   (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))
;; (set! dashboard-init-info #'oo-dashboard-init-info)

;; (set! dashboard-banner-logo-title "Welcome!")
;; (set! dashboard-set-footer nil)
;; (set! dashboard-items nil)
;; (set! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))
;; (set! dashboard-center-content t)
;; ;; (set! dashboard-banner-logo-title "Welcome!")

;; (defhook! create-dashboard (oo-initial-buffer-choice-hook)
;;   (when (require 'dashboard nil t)
;;     (aprog1 (get-buffer-create dashboard-buffer-name)
;;       (with-current-buffer it
;; 	(dashboard-insert-startupify-lists)))))

;; (defun oo-set-window-divider-face (&rest _)
;;   "Set the window divider face."
;;   (set-face-foreground 'window-divider "black"))

;; (oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)

;; (oo-add-advice #'load-theme :after #'oo-set-window-divider-face)

;; (oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)

;; (set! window-divider-default-bottom-width 7)
;; (set! window-divider-default-right-width 7)

;; (setq window-divider-default-places t)

;; (oo-add-hook 'after-init-hook #'load-theme 'modus-operandi)

;; (set! lambda-themes-set-italic-comments nil)
;; (set! lambda-themes-set-italic-keywords nil)

;; (set! lambda-themes-set-variable-pitch nil)

;; (set! modus-themes-headings '((1 . (rainbow light 1.4))
;; 			                  (2 . (rainbow light 1.3))
;; 			                  (3 . (rainbow light 1.2))
;; 			                  (4 . (rainbow light 1.1))
;; 			                  (t . (rainbow light 1.0))))

;; (set! mu4e-compose-signature-auto-include t)
;; (set! mu4e-compose-format-flowed t)

;; (set! mu4e-headers-auto-update t)

;; (bind! (:map dired-mode-map)
;;        (:nm "h" #'dired-up-directory))

;; (oo-add-hook 'dired-mode-hook #'dired-omit-mode)

;; (set! dired-recursive-copies 'always)
;; (set! dired-recursive-deletes 'always)

;; (oo-call-after-load 'dirvish #'dirvish-override-dired-mode 1)

;; (set! dirvish-use-mode-line nil)

;; (set! dirvish-attributes '(file-size all-the-icons subtree-state))

;; (bind! (:map oo-app-map "d" #'dired))

;; (bind! (:alt dired dirvish))

;; (set! dirvish-default-layout nil)

;; (set! idle-require-symbols (append '(em-alias em-banner em-basic em-cmpl em-glob em-hist em-ls em-prompt em-term em-unix)
;;                                    idle-require-symbols))

;; (oo-popup-at-bottom "\\*eshell")

;; (set! eshell-directory-name (concat oo-cache-dir "eshell/"))
;; (set! eshell-history-file-name (concat eshell-directory-name "history"))

;; (set! eshell-banner-message "")

;; (set! eshell-hist-ignoredups t)

;; (set! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
;; (set! emms-directory (expand-file-name "emms/" oo-cache-dir))

;; (oo-call-after-load 'emms #'require 'emms-player-mpv)
;; (set! emms-player-list '(emms-player-mpv))

;; (oo-popup-at-bottom "\\*[Hh]elp")

;; (set! org-auto-tangle-default t)

;; (defun! +org/choose-capture-template ()
;;   "Choose a capture template."
;;   (interactive)
;;   (dolist (template org-capture-templates)
;;     (let! (keys name) template)
;;     (collecting! alist (cons name keys)))
;;   (let! selected (completing-read "capture template: " alist nil t))
;;   (org-capture nil (alist-get selected alist nil nil #'string=)))

;; (oo-popup-at-bottom "CAPTURE[^z-a]+")

;; (oo-popup-at-bottom "\\*Org Src")

;; (gv-define-expander org-ml-headline-get-section
;;   (lambda (do place)
;;     (gv-letplace (getter setter) place
;;       (funcall do `(org-ml-headline-get-section ,getter)
;;                (lambda (v)
;;                  (macroexp-let2 nil v v
;;                    `(progn
;;                       ,(funcall setter `(org-ml-headline-set-section ,v ,getter)))))))))

;; (bind! (:map org-mode-map)
;;        (:n "C" #'org-refile-copy))

;; (set! org-refile-target-verify-function (lambda () (not (+org-has-src-block-p))))

;; (defun! +org-has-src-block-p ()
;;   "Return non-nil if current headline has a source block."
;;   (save-excursion
;;     (let! beg (point))
;;     (let! end (or (outline-next-heading) (point-max)))
;;     (goto-char beg)
;;     (and (save-match-data (re-search-forward "^#\\+begin_src" end t)) t)))

;; (set! org-superstar-leading-bullet ?\s)
;; (set! org-superstar-special-todo-items t)
;; (set! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))
;; (oo-add-hook 'org-mode-hook #'org-superstar-mode)

;; (set! org-refile-use-outline-path 'file)

;; (defun +org-directory-files ()
;;   "Return a list of org files in the `org-directory'."
;;   (directory-files org-directory t "\\.org\\'"))

;; (set! org-directory (expand-file-name "~/dotfiles"))

;; (set! org-archive-location (concat org-directory "archive.org::"))

;; (set! org-default-notes-file null-device)

;; (set! org-refile-use-cache nil)

;; (setq org-outline-path-complete-in-steps nil)

;; (set! org-refile-targets '((+org-directory-files :maxlevel . 10)))

;; (set! org-archive-save-context-info nil)

;; (set! org-refile-allow-creating-parent-nodes t)

;; (set! org-src-preserve-indentation t)
;; (set! org-edit-src-content-indentation 0)

;; (set! org-src-ask-before-returning-to-edit-buffer nil)

;; (defun! +org/dwim-next-visible-heading ()
;;   (interactive)
;;   (+org/dwim-previous-visible-heading t))

;; (defun! +org/dwim-previous-visible-heading (&optional next)
;;   (interactive)
;;   (let! case-fold-search nil)
;;   (let! regexp "^\\*+\\(?: +[[:upper:]]+\\)?\\(?: +\\[#.]\\)?\\(?: +\\(.*?\\)\\)?")
;;   (if next
;;       (org-next-visible-heading 1)
;;     (org-previous-visible-heading 1))
;;   (org-back-to-heading)
;;   (save-match-data
;;     (looking-at regexp)
;;     (goto-char (match-beginning 1))))

;; (defun +org/disable-tangling ()
;;   "Disable tangling source block at point."
;;   (interactive)
;;   (org-entry-put (point) "HEADER-ARGS" ":tangle no"))

;; (set! org-babel-default-header-args
;;       '((:session . "none")
;;         (:results . "silent")
;;         (:exports . "code")
;;         (:mkdirp  . "yes")
;;         (:cache   .  "no")
;;         (:noweb   .  "no")
;;         (:hlines  .  "no")
;;         (:tangle  .  "no")))

;; (set! org-tags-column 80)

;; (defun! +org/open-heading-above (&optional below)
;;   "Insert a heading above the current heading."
;;   (interactive)
;;   (ignore-errors (org-back-to-heading))
;;   (let! level (if (org-at-heading-p)
;; 		          (car (org-heading-components))
;; 		        1))
;;   (when (and below (org-at-heading-p))
;;     (or (outline-next-heading) (org-end-of-subtree)))
;;   (let->>! headline
;;     (org-ml-build-headline! :level level)
;; 	(org-ml-headline-set-node-property "ID" (org-id-new)))
;;   (org-ml-insert (point) headline)
;;   (run-hooks 'org-insert-heading-hook))

;; (defun +org/open-heading-below ()
;;   (interactive)
;;   (+org/open-heading-above t))

;; (defun! +org/dwim-insert-src-block ()
;;   "Insert source block for the current headline if it does not already exist."
;;   (interactive)
;;   (let! lang (completing-read "Language: " (mapcar 'car org-src-lang-modes)))
;;   (let! headline (org-ml-parse-this-headline))
;;   (let! section (org-ml-headline-get-section headline))
;;   (when (--any-p (equal 'src-block (org-element-type it)) section) (return!))
;;   (let! src-block (org-ml-build-src-block :value "" :language lang))
;;   (snocing! (org-ml-headline-get-section headline) src-block)
;;   (org-ml-update-this-headline (-const headline)))

;; (defun! +org/dwim-edit-src-code ()
;;   (interactive)
;;   (mapc #'require '(edit-indirect org-ml))
;;   (unless (org-in-src-block-p) (org-next-block 1))
;;   (let! (beg end) (org-src--contents-area (org-ml-parse-this-element)))
;;   (let! parent-buffer (current-buffer))
;;   (edit-indirect-region beg end t))

;; (set! org-hide-emphasis-markers t)

;; (set! org-fontify-emphasized-text t)

;; (set! org-adapt-indentation nil)

;; (set! org-edit-src-persistent-message nil)

;; (defun! +org/update-tags ()
;;   "Update tags for the current entry."
;;   (interactive)
;;   (let->>! all
;;     (org-map-entries #'org-get-tags nil (+org-directory-files))
;;     (apply #'append)
;;     (-non-nil)
;;     (-uniq))
;;   (let! old (org-get-tags))
;;   (let! new (completing-read-multiple "Tags: " all))
;;   (let! updated (-uniq (append (-difference old new) (-difference new old))))
;;   (org-set-tags updated))

;; (set! org-id-track-globally t)
;; (set! org-id-locations-file (concat oo-cache-dir "org-id-locations"))

;; (set! org-id-method 'ts)

;; (set! org-src-window-setup 'plain)

;; (set! org-id-link-to-org-use-id t)

;; (defun! +org/add-tangle-header-arg ()
;;   "Add header arguments to current headline."
;;   (interactive)
;;   ;; Find tangle header arguments from other source blocks.
;;   (let! files (directory-files org-directory t "\\.org\\'"))
;;   (let! regexp ":tangle \\([^[:space:]]+\\)")
;;   (flet! tangle-file ()
;;     (awhen (org-entry-get (point) "HEADER-ARGS")
;;       (-second-item (s-match regexp (or it "")))))
;;   (let! header-args (-non-nil (-uniq (org-map-entries #'tangle-file nil files))))
;;   (let! choosen (completing-read "files: " header-args))
;;   ;; Now replace based on what was selected.
;;   (let! header-args (org-entry-get (point) "HEADER-ARGS" ))
;;   (org-entry-put (point) "HEADER-ARGS"))

;; (bind! (:map org-mode-map)
;;        (:n "TAB" #'oo/toggle-fold-subtree))

;; (defun! oo/toggle-fold-subtree ()
;;   "Toggle folding of current subtree."
;;   (interactive)
;;   (let! folded-p (outline-invisible-p (line-end-position)))
;;   (outline-toggle-children)
;;   (save-excursion
;;     (next-line 1)
;;     (when (org-at-drawer-p)
;;       (org-fold-hide-drawer-toggle))))

;; (defun +org/dwim-eval-src-block ()
;;   "Eval block contents."
;;   (interactive)
;;   (unless (org-at-heading-p)
;;     (user-error "Not in source block"))
;;   (save-window-excursion
;;     (org-babel-execute-subtree)))

;; (set! org-src-lang-modes (add-to-list 'org-src-lang-modes '("emacs-lisp" . emacs-lisp)))

;; (oo-add-hook 'org-insert-heading-hook #'evil-append 1)

;; (bind! (:map oo-toggle-map)
;;        ("f" #'oo/set-font-face))

;; (bind! (:map oo-app-map)
;;        ("E" #'restart-emacs-start-new-emacs))

;; (bind! ((:map oo-quick-map)
;;         (:wk "capture" "j" #'org-capture))
;;        ((:map oo-app-map)
;;         (:wk "capture" "a" #'org-capture)
;;         (:wk "capture" "j" #'org-capture)))

;; (bind! (:localleader org-mode-map)
;;        (:wk "execute subtree" "E" #'org-babel-execute-subtree)
;;        ("w" #'widen)
;;        (:wk "narrow" "n" #'org-narrow-to-subtree)
;;        (:wk "refile" "r" #'org-refile)
;;        (:wk "tangle" "t" #'org-babel-tangle))

;; (bind! (:map oo-leader-map)
;;        (:wk "eval" :prefix "e")
;;        ("e" #'eval-expression))

;; (bind! (:localleader emacs-lisp-mode-map)
;;        (:wk "eval" :prefix "e")
;;        ("r" #'lispy-eval-and-replace)
;;        ("b" #'eval-buffer)
;;        ("d" #'eval-defun)
;;        ("e" #'eval-expression)
;;        ("l" #'eval-last-sexp)
;;        ("p" #'eval-print-last-sexp))

;; (bind! (:map oo-app-map "e" #'eshell))

;; (bind! (:nm "+" #'text-scale-increase)
;;        (:nm "-" #'text-scale-decrease))

;; (bind! (:map oo-toggle-map)
;;        ("r" #'read-only-mode)
;;        ("t" #'load-theme)
;;        ("c" #'caps-lock-mode)
;;        ("r" #'redacted-mode)
;;        ("s" #'smartparens-mode)
;;        ("d" #'toggle-debug-on-error))

;; (bind! (:map oo-miscellany-map "k" #'bookmark-set))

;; (defvar oo-miscellany-map (make-sparse-keymap))
;; (define-prefix-command 'oo/miscellany-prefix-command 'oo-miscellany-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "miscellany" "k" #'oo/miscellany-prefix-command))

;; (bind! (:map oo-help-map)
;;        ("i" #'info)
;;        ("m" #'describe-mode)
;;        ("f" #'describe-function)
;;        ("v" #'describe-variable)
;;        ("h" #'describe-variable)
;;        ("C" #'describe-char)
;;        ("k" #'describe-key)
;;        ("a" #'apropos)
;;        ("w" #'woman))

;; (bind! (:when (require 'helpful nil t))
;;        (:alt describe-function helpful-callable)
;;        (:alt describe-command helpful-command)
;;        (:alt describe-variable helpful-variable)
;;        (:alt describe-key helpful-key))

;; (bind! (:map vertico-map)
;;        (:state insert)
;;        ("TAB" #'vertico-next)
;;        ("C-k" #'vertico-previous)
;;        ("C-j" #'vertico-next)
;;        (";" #'vertico-quick-exit)
;;        ("C-;" #'vertico-quick-exit)
;;        ([backtab] #'vertico-previous))

;; (bind! ((:map vertico-map)
;; 	(:i "C-o" #'embark-act)
;; 	(:i "C-," #'embark-collect))
;;        (:n "C-o" #'embark-act))

;; (bind! (:map oo-leader-map "SPC" #'execute-extended-command)
;;        (:map oo-override-mode-map :nmv ";" #'execute-extended-command)
;;        (:i "A-x" #'execute-extended-command)
;;        (:i "M-x" #'execute-extended-command))

;; (bind! (:map org-mode-map)
;;        (:n "j" #'+org/dwim-next-visible-heading)
;;        (:n "k" #'+org/dwim-previous-visible-heading))

;; (bind! (:map org-mode-map)
;;        (:n "p" #'ignore))

;; (bind! (:map org-mode-map)
;;        (:n "Y" #'org-copy-subtree)
;;        (:n "D" #'org-cut-subtree)
;;        (:n "P" #'org-paste-subtree))

;; (bind! (:map org-mode-map)
;;        (:n "R" #'org-refile))

;; (bind! (:map org-mode-map)
;;        (:n "b" #'+org/dwim-insert-src-block))

;; (bind! (:map org-mode-map)
;;        (:n "o" #'+org/open-heading-below)
;;        (:n "O" #'+org/open-heading-above))

;; (oo-add-hook 'prog-mode-hook #'smartparens-strict-mode)
;; (oo-add-hook 'prog-mode-hook #'corfu-mode)

;; (set! org-confirm-babel-evaluate nil)

;; (bind! (:map org-mode-map)
;;        (:n "e" #'+org/dwim-eval-src-block))

;; (bind! (:map org-mode-map)
;;        (:n "t" #'+org/update-tags))

;; (bind! (:map org-mode-map)
;;        (:n ">" #'org-demote-subtree)
;;        (:n "<" #'org-promote-subtree))

;; (bind! (:map org-mode-map)
;;        (:n "K" #'org-metaup)
;;        (:n "J" #'org-metadown))

;; (bind! (:map oo-window-map)
;;        (:wk "left"  "h" #'windmove-left)
;;        (:wk "down"  "j" #'windmove-down)
;;        (:wk "up"    "k" #'windmove-up)
;;        (:wk "right" "l" #'windmove-right))

;; (bind! (:map oo-window-map)
;;        (:wk "delete"        "d" #'delete-window)
;;        (:wk "delete others" "D" #'delete-other-windows))

;; (bind! (:map oo-window-map)
;;        (:wk "vsplit"       "v" #'split-window-horizontally)
;;        (:wk "split"        "s" #'split-window-vertically)
;;        (:wk "vsplit+focus" "V" #'oo/split-window-right-and-focus)
;;        (:wk "split+focus"  "v" #'oo/split-window-below-and-focus))

;; (bind! (:n "gr" 'evil-operator-eval))

;; (bind! (:when (require 'eros nil t))
;;        (:alt eval-last-sexp eros-eval-last-sexp))

;; (bind! (:map evil-inner-text-objects-map "f" #'evil-cp-inner-form)
;;        (:map evil-outer-text-objects-map "f" #'evil-cp-a-form))

;; (bind! (:minor-mode org-src-mode)
;;        (:localleader org-src-mode-map)
;;        ("," #'org-edit-src-exit)
;;        ("a" #'org-edit-src-abort)
;;        ("c" #'org-edit-src-exit))

;; (defun! +org/dwim-edit-src-code ()
;;   "Edit nearest source block."
;;   (interactive)
;;   (unless (org-in-src-block-p) (org-next-block 1))
;;   (call-interactively #'org-edit-src-code))

;; (bind! (:alt org-edit-src-code +org/dwim-edit-src-code))

;; (bind! (:localleader org-mode-map)
;;        ("," #'org-edit-src-code)
;;        ((:wk "edit" :prefix "e")
;; 	(:wk "source block" "s" #'org-edit-src-code)))

;; (bind! (:localleader emacs-lisp-mode-map)
;;        (:wk "macrostep" :prefix "m")
;;        ("e" #'macrostep-expand)
;;        ("c" #'macrostep-collapse)
;;        ("C" #'macrostep-collapse-all))

;; (oo-add-hook 'prog-mode-hook #'lispyville-mode)

;; (bind! (:map lispyville-mode-map)
;;        (:i "SPC" #'lispy-space)
;;        (:i ";" #'lispy-comment))

;; (setq corfu-bar-width 0)

;; (set! sp-show-pair-delay 0.2)
;; ;; fix paren highlighting in normal mode
;; ;; (set! sp-show-pair-from-inside nil)
;; ;; sp-cancel-autoskip-on-backward-movement nil

;; (set! corfu-quit-at-boundary nil)
;; (set! corfu-auto t)
;; (set! corfu-auto-delay 0.1)
;; (set! corfu-auto-prefix 2)

;; (setq-default fill-column 100)

;; (set! sp-highlight-wrap-tag-overlay nil)
;; (set! sp-highlight-pair-overlay nil)
;; (set! sp-highlight-wrap-overlay nil)

;; (defadvice! disable-old-themes (around load-theme)
;;   "Disable old themes before loading new ones."
;;   (:args orig-fn &rest args)
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (apply orig-fn args))

;; (bind! (:state visual)
;;        ("V" #'er/contract-region)
;;        ("v" #'er/expand-region))

;; (defvar oo-file-map (make-sparse-keymap))
;; (define-prefix-command 'oo/file-prefix-command 'oo-file-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "file" "f" #'oo/file-prefix-command))

;; (bind! (:when (require 'consult nil t))
;;        (:alt switch-to-buffer consult-buffer)
;;        (:alt yank-pop consult-yank-pop)
;;        (:alt apropos consult-apropos)
;;        (:alt man consult-man))

;; (defvar oo-search-map (make-sparse-keymap))
;; (define-prefix-command 'oo/search-prefix-command 'oo-search-map)
;; (bind! (:map oo-leader-map)
;;        (:wk "search" "s" #'oo/search-prefix-command))

;; (bind! (:map oo-search-map)
;;        (:wk "line"        "f" #'consult-line)
;;        (:wk "line"        "s" #'consult-line)
;;        (:wk "line"        "l" #'consult-line)
;;        (:wk "outline"     "h" #'consult-outline)
;;        (:wk "org heading" "o" #'consult-org-heading))

;; (bind! (:prefix "g")
;;        (:n "," #'lispyville-comment-or-uncomment)
;;        (:n "c" #'lispyville-comment-and-clone-dwim)
;;        (:n "l" #'lispyville-comment-or-uncomment-line))

;; (bind! (:map oo-package-map)
;;        (:wk "browse" "b" #'elpaca-browse)
;;        (:wk "update all"     "U" #'elpaca-update-all)
;;        (:wk "update"         "u" #'elpaca-update)
;;        (:wk "visit"          "v" #'elpaca-visit)
;;        (:wk "try"            "i" #'elpaca-try)
;;        (:wk "rebuild"        "r" #'elpaca-rebuild)
;;        (:wk "delete"         "d" #'elpaca-delete)
;;        (:wk "log"            "l" #'elpaca-log)
;;        (:wk "write lockfile" "w" #'elpaca-write-lockfile)
;;        (:wk "manager"        "m" #'elpaca-manager))

;; (bind! (:alt org-capture +org/choose-capture-template))

;; (bind! (:v "E" #'lispy-eval-and-replace))

;; (bind! (:map oo-app-map)
;;        ("g" #'gumshoe-peruse-globally))

;; (bind! (:map org-mode-map)
;;        (:n "H" #'outline-up-heading))

;; (bind! (:when (require 'consult nil t))
;;        (:alt switch-to-buffer consult-buffer)
;;        (:alt yank-pop consult-yank-pop)
;;        (:alt apropos consult-apropos)
;;        (:alt man consult-man))

;; (bind! (:map oo-find-map)
;;        (:wk "line"        "s" #'consult-line)
;;        (:wk "line"        "l" #'consult-line)
;;        (:wk "outline"     "h" #'consult-outline)
;;        (:wk "org heading" "o" #'consult-org-heading))

;; (oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

;; (bind! (:map oo-miscellany-map)
;;        ("l" #'consult-bookmark))

;; (set! bookmark-save-flag 1)

;; (defafter! dont-pair-quotes (smartparens)
;;   (sp-local-pair sp-lisp-modes "'" nil :actions nil))

;; (defafter! pair-backquote-quote (smartparens)
;;   (sp-local-pair sp-lisp-modes "`" "'" :when '(sp-in-string-p sp-in-comment-p)))

;; (set! avy-style 'pre)
;; (set! avy-keys (number-sequence 97 122))

;; (oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

;; (set! rainbow-delimiters-max-face-count 9)
;; (oo-add-hook '(prog-mode-hook reb-mode-hook) #'rainbow-delimiters-mode)

;; (oo-add-hook 'prog-mode-hook #'hs-minor-mode)

;; (set! savehist-save-minibuffer-history t)
;; (set! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
;; (set! savehist-autosave-interval (* 60 5))
;; (set! savehist-file (concat oo-cache-dir "savehist"))

;; (oo-add-hook 'on-first-input-hook #'savehist-mode)

;; (set! save-place-file (concat oo-cache-dir "saveplace"))
;; (set! save-place-limit nil)

;; (oo-add-hook 'on-first-file-hook #'save-place-mode)

;; (set! super-save-auto-save-when-idle t)
;; (set! super-save-idle-duration 5)

;; (oo-add-hook 'on-first-file-hook #'super-save-mode)

;; (oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

;; (oo-add-hook '(prog-mode-hook text-mode-hook) #'evil-surround-mode)

;; (defhook! enable-smartparens-maybe (minibuffer-setup-hook)
;;   "Enable `smartparens-mode' in the minibuffer."
;;   (when (memq this-command '(eval-expression evil-ex))
;;     (require 'smartparens)
;;     (smartparens-strict-mode 1)))

;; (adding-to-list! dogears-ignore-modes 'dashboard-mode)

;; (bind! (:map oo-leader-map)
;;        (:prefix "l")
;;        ("l" #'dogears-go))

;; (defun oo-org-src-buffer-p () (bound-and-true-p org-src-mode))
;; (adding-to-list! dogears-ignore-places-functions #'oo-org-src-buffer-p)

;; (adding-to-list! savehist-additional-variables 'dogears-list)

;; (adding-to-list! savehist-additional-variables 'register-alist)

;; (oo-add-hook 'on-first-input-hook #'dogears-mode)

;; (oo-silence #'flyspell-mode)

;; (oo-add-hook 'text-mode-hook #'flyspell-mode)

;; (setq-default bookmark-default-file (expand-file-name "bookmarks" oo-cache-dir))

;; (defun! oo/set-font-face ()
;;   "Apply an existing xfont to all graphical frames."
;;   (interactive)
;;   (let! font (completing-read "Choose font: " (x-list-fonts "*")))
;;   (set-frame-font font nil t))

;; (set! transient-levels-file (concat oo-cache-dir "transient/levels"))
;; (set! transient-values-file (concat oo-cache-dir "transient/values"))
;; (set! transient-history-file (concat oo-cache-dir "transient/history"))

;; (adding-to-list! recentf-filename-handlers #'abbreviate-file-name)

;; (adding-to-list! recentf-filename-handlers #'substring-no-properties)

;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; (set! ispell-program-name (or (executable-find "hunspell") (executable-find "ispell")))
