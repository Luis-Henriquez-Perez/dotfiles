
;;; initial line spacing
(add-hook 'org-mode-hook (lambda () (setq-local line-spacing 4)))

;;; org-src
(oo-popup-at-bottom "\\*Org Src")

(set! org-edit-src-persistent-message nil)

(adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))

(adjoin! org-src-lang-modes '("lua" . lua))

(set! org-src-ask-before-returning-to-edit-buffer nil)

(set! org-src-preserve-indentation t)
(set! org-edit-src-content-indentation 0)

(set! org-src-window-setup 'plain)

(oo-bind :alt #'org-edit-src-code #'oo-dwim-edit-src-code)
(oo-bind :h "," #'org-edit-src-code :localleader t)
;; (oo-bind 'org-mode-map "e" #'org-edit-src-code)
                                        ;(oo-bind ':h "es" #'org-edit-src-code :wk "source block" :localleader t)

(oo-bind 'org-src-mode-map "," #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "a" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "c" #'org-edit-src-exit :localleader t :mode 'org-src-mode)

;;; org-appear
(oo-add-hook 'org-mode-hook #'org-appear-mode)

(set! org-appear-autolinks t)

;;; org-auto-tangle
(set! org-auto-tangle-default t)

;;; org-superstar
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(set! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))

(set! org-superstar-leading-bullet ?\s)

(set! org-superstar-special-todo-items t)

;;; org-tidy
(oo-add-hook 'org-mode-hook #'org-tidy-mode)

(set! org-tidy-properties-style 'invisible)

;;; org-refile
(set! org-refile-allow-creating-parent-nodes t)

;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(set! org-refile-targets '((oo-directory-files :maxlevel . 10)))

(set! org-outline-path-complete-in-steps nil)

(set! org-refile-use-cache nil)

;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(set! org-refile-use-outline-path 'file)

;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
(set! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))

;;; org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")

(oo-bind 'oo-quick-map "j" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "a" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "j" #'org-capture :wk "capture")

(set! org-archive-save-context-info nil)

(set! org-archive-location (concat org-directory "archive.org::"))

(oo-add-hook 'org-insert-heading-hook #'org-id-get-create)

;;; org-id
(set! org-id-track-globally t)
(set! org-id-locations-file (concat oo-cache-dir "org-id-locations"))

;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(set! org-id-method 'ts)

(set! org-id-link-to-org-use-id t)
