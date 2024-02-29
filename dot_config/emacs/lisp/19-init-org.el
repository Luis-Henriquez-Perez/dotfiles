;;; org-superstar
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(opt! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))

(opt! org-superstar-leading-bullet ?\s)

(opt! org-superstar-special-todo-items t)
;;; org-appear
(oo-add-hook 'org-mode-hook #'org-appear-mode)

(opt! org-appear-autolinks t)
;;; org-refile
(opt! org-refile-allow-creating-parent-nodes t)

;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(opt! org-refile-targets '((oo-directory-files :maxlevel . 10)))

(opt! org-outline-path-complete-in-steps nil)

(opt! org-refile-use-cache nil)

;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(opt! org-refile-use-outline-path 'file)

;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
(opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;;; org-id
(opt! org-id-track-globally t)
(opt! org-id-locations-file (expand-file-name "org-id-locations" oo-data-dir))

;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(opt! org-id-method 'ts)

(opt! org-id-link-to-org-use-id t)
;;; org-src
(oo-popup-at-bottom "\\*Org Src")

(opt! org-edit-src-persistent-message nil)

;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))

;; (adjoin! org-src-lang-modes '("lua" . lua))

(opt! org-src-ask-before-returning-to-edit-buffer nil)

(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)

(opt! org-src-window-setup 'plain)

(oo-bind :alt #'org-edit-src-code #'oo-dwim-edit-src-code)
(oo-bind :h "," #'org-edit-src-code :localleader t)
;; (oo-bind 'org-mode-map "e" #'org-edit-src-code)
                                        ;(oo-bind ':h "es" #'org-edit-src-code :wk "source block" :localleader t)

(oo-bind 'org-src-mode-map "," #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "a" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "c" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
;;; Org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")

(oo-bind 'oo-quick-map "j" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "a" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "j" #'org-capture :wk "capture")

(opt! org-archive-save-context-info nil)

(opt! org-archive-location (concat org-directory "archive.org::"))

(oo-add-hook 'org-insert-heading-hook #'org-id-get-create)

(provide '19-init-org)
