;; *** recentf
;; =recentf= is a built-in program that tracks the files you've opened recently
;; persistently.  This is a great idea because these are the files you'll likely
;; revisit.  In practice, I look at this list of files in addition to the buffers I
;; already have open using a [[id:f26bedb3-a172-4543-afd0-4c47f5872d15][completion-framework]].  Because of this I rarely
;; have to set out to look for a file with =dired=.
(oo-add-hook 'emacs-startup-hook #'recentf-mode)

(set! recentf-max-menu-items 0)
(set! recentf-max-saved-items 700)
(set! recentf-save-file (concat oo-cache-dir "recentf"))
(set! recentf-auto-cleanup (* 60 10))
(set! recentf-filename-handlers '(file-truename))

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(provide 'oo-recentf-config)
