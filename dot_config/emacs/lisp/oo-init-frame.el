(oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)

;; TODO: The display flickers when setting the initial theme.  Maybe this is
;; inevitable.  But maybe this has to do with me either disabling the previous
;; theme first or the order of setting the window-divider, or maybe I can
;; specify the default theme to load beforehand.  I need to play around with
;; settings and see if this flickering can be avoided.
(oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)

(add-hook 'emacs-startup-hook
          (lambda (&rest _) (oo-add-advice #'load-theme :after #'oo-set-window-divider-face)))

;; You can either use =right-only= to place window dividers on the right of each
;; window.  Or =bottom-only= to place them just on the bottom.
(set! window-divider-default-places t)

;; The default value is 6.
(set! window-divider-default-right-width 7)
(set! window-divider-default-bottom-width 7)

;;; window divider
;; The function =oo-set-window-divider-face= was being called like 10 times and
;; I was unsure why.  Adding the advice to =load-theme= after
;; =emacs-startup-hook= stopped it from being called so much. I wonder how many
;; times =load-theme= was being called?

(defun oo-set-window-divider-face (&rest _)
  "Set the window divider face."
  (message "set face of window divider to black")
  (set-face-foreground 'window-divider "black"))
