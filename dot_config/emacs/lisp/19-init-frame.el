(defun oo-set-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

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
(opt! window-divider-default-places t)

;; The default value is 6.
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-bottom-width 7)

(provide '19-init-frame)
