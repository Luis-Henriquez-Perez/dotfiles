(require 'which-key)

(set! which-key-show-prefix 'top)

;; Emacs is full of so many keybindings, that it can be difficult to keep track of
;; them.  Especially when you're starting out, but even when you're an Emacs-pro,
;; it's easy to forget what a particular functionality is bound to.  Typically,
;; you'll remember the first few key strokes but struggle with the rest.  To address
;; this [[github:][which-key]] displays key binding sequences in the minibuffer as your typing
;; them.  By doing this you can "discover" the commands as you go along.
;; Note that this can't work with `on-first-input-hook' because which-key
;; doesn't happen on first keypress.  It needs to be in the startup hook.
;; (set! which-key-sort-uppercase-first nil)
(set! which-key-max-display-columns nil)
(set! which-key-add-column-padding 1)
(set! which-key-min-display-lines 1)
(set! which-key-side-window-slot -10)
(set! which-key-sort-order #'which-key-prefix-then-key-order)
(set! which-key-popup-type 'side-window)
(set! which-key-idle-delay 0.8)
;; (set! line-spacing 3 :hook which-key-init-buffer-hook :local t)

(set! which-key-show-transient-maps t)
(set! which-key-show-operator-state-maps t)

(provide 'oo-which-key-configuration)
