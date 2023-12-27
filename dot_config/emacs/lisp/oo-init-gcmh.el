(oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)

(set! gcmh-idle-delay 'auto)

(set! gcmh-high-cons-threshold (* 8 1024 1024))

(set! gcmh-low-cons-threshold (* 4 1024 1024))

;; [[helpvar:minibuffer-setup-hook][minibuffer-setup-hook]] and [[helpvar:minibuffer-exit-hook][minibuffer-exit-hook]] are the hooks run just before
;; entering and exiting the minibuffer (respectively).  In the minibuffer I'll be
;; primarily doing searches for variables and functions.  There are alot of
;; variables and functions so this can certainly get computationally expensive.  To
;; keep things snappy I increase boost the [[helpvar:gc-cons-threshold][gc-cons-threshold]] just before I enter
;; the minibuffer, and restore it to it's original value a few seconds after it's closed.

(defhook! boost-garbage-collection (minibuffer-setup-hook)
  "Boost garbage collection settings to `gcmh-high-cons-threshold"
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defhook! defer-garbage-collection (minibuffer-exit-hook :append t)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold gcmh-low-cons-threshold))
