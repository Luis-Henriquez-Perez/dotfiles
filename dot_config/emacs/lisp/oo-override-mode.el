;; **** override map
;; ***** oo-override-mode-map
;; Creating a minor mode to hold the leader map allows us to toggle our leader
;; bindings on or off.

;; Enabling =override-mode= needs to be the first thing we do in
;; =emacs-startup-hook=, or at least it needs to be before modes that set
;; keybindings like evil.  Otherwise, your bindings might not take effect
;; immediately.  This is why I set the advice depth to =-100=.
(defvar oo-override-mode-map (make-sparse-keymap))

(define-minor-mode oo-override-mode
  "Global minor mode for higher precedence evil keybindings."
  :keymap oo-override-mode-map
  :global t)

(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)
;; ***** make =oo-override-mode-map= an intercept map
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defun evil-mode-hook&make-intercept-map ()
  "Register `oo-override-map' as an intercept map."
  (message "Make intercept map.")
  (evil-make-intercept-map oo-override-mode-map 'all t))

(oo-add-hook 'evil-mode-hook #'evil-mode-hook&make-intercept-map)

;; Looking at the [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html][Emacs keymap hierarchy]], emulation mode maps is pretty up
;; there.  The [[helpvar:emulation-mode-map-alists][emulation-mode-map-alists]]
(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))

(provide 'oo-override-mode)
