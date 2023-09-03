(defvar oo-override-mode-map (make-sparse-keymap))

(define-minor-mode oo-override-mode
  "Global minor mode for higher precedence evil keybindings."
  :keymap oo-override-mode-map
  :global t)

(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))

(defun evil-mode-hook&make-intercept-map ()
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map oo-override-mode-map 'all t))

(oo-add-hook 'evil-mode-hook #'evil-mode-hook&make-intercept-map)

(provide 'oo-override-mode)
