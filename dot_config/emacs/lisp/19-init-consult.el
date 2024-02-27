;;;;;; consult
(opt! consult-preview-key nil)

(opt! consult-fontify-preserve nil)

(defun! oo-display-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'display-buffer)
  (call-interactively #'consult-buffer))

(oo-bind :alt #'display-buffer #'oo-display-buffer)

(oo-bind 'oo-find-map "k" #'consult-bookmark :wk "bookmark")
(oo-bind 'oo-find-map "b" #'consult-bookmark :wk "bookmark")

(oo-bind 'oo-find-map "s" #'consult-line :wk "line")
(oo-bind 'oo-find-map "l" #'consult-line :wk "line")

(oo-bind 'oo-find-map "h" #'consult-outline :wk "outline")
(oo-bind 'oo-find-map "h" #'consult-org-heading :wk "heading")

(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)

(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)

(oo-bind 'oo-miscellany-map "l" #'consult-bookmark)

(provide '19-init-consult)
