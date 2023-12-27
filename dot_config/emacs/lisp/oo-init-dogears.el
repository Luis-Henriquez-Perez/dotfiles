(oo-add-hook 'on-first-input-hook #'dogears-mode)

(after! org
  (defun oo-org-src-buffer-p () (bound-and-true-p org-src-mode))
  (adjoining! dogears-ignore-places-functions #'oo-org-src-buffer-p))

(after! savehist (adjoin! savehist-additional-variables 'dogears-list))

(after! dashboard (adjoin! dogears-ignore-modes 'dashboard-mode))

(oo-bind 'oo-leader-map "ll" #'dogears-go)
