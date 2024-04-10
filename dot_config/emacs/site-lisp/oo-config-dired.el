(oo-bind 'dired-mode-map :nm "h" #'dired-up-directory)

(oo-add-hook 'dired-mode-hook #'dired-omit-mode)

(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)
