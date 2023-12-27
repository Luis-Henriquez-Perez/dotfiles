(oo-bind 'dired-mode-map :nm "h" #'dired-up-directory)

(oo-add-hook 'dired-mode-hook #'dired-omit-mode)

(set! dired-recursive-copies 'always)
(set! dired-recursive-deletes 'always)
