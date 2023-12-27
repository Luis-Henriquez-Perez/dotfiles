(oo-bind 'oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-find-map "j" #'burly-open-bookmark)

;; *** save window configuration with =b= or =S=
;; The command [[file:snapshots/_helpful_command__burly-bookmark-windows_.png][burly-bookmark-windows]] creates a bookmark with the information
;; necessary to reproduce the current window configuration.  I can then restore the
;; window information I've bookmarked with [[file:snapshots/_helpful_command__burly-open-bookmark_.png][burly]].
(oo-bind 'oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")

