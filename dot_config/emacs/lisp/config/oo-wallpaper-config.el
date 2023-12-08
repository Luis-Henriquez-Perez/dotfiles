;;; settings
;;;; control how often a wallpaper is changed
;; This variable controls how often (in seconds) to change your wallpaper when you
;; have =wallpaper-cycle-mode= enabled. The default is every =15= seconds but in my
;; opinion that's way too distracting and disorienting.  Therefore, I set it to =5=
;; minutes.  For me 5 minutes provides a good balance of not being too distracting,
;; but also not letting the current wallpaper get too boring. Note that if you
;; change this interval while =wallpaper-cycle-mode= is active it will not take
;; effect because the timer is already running; you have to disable and re-enable
;; wallpaper-cycle-mode= which would create a new timer.
(set! wallpaper-cycle-interval (* 5 60))

;;;; set directory where my wallpapers should go
;; This is the symbol that provides the path for getting the set of wallpapers.  I
;; would have just name this =wallpaper-directory= or =wallpaper-dir= (probably the
;; former so that its more predictable) because this directory is used for more
;; than just =wallpaper-cycle-mode=.  For example, its also used in
;; [[file:snapshots/_helpful_command__wallpaper-set-wallpaper_.png][wallpaper-set-wallpaper]].
(set! wallpaper-cycle-directory (expand-file-name "~/dotfiles/wallpapers"))

;;; problem with

;;; add feature to

;;; provide
(provide 'oo-wallpaper-config)
