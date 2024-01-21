#!/usr/bin/guile -s
!#

(use-modules (ice-9 rdelim))

;; Directory containing wallpapers
(define wallpaper-dir "~/dotfiles/wallpapers")

;; Get a list of wallpapers in the directory
;; (define wallpapers (scandir wallpaper-dir))
(define wallpapers
  (map (cut string-append wallpaper-dir "/" <>)
       (filter (lambda (file) (not (string=? file ".")))
               (directory-files wallpaper-dir))))

;; Choose a random wallpaper
(define random-wallpaper
  (list-ref wallpapers (random (length wallpapers))))

;; Set the wallpaper using the `feh` command (replace with your wallpaper tool)
(system* "feh" "--bg-scale" (string-append wallpaper-dir "/" random-wallpaper))
