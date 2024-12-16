;; https://docs.hylang.org/en/stable/index.html

(import subprocess)
(import random)
(import glob)

;; * wallpaper
;; The goal of this file is to set the wallpaper based on a list of wallpapers.

; * history

;; def get_wallpaper_history ():
;;     "Update the history file."
;;     if not os.path.exists(wallpaper_history_file):
;;         return []

;;     with open(wallpaper_history_file, 'r') as file:
;;         return json.load(file)

(defn set-wallpaper [image-path]
  "Set the desktop wallpaper using 'feh'."
  (try
    (subprocess.run ["feh" "--bg-scale" image-path] :check True)
    (print (.format "Wallpaper set to {}" image-path))
    (except [e subprocess.CalledProcessError]
      (print (.format "Error setting wallpaper: {}" e)))))

(defn random-wallpaper []
  "Set the wallpaper to a random one."
  (random.choice (glob.glob "/home/luis/dotfiles/wallpapers/*")))

;; Set the wallpaper
(set-wallpaper (random-wallpaper))
