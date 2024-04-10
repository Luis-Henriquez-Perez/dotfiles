;;; Fix bugs with function for setting the wallpaper
;; There are two problems with the [[file][eoriginal]] wallpaper function.  I'll start with the
;; lesser one.  The original function allows you to select the current
;; wallpaper again.  I get it, its supposed to be random.
(defun oo-wallpaper-set-wallpaper ()
  "Set the wallpaper to a random different wallpaper.

This function will either choose a random wallpaper from
`wallpaper-cycle-directory' or use the wallpapers listed in
`wallpaper-static-wallpapers'."
  (let ((wallpapers (or (wallpaper--per-workspace-wallpapers)
                        wallpaper-static-wallpaper-list
                        (wallpaper--random-wallpapers)))
        (command (concat "feh --no-fehbg " (wallpaper--background))))
    (if (not wallpapers)
        (message "No wallpapers selected.")
      (setq wallpaper-current-wallpapers nil)
      (dolist (wallpaper wallpapers)
        (alet (expand-file-name (shell-quote-argument (f-filename wallpaper)) (f-dirname wallpaper))
          (setq command (concat command (wallpaper--scaling) it " ")))
        (add-to-list 'wallpaper-current-wallpapers wallpaper))
      (start-process-shell-command "Wallpaper" nil command))))

(advice-add #'wallpaper-set-wallpaper :override #'oo-wallpaper-set-wallpaper)

;;; Fix bugs with getting random wallpaper.
(defun oo-wallpaper--random-wallpapers ()
  "Return a string of random wallpapers for each monitor.

If `wallpaper-cycle-single' is non-nil, only one wallpaper is returned."
  (let* ((available (wallpaper--get-available))
         (num-available (length available))
         (num-monitors (if wallpaper-cycle-single 1 (wallpaper--num-monitors)))
         (wallpapers nil))
    (dotimes (_ num-monitors)
      (let ((wallpaper (nth (1- (random num-available)) available)))
        (cl-pushnew wallpaper wallpapers)
        (setq available (delq wallpaper available))))
    wallpapers))

(advice-add #'wallpaper--random-wallpapers :override #'oo-wallpaper--random-wallpapers)

;;; Add feature for adding trashing a wallpaper I don't like
;; TODO: Create a function trash the current wallpaper.
;; There are some wallpapers I don't want to see again.  I want to
;; automate trashing them.
(defun! oo-wallpapaper-trash-current ()
  "Trash the image corresponding to the current wallpaper."
  (interactive)
  ;; The symbol =wallpaper-current-wallpapers= contains the list of
  ;; current wallpapers for each monitor.  I do not know if I should
  ;; just trash first wallpaper in this list or all of them.  I don't
  ;; use multiple monitors.

  ;; Assume no multiple monitors.
  (cl-assert (not (cdr wallpaper-current-wallpapers)))
  (cl-assert (car wallpaper-current-wallpapers))
  (let! current-wallpaper (car wallpaper-current-wallpapers))
  (move-file-to-trash current-wallpaper)
  (wallpaper-set-wallpaper))
