(require 'oo-base-settings)
(require 'oo-base-utils)
(require 'on)

(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
	          (get-buffer-create "*scratch*"))
    (oo-log-info "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)

(defhook! boost-garbage-collection (minibuffer-setup-hook)
  "Boost garbage collection settings to `gcmh-high-cons-threshold"
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defhook! defer-garbage-collection (minibuffer-exit-hook :append t)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold gcmh-low-cons-threshold))

(defadvice! disable-old-themes (around load-theme)
  "Disable old themes before loading new ones."
  (:args orig-fn &rest args)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(defun! oo/set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (let! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

(provide 'oo-base-main)
