(defun exwm-update-class-hook&set-descriptive-name ()
  "Set the name of EXWM buffer to be more descriptive."
  (exwm-workspace-rename-buffer exwm-class-name))

(add-hook 'exwm-update-class-hook #'exwm-update-class-hook&set-descriptive-name)
