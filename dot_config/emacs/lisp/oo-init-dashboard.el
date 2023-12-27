(defhook! create-dashboard (oo-initial-buffer-choice-hook)
  (when (require 'dashboard nil t)
    (aprog1 (get-buffer-create dashboard-buffer-name)
      (with-current-buffer it
        (dashboard-insert-startupify-lists)))))

(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(set! dashboard-init-info #'oo-dashboard-init-info)

(set! dashboard-banner-logo-title "Welcome!")

(set! dashboard-set-footer nil)

(set! dashboard-items nil)

(set! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))

(set! dashboard-center-content t)
