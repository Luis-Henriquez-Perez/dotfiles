(require 'dashboard)

(defhook! oo-initial-buffer-choice-hook&make-dashboard ()
  (when (require 'dashboard nil t)
    (aprog1 (get-buffer-create dashboard-buffer-name)
      (with-current-buffer it
        (dashboard-insert-startupify-lists)))))

;; If I put dashboard configuration in its own.
(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(opt! dashboard-init-info #'oo-dashboard-init-info)

(opt! dashboard-banner-logo-title "Welcome!")

(opt! dashboard-set-footer nil)

(opt! dashboard-items nil)

(opt! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))

(opt! dashboard-center-content t)

(provide 'dashboard)
