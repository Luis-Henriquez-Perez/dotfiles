(defun! oo-display-buffer-base-action (&rest args)
  "Open new window and ensure all windows are arranged as master-slave."
  (require 'edwina)
  (aprog1 (with-demoted-errors "Error: %S" (apply #'display-buffer-at-bottom args))
    (edwina--respective-window it
      (edwina-arrange))))

(setq display-buffer-base-action (cons #'oo-display-buffer-base-action nil))

;;; arrange windows in a master-slave style with =a=
;; I [[https://gitlab.com/ajgrf/edwina][edwina]] to automatically window configurations.  I choose the buffers I
;; want to display and it opens a window at a predictable location.  It is possible
;; that through some operation my windows could get out of wack.  It shouldn't
;; happen accidentally, but most often it will happen after I maximize or minimize
;; a window.  In that case, I can call =edwina-arrange= to proportion my windows properly.
(oo-bind 'oo-window-map "a" #'edwina-arrange :wk "arrange")
