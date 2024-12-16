;; This file is meant to be loaded with =-l= and make it so that emacs writes
;; its messages to a file.

(defvar oo-log-file (expand-file-name (format-time-string "%Y%m%d%H%M%S-emacs-message-log.txt") "~/"))

(defun oo-append-to-file (file text)
  "Append TEXT to the end of FILE."
  (with-temp-buffer
    (insert text)
    (append-to-file (point-min) (point-max) file)))

(defun oo-log-message-to-file (file _ &rest args)
  "Custom message function that appends formatted message to FILE."
  (oo-append-to-file file (concat (apply #'format args) "\n")))

(advice-add #'message :around (apply-partially #'oo-log-message-to-file oo-log-file))
