(require 'lgr)

(defvar oo-log-buffer (get-buffer-create "*oo-log*")
  "Buffer where information should be logged.")

(defvar oo-lgr (-> (lgr-get-logger "oo")
                   (lgr-add-appender (lgr-appender-buffer :buffer oo-log-buffer)))
  "Object used for logging.")

(provide 'oo-base-log)
