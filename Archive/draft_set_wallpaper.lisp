;;; setting the wallpaper

;;;; define places to set wallpaper 
(defparameter *wallpaper-dir* nil)
(defparameter *history-file* nil)

(defun read-history (history-file)
  "Reads the history of used wallpapers from a file."
  (if (probe-file history-file)
      (with-open-file (stream history-file)
        (loop for line = (read-line stream nil nil)
              while line collect line))
      '()))

(defun random-wallpaper (wallpaper-dir)
  "Set the wallpaper to a random one not in the history."
  (let* ((all-wallpapers (directory (concatenate 'string wallpaper-dir "/*"))))
    (nth (random (length all-wallpapers)) all-wallpapers)))

(defun set-wallpaper (image-path history-file)
  "Sets the desktop wallpaper using 'feh' and records it."
  (let ((command (format nil "feh --bg-scale '~a'" image-path)))
    (handler-case
        (sb-ext:run-program "/bin/sh" (list "-c" command)
                            :output *standard-output*
                            :error *error-output*)
      (error (e)
        (format t "Error setting wallpaper: ~a~%" e)))
    (write-history history-file image-path)))

(set-wallpaper wallpaper)
