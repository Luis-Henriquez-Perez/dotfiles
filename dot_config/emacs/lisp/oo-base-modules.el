(require 'oo-call-after-load)

;; This file is for specifying how to load modules.

(defun oo-list-modules ()
  "List available modules.")

(defun oo-load-module (name)
  "Load module named NAME."
  (let! recipe-file )
  (let! init-file)
  (let! config ())
  (dolist ()
    (when (file-exists-p)
      (load file t)))
  (oo-call-after-load name #'load configuration))
