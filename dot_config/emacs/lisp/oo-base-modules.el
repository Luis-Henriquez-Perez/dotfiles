;; This file is for specifying how to load modules.

(defun oo-list-modules ()
  "List available modules.")

(defun oo-load-module (name)
  "Load module named NAME."
  (let! recipe-file )
  (let! init-file)
  (let! config ())
  ()
  (when (file-exists-p)
    (load file t))
  (when (file)))
