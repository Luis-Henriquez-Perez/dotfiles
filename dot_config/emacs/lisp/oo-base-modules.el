(require 'oo-call-after-load)

;; This file is for specifying information about modules.  What I call modules
;; differs from what would be considered modules in doom or spacemacs.  I am
;; hesitant to make a generic abstraction like "completion" or to group a set of
;; packages together as one unit

(defvar oo-loaded-modules nil)

(defun oo-list-modules ()
  "List available modules."
  (directory-files))

(defun oo-load-module (name)
  "Load module named NAME."
  (let! recipe-file )
  (let! init-file)
  (let! config ())
  (dolist ()
    (when (file-exists-p)
      (load file t)))
  (oo-call-after-load name #'load configuration)
  (push module oo-loaded-modules))

(provide 'oo-modules)
