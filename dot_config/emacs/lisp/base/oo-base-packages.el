(require 'cl-lib)
(require 'oo-quelpa-bootstrap)

(oo-boostrap-quelpa)

;; Read recipes.

(defun oo-boostrap-packages (dir recipes)
  "Install packages to DIR and return LOAD-PATHS added."
  (oo-boostrap-quelpa)
  (let ((load-path load-path))
    (dolist (recipe recipes)
      (oo-package-install package dir))
    (cl-difference)))

(provide 'oo-base-packages)
