(defun oo-bootstrap-straight (&optional dir)
  (setq straight-use-package-by-default t)
  (setq straight-vc-git-default-clone-depth 1)
  (setq straight-recipes-gnu-elpa-use-mirror t)
  ;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
  (setq straight-check-for-modifications nil)
  (setq use-package-always-defer t)
  (defvar bootstrap-version)
  (let* ((straight-repo-dir
          (expand-file-name "straight/repos" user-emacs-directory))
         (bootstrap-file
          (concat straight-repo-dir "/straight.el/bootstrap.el"))
         (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (shell-command
       (concat
        "mkdir -p " straight-repo-dir " && "
        "git -C " straight-repo-dir " clone "
        "https://github.com/raxod502/straight.el.git && "
        "git -C " straight-repo-dir " checkout 2d407bc")))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  ;; This is a variable that has been renamed but straight still refers when
  ;; doing :sraight (:no-native-compile t)
  (setq comp-deferred-compilation-black-list nil))

(defun oo-straight-boostrap-packages (dir recipes)
  "Install all packages as specified by recipe."
  (let ((load-path load-path)
        (recipes ()))
    (oo-bootstrap-straight)
    ;; First we bootstrap straight.el.
    ;; The following are the steps to ensure we don't have to do things twice.
    ;; 1. Register all the recipes.
    (mapc #'straight-register-package recipes)
    ;; 2. Clone all the packages in recipes.
    (mapc #'straight-clone-package recipes)
    ;; 3. Set all the branches to the proper branch and commit
    (dolist (recipe recipes)
      (setq repo (str))
      (straight-vc-branch)
      (straight-vc-checkout version))
    ;; 4. Build the packages.
    (mapc #'straight-build-package recipes))
  ;; 5. Return the difference in load paths
  (cl-difference))

(provide 'oo-bootstrap-straight)
