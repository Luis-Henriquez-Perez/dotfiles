;; (require 'package-vc)
;; (require 'vc)
;; (push '(chezmoi :url "https://github.com/tuh8888/chezmoi.el" :commit "1389782") package-vc-selected-packages)
;; (push '(grugru :url "https://github.com/ROCKTAKEY/grugru" :commit "92e588e") package-vc-selected-packages)
;; (push '(aggressive-indent :url "https://github.com/Malabarba/aggressive-indent-mode" :commit "b0ec004") package-vc-selected-packages)
;; (push '(dashboard :url "https://github.com/emacs-dashboard/emacs-dashboard" :commit "36c8da4") package-vc-selected-packages)
;; (push '(macrostep :url "https://github.com/joddie/macrostep" :commit "424e373") package-vc-selected-packages)
;; ;; (push '(rainbow-delimiters :url "https://github.com/Fanael/rainbow-delimiters" :commit "f43d48a") package-vc-selected-packages)
;; ;; Dependency of evil.
;; (push '(goto-chg :url "https://github.com/emacs-evil/goto-chg" :commit "2af6121") package-vc-selected-packages)
;; (push '(evil :url "https://github.com/emacs-evil/evil" :commit "cc9d688") package-vc-selected-packages)
;; (push '(evil-goggles :url "https://github.com/edkolev/evil-goggles" :commit "08a2205") package-vc-selected-packages)

;; ;; (push '(lispy :url "https://github.com/abo-abo/lispy" :commit "1ad128b") package-vc-selected-packages)
;; ;; (push '(lispyville :url "https://github.com/noctuid/lispyville" :commit "0f13f26") package-vc-selected-packages)
;; ;; (push '(smartparens :url "https://github.com/Fuco1/smartparens" :commit "63695c6") package-vc-selected-packages)

;; ;; So the install dependencies installs the packages from the archive.
;; ;; Show me the dependencies for package and check to see if they are available
;; ;; in.
;; (defun oo-install-recipe (recipe)
;;   (block! nil
;;     (noflet! package-vc--unpack-1 (desc dir)
;;       (let ((url (alist-get :url (package-desc-extras desc)))
;;             (commit (plist-get (cdr recipe) :commit))
;;             (name (package-desc-name desc)))
;;         ;; (cl-assert url t "no url from %s" (car recipe))
;;         (message "CLONING %s FROM %S to %S..." (package-desc-name desc) url dir)
;;         (message "CURRENT COMMIT -> %S" (package-vc-commit desc))
;;         (if (not commit)
;;             (message "NO COMMIT TO CHECKOUT")
;;           (message "COMMIT TO CHECKOUT -> %S" commit)
;;           (let ((default-directory dir))
;;             (message "CURRENT DIRECTORY: %s" default-directory)
;;             (let* ((outcome (vc-git-command nil t nil "checkout" commit))
;;                    (status (if (and outcome (zerop outcome)) "SUCCEED IN" "FAILED IN")))
;;               (message "%s CHECKOUT %s of %s" status commit name)
;;               (message "COMMIT IS NOW %s" (package-vc-commit desc)))))
;;         ;; Tell me the dependencies.
;;         (funcall this-fn desc dir)))
;;     (noflet! package-install-from-archive (desc)
;;       (let ((url (alist-get :url (package-desc-extras desc)))
;;             (name (package-desc-name desc)))
;;         (message "PREPARING TO INSTALL DEPENDENCY %S..." name)
;;         (cond ((package-installed-p name)
;;                (message "PACKAGE %s ALREADY INSTALLED"))
;;               (url
;;                (message "INSTALLING DEPENDENCY %S from %S..." name url)
;;                (package-vc-install `(,name :url ,url)))
;;               (t
;;                (message "No url, installing dependency %s from archive" name)
;;                (funcall this-fn desc)))))
;;     ;; (noflet! package-vc-install-dependencies (&rest args)
;;     ;;   (prog1 (apply this-fn args)
;;     ;;     (message "done with dependencies")))
;;     (package-vc-install recipe)
;;     ;; (set! name (car recipe))
;;     ;; (when (stringp name) (setq name (intern name)))
;;     ;; (set! pkg-descs (assoc name package-alist))
;;     ;; (set! spec (cdr recipe))
;;     ;; (set! url (plist-get spec :url))
;;     ;; (set! commit (plist-get spec :commit))
;;     ;; (set! installed nil)

;;     ;; Find the dependencies of the package.
;;     ;; (unless (seq-some #'package-vc-p (cdr pkg-descs))
;;     ;;   (package-vc-install recipe))
;;     ))

;; (mapc #'oo-install-recipe (reverse package-vc-selected-packages))

;; (dolist (recipe (reverse package-vc-selected-packages))
;;   (unless (package-installed-p (car recipe))
;;     (with-demoted-errors (package-vc-install recipe))))

