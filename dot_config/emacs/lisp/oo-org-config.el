(require 'org-superstar)
(require 'gv)

(set! org-confirm-babel-evaluate nil)

(set! org-auto-tangle-default t)

(defun! +org/choose-capture-template ()
  "Choose a capture template."
  (interactive)
  (dolist (template org-capture-templates)
    (let! (keys name) template)
    (collecting! alist (cons name keys)))
  (let! selected (completing-read "capture template: " alist nil t))
  (org-capture nil (alist-get selected alist nil nil #'string=)))

(gv-define-expander org-ml-headline-get-section
  (lambda (do place)
    (gv-letplace (getter setter) place
      (funcall do `(org-ml-headline-get-section ,getter)
               (lambda (v)
                 (macroexp-let2 nil v v
                   `(progn
                      ,(funcall setter `(org-ml-headline-set-section ,v ,getter)))))))))

(oo-popup-at-bottom "CAPTURE[^z-a]+")

(oo-popup-at-bottom "\\*Org Src")

(set! org-refile-target-verify-function (lambda () (not (+org-has-src-block-p))))

(defun! +org-has-src-block-p ()
  "Return non-nil if current headline has a source block."
  (save-excursion
    (let! beg (point))
    (let! end (or (outline-next-heading) (point-max)))
    (goto-char beg)
    (and (save-match-data (re-search-forward "^#\\+begin_src" end t)) t)))

(set! org-superstar-leading-bullet ?\s)
(set! org-superstar-special-todo-items t)
(set! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(set! org-refile-use-outline-path 'file)

(defun +org-directory-files ()
  "Return a list of org files in the `org-directory'."
  (directory-files org-directory t "\\.org\\'"))

(set! org-directory (expand-file-name "~/dotfiles"))

(set! org-archive-location (concat org-directory "archive.org::"))

(set! org-default-notes-file null-device)

(set! org-refile-use-cache nil)

(setq org-outline-path-complete-in-steps nil)

(set! org-refile-targets '((+org-directory-files :maxlevel . 10)))

(set! org-archive-save-context-info nil)

(set! org-refile-allow-creating-parent-nodes t)

(set! org-src-preserve-indentation t)
(set! org-edit-src-content-indentation 0)

(set! org-src-ask-before-returning-to-edit-buffer nil)

(defun! +org/dwim-next-visible-heading ()
  (interactive)
  (+org/dwim-previous-visible-heading t))

(defun! +org/dwim-previous-visible-heading (&optional next)
  (interactive)
  (let! case-fold-search nil)
  (let! regexp "^\\*+\\(?: +[[:upper:]]+\\)?\\(?: +\\[#.]\\)?\\(?: +\\(.*?\\)\\)?")
  (if next
      (org-next-visible-heading 1)
    (org-previous-visible-heading 1))
  (org-back-to-heading)
  (save-match-data
    (looking-at regexp)
    (goto-char (match-beginning 1))))

(defun +org/disable-tangling ()
  "Disable tangling source block at point."
  (interactive)
  (org-entry-put (point) "HEADER-ARGS" ":tangle no"))

(set! org-babel-default-header-args
      '((:session . "none")
        (:results . "silent")
        (:exports . "code")
        (:mkdirp  . "yes")
        (:cache   .  "no")
        (:noweb   .  "no")
        (:hlines  .  "no")
        (:tangle  .  "no")))

(set! org-tags-column 80)

(defun! +org/open-heading-above (&optional below)
  "Insert a heading above the current heading."
  (interactive)
  (ignore-errors (org-back-to-heading))
  (let! level (if (org-at-heading-p)
		          (car (org-heading-components))
		        1))
  (when (and below (org-at-heading-p))
    (or (outline-next-heading) (org-end-of-subtree)))
  (let->>! headline
    (org-ml-build-headline! :level level)
	(org-ml-headline-set-node-property "ID" (org-id-new)))
  (org-ml-insert (point) headline)
  (run-hooks 'org-insert-heading-hook))

(defun +org/open-heading-below ()
  (interactive)
  (+org/open-heading-above t))

(defun! +org/dwim-insert-src-block ()
  "Insert source block for the current headline if it does not already exist."
  (interactive)
  (let! lang (completing-read "Language: " (mapcar 'car org-src-lang-modes)))
  (let! headline (org-ml-parse-this-headline))
  (let! section (org-ml-headline-get-section headline))
  (when (--any-p (equal 'src-block (org-element-type it)) section) (return!))
  (let! src-block (org-ml-build-src-block :value "" :language lang))
  (snocing! (org-ml-headline-get-section headline) src-block)
  (org-ml-update-this-headline (-const headline)))

(defun! +org/dwim-edit-src-code ()
  (interactive)
  (mapc #'require '(edit-indirect org-ml))
  (unless (org-in-src-block-p) (org-next-block 1))
  (let! (beg end) (org-src--contents-area (org-ml-parse-this-element)))
  (let! parent-buffer (current-buffer))
  (edit-indirect-region beg end t))

(set! org-hide-emphasis-markers t)

(set! org-fontify-emphasized-text t)

(set! org-adapt-indentation nil)

(set! org-edit-src-persistent-message nil)

(defun! +org/update-tags ()
  "Update tags for the current entry."
  (interactive)
  (let->>! all
    (org-map-entries #'org-get-tags nil (+org-directory-files))
    (apply #'append)
    (-non-nil)
    (-uniq))
  (let! old (org-get-tags))
  (let! new (completing-read-multiple "Tags: " all))
  (let! updated (-uniq (append (-difference old new) (-difference new old))))
  (org-set-tags updated))

(set! org-id-track-globally t)
(set! org-id-locations-file (concat oo-cache-dir "org-id-locations"))

(set! org-id-method 'ts)

(set! org-src-window-setup 'plain)

(set! org-id-link-to-org-use-id t)

(defun! +org/add-tangle-header-arg ()
  "Add header arguments to current headline."
  (interactive)
  ;; Find tangle header arguments from other source blocks.
  (let! files (directory-files org-directory t "\\.org\\'"))
  (let! regexp ":tangle \\([^[:space:]]+\\)")
  (flet! tangle-file ()
         (awhen (org-entry-get (point) "HEADER-ARGS")
           (-second-item (s-match regexp (or it "")))))
  (let! header-args (-non-nil (-uniq (org-map-entries #'tangle-file nil files))))
  (let! choosen (completing-read "files: " header-args))
  ;; Now replace based on what was selected.
  (let! header-args (org-entry-get (point) "HEADER-ARGS" ))
  (org-entry-put (point) "HEADER-ARGS"))

(bind! (:map org-mode-map)
       (:n "TAB" #'oo/toggle-fold-subtree))

(defun! oo/toggle-fold-subtree ()
  "Toggle folding of current subtree."
  (interactive)
  (let! folded-p (outline-invisible-p (line-end-position)))
  (outline-toggle-children)
  (save-excursion
    (next-line 1)
    (when (org-at-drawer-p)
      (org-fold-hide-drawer-toggle))))

(defun +org/dwim-eval-src-block ()
  "Eval block contents."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not in source block"))
  (save-window-excursion
    (org-babel-execute-subtree)))

(set! org-src-lang-modes (add-to-list 'org-src-lang-modes '("emacs-lisp" . emacs-lisp)))

(oo-add-hook 'edit-indirect-after-creation-hook #'beginning-of-buffer)

(provide 'oo-org-config)
