
;;; initial line spacing
(add-hook 'org-mode-hook (lambda () (setq-local line-spacing 4)))

;;; org-src
(oo-popup-at-bottom "\\*Org Src")

(set! org-edit-src-persistent-message nil)

(adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))

(adjoin! org-src-lang-modes '("lua" . lua))

(set! org-src-ask-before-returning-to-edit-buffer nil)

(set! org-src-preserve-indentation t)
(set! org-edit-src-content-indentation 0)

(set! org-src-window-setup 'plain)

(oo-bind :alt #'org-edit-src-code #'oo-dwim-edit-src-code)
(oo-bind :h "," #'org-edit-src-code :localleader t)
;; (oo-bind 'org-mode-map "e" #'org-edit-src-code)
                                        ;(oo-bind ':h "es" #'org-edit-src-code :wk "source block" :localleader t)

(oo-bind 'org-src-mode-map "," #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "a" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "c" #'org-edit-src-exit :localleader t :mode 'org-src-mode)

;;; org-appear
(oo-add-hook 'org-mode-hook #'org-appear-mode)

(set! org-appear-autolinks t)

;;; org-auto-tangle
(set! org-auto-tangle-default t)

;;; org-superstar
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(set! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))

(set! org-superstar-leading-bullet ?\s)

(set! org-superstar-special-todo-items t)

;;; org-tidy
(oo-add-hook 'org-mode-hook #'org-tidy-mode)

(set! org-tidy-properties-style 'invisible)

;;; org-refile
(set! org-refile-allow-creating-parent-nodes t)

;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(set! org-refile-targets '((oo-directory-files :maxlevel . 10)))

(set! org-outline-path-complete-in-steps nil)

(set! org-refile-use-cache nil)

;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(set! org-refile-use-outline-path 'file)

;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
(set! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))

(defun! oo-choose-capture-template ()
  "Choose a capture template."
  (interactive)
  (dolist (template org-capture-templates)
    (let! (keys name) template)
    (collecting! alist (cons name keys)))
  (let! selected (completing-read "capture template: " alist nil t))
  (org-capture nil (alist-get selected alist nil nil #'string=)))

;;; org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")

(oo-bind 'oo-quick-map "j" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "a" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "j" #'org-capture :wk "capture")

(set! org-archive-save-context-info nil)

(set! org-archive-location (concat org-directory "archive.org::"))

(oo-add-hook 'org-insert-heading-hook #'org-id-get-create)

;;; org-id
(set! org-id-track-globally t)
(set! org-id-locations-file (concat oo-cache-dir "org-id-locations"))

(set! org-id-method 'ts)

(set! org-id-link-to-org-use-id t)

(defun oo-add-ids ()
  (interactive)
  (org-map-entries #'org-id-get-create))

;;; org-bookmark-heading
(oo-call-after-load '(org bookmark) #'require 'org-bookmark-heading)

(defun oo-directory-files ()
  "Return a list of org files in the `org-directory'."
  (directory-files org-directory t "\\.org\\'"))

(gv-define-expander org-ml-headline-get-section
  (lambda (do place)
    (gv-letplace (getter setter) place
      (funcall do `(org-ml-headline-get-section ,getter)
               (lambda (v)
                 (macroexp-let2 nil v v
                   `(progn
                      ,(funcall setter `(org-ml-headline-set-section ,v ,getter)))))))))

(defun! oo-org-heading-start ()
  (save-match-data
    (let! regexp (rx (seq bol (one-or-more "*")
                          (opt (one-or-more "/s")
                               (one-or-more upper))
                          (opt (one-or-more "/s")
                               "[#" nonl "]")
                          (group (1+ nonl)))))
    (looking-at regexp)
    (goto-char (1+ (match-beginning 1)))))

(defun! oo-dwim-previous-visible-heading (&optional next)
  (interactive)
  (let! case-fold-search nil)
  (if next
      (org-next-visible-heading 1)
    (org-previous-visible-heading 1))
  (org-back-to-heading)
  (oo-org-heading-start))

(defun! oo-dwim-next-visible-heading ()
  (interactive)
  (oo-dwim-previous-visible-heading t))

;; Promote all of the children of current headline.  Then remove the current
;; headline.
(defun oo-org-demote-children ()
  "Promote all the children of the current subtree."
  (interactive)
  (org-promote-subtree)
  (org-demote))

(defun! oo-get-paragraph-bounds ()
  "Return the beginning and end points of the paragraphs between the
property drawer and the source block in the current heading."
  (interactive)
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (oo-narrow-to-heading)
      (let! beg (progn (org-end-of-meta-data t) (point)))
      (let! elt (org-element-at-point))
      (while (and elt (equal 'paragraph (org-element-type elt)))
        (goto-char (org-element-property :end elt))
        (let! elt (org-element-at-point)))
      (let! end (point))
      (list beg end))))

(defun oo-remove-all-tags ()
  (interactive)
  (org-map-entries (-partial #'org-set-tags nil) nil nil))

(defun! oo-update-tags ()
  "Update tags for the current entry."
  (interactive)
  (let->>! all
    (org-map-entries #'org-get-tags nil (oo-directory-files))
    (apply #'append)
    (-non-nil)
    (-uniq))
  (let! old (org-get-tags))
  (let! new (completing-read-multiple "Tags: " all))
  (let! updated (-uniq (append (-difference old new) (-difference new old))))
  (org-set-tags updated))

(defun edit-indirect-before-commit-hook&insert-newline-maybe (&rest _)
  "Add a newline to edit-indirect buffers if they don't have one."
  (alet (buffer-string)
    (unless (or (string-empty-p it) (string-match-p (rx (1+ anything) "\n" eos) it))
      (insert (prog1 (concat (buffer-string) "\n") (erase-buffer))))))

(defun oo-narrow-to-heading ()
  "Narrow region to the heading at point.
By heading I mean the heading contents but not its subtree."
  (save-excursion
    (org-back-to-heading t)
    (let! narrow-beg (point))
    (let! narrow-end (save-excursion
                       (or (outline-next-heading) (end-of-buffer))
                       (point))))
  (narrow-to-region narrow-beg narrow-end))

(defun! oo-dwim-edit-paragraph ()
  "Edit consecutive paragraphs after a headline."
  (interactive)
  (let! edit-indirect-guess-mode-function (lambda (&rest _) (org-mode)
                                            (when (bound-and-true-p evil-mode)
                                              (evil-insert-state 1))))
  (let! (beg end) (oo-get-paragraph-bounds))
  (let! edit-indirect-after-creation-hook edit-indirect-after-creation-hook)
  (alet (lambda () (add-hook 'edit-indirect-before-commit-hook #'edit-indirect-before-commit-hook&insert-newline-maybe 100 t))
    (add-hook 'edit-indirect-after-creation-hook it))
  (edit-indirect-region beg end t))

(oo-bind 'edit-indirect-mode-map "," #'edit-indirect-commit :localleader t)
(oo-bind 'edit-indirect-mode-map "c" #'edit-indirect-commit :localleader t)
(oo-bind 'edit-indirect-mode-map "a" #'edit-indirect-abort :localleader t)
(oo-bind 'edit-indirect-mode-map "s" #'edit-indirect-save :localleader t)

(oo-bind 'edit-indirect-mode-map [remap save-buffer] #'edit-indirect-save)

(set! edit-indirect-guess-mode-function #'oo-edit-indirect-run-guess-mode-hook)

(defvar oo-edit-indirect-guess-mode-hook nil
  "Hooks run until a hook returns a function.
The function returned is called with no arguments and should set the mode of the
edit-indirect buffer.")

(defun! oo-edit-indirect-run-guess-mode-hook (parent-buffer beg end)
  (with-current-buffer parent-buffer
    (let! mode-fn (run-hook-with-args-until-success 'oo-edit-indirect-guess-mode-hook beg end)))
  (funcall (or mode-fn #'fundamental-mode)))

(oo-popup-at-bottom "\\*edit-indirect[^z-a]+")

(defun! oo-edit-indirect-guess-mode-from-src-block (beg _)
  "Call the mode the edit-indirect buffer should be in."
  (save-excursion
    (goto-char beg)
    (and (let! element (org-element-at-point))
         (equal 'src-block (org-element-type element))
         (let! lang (org-element-property :language element))
         (let! mode-fn (org-src-get-lang-mode lang))
         mode-fn)))

(oo-add-hook 'oo-edit-indirect-guess-mode-hook #'oo-edit-indirect-guess-mode-from-src-block)

(defun! oo-dwim-edit-src-code ()
  "Edit nearest source block."
  (interactive)
  (mapc #'require '(edit-indirect org-ml))
  (unless (org-in-src-block-p) (org-next-block 1))
  (let! (beg end) (org-src--contents-area (org-ml-parse-this-element)))
  (let! parent-buffer (current-buffer))
  (edit-indirect-region beg end t))

(defun oo-dwim-eval-src-block ()
  "Eval block contents."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not in source block"))
  (save-window-excursion
    (org-babel-execute-subtree)))

(defun! oo-has-src-block-p ()
  "Return non-nil if current headline has a source block."
  (save-excursion
    (let! beg (point))
    (let! end (or (outline-next-heading) (point-max)))
    (goto-char beg)
    (and (save-match-data (re-search-forward "^#\\+begin_src" end t)) t)))

(defun! oo-dwim-insert-src-block ()
  "Insert source block for the current headline if it does not already exist."
  (interactive)
  (let! lang (completing-read "Language: " (mapcar 'car org-src-lang-modes)))
  (let! headline (org-ml-parse-this-headline))
  (let! section (org-ml-headline-get-section headline))
  (when (--any-p (equal 'src-block (org-element-type it)) section) (return!))
  (let! src-block (org-ml-build-src-block :value "" :language lang))
  (snocing! (org-ml-headline-get-section headline) src-block)
  (org-ml-update-this-headline (-const headline)))

(defun! oo-add-tangle-header-arg ()
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

(defun oo-disable-tangling ()
  "Disable tangling source block at point."
  (interactive)
  (org-entry-put (point) "HEADER-ARGS" ":tangle no"))

(oo-add-hook 'org-insert-heading-hook #'evil-append-line :args '(1) :mode 'evil-mode)

(set! org-directory (expand-file-name "~/dotfiles"))

(set! org-default-notes-file null-device)

(set! org-adapt-indentation nil)

(setq org-tags-column 80)

(setq org-hide-emphasis-markers t)

(setq org-fontify-emphasized-text t)

;; ob-core
(set! org-babel-default-header-args
      '((:session . "none")
        (:results . "silent")
        (:exports . "code")
        (:mkdirp  . "yes")
        (:cache   .  "no")
        (:noweb   .  "no")
        (:hlines  .  "no")
        (:tangle  .  "no")))

(set! org-confirm-babel-evaluate nil)
