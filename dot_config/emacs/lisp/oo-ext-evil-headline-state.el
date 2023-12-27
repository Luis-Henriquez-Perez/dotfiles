(evil-define-state headline
  "Navigate and perform operations on org headlines"
  :tag " <H> "
  :suppress-keymap t)
(evil-set-initial-state 'org-mode 'headline)
(set! evil-headline-state-cursor '(box "violet"))

;; I eventually want to automate this for the creation of new states.  I think
;; it is bound to happen multiple times.
(oo-bind 'oo-override-mode-map :h ";" #'execute-extended-command)
(oo-bind 'oo-override-mode-map :h oo-normal-leader-key #'oo-leader-prefix-command)
(oo-bind :h [escape] (lambda () (interactive) (@exit-everything)))

(oo-bind :h "w" #'widen)
(oo-bind :h "n" #'org-narrow-to-subtree)

(oo-bind :h "x" #'evil-normal-state)
(oo-bind :h "C-j" #'evil-normal-state)
(oo-bind 'org-mode-map :n "C-j" #'evil-headline-state)

(oo-bind :h "RET" #'ignore)

(alet (lambda () (set-keymap-parent evil-headline-state-map (make-composed-keymap evil-motion-state-map evil-normal-state-map)))
  (oo-call-after-keymap 'evil-headline-state-map it))

(alet (lambda () (interactive)
        (org-fold-hide-subtree)
        (awhen (org-up-heading-safe)
          (org-fold-hide-subtree)
          (org-show-children)
          (oo-org-heading-start)))
  (oo-bind :h "H" it))
;; (awhen (org-up-heading-safe)
;;   (org-fold-show-children))

(after! evil-easymotion
  (defevilem! oo-goto-headline ()
              "Jump the beginning of the text at headline title."
              (:scope 'page)
              (:initial-point #'point-min)
              (org-next-visible-heading 1)
              (oo-org-heading-start)))

;; Select a headline to jump to.  Show the headline and hide all the siblings of
;; around that headline.
(defun! oo-goto-headline-and-open ()
  (interactive)
  (oo-goto-headline)
  (save-excursion
    (awhen (org-up-heading-safe)
      (org-fold-hide-subtree)
      (org-fold-show-children)))
  (org-show-children)
  (org-show-entry))

(oo-bind :h "f" #'oo-goto-headline-and-open)

(oo-bind :h "TAB" #'evil-toggle-fold)

(oo-bind :h "C" #'org-refile-copy)

(oo-bind :h "ed" #'oo-dwim-edit-paragraph :wk "edit paragraph" :localleader t)
(oo-bind :h "d" #'oo-dwim-edit-paragraph :wk "edit paragraph" :localleader t)

(oo-bind :h "r" #'org-refile :wk "refile")
(oo-bind :h "t" #'org-babel-tangle :localleader t :wk "tangle")

(oo-bind :h "j" #'oo-dwim-next-visible-heading)
(oo-bind :h "k" #'oo-dwim-previous-visible-heading)

(oo-bind :h "Y" #'org-copy-subtree)
(oo-bind :h "D" #'org-cut-subtree)
(oo-bind :h "P" #'org-paste-subtree)

(oo-bind :h "r" #'org-refile)
(oo-bind :h "R" #'org-refile)

(oo-bind :h "b" #'oo-dwim-insert-src-block)

(oo-bind :h "o" #'org-insert-heading-after-current)
(oo-bind :h "O" (lambda () (interactive)
                  (block! nil
                    (noflet! org-move-subtree-down (&rest _))
                    (funcall-interactively
                     #'org-insert-heading-after-current))))

(oo-bind :alt #'org-babel-execute-subtree #'oo-dwim-eval-src-block)
(oo-bind :h "ee" #'org-babel-execute-subtree :localleader t :wk "eval subtree")
(oo-bind :h "eE" #'org-babel-execute-subtree :localleader t :wk "eval subtree")
(oo-bind :h "E" #'org-babel-execute-subtree :wk "eval subtree")

(oo-bind :h ">" #'org-demote-subtree)
(oo-bind :h "<" #'org-promote-subtree)

(oo-bind :h "J" #'org-metadown)
(oo-bind :h "K" #'org-metaup)

(oo-bind 'org-mode-map :i [escape] (lambda () (interactive) (evil-headline-state 1) (@exit-everything)))

(provide 'oo-ext-evil-headline-state)
