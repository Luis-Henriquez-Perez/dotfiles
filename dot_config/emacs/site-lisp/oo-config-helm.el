(set! helm-candidate-number-limit 50)

(oo-popup-at-bottom "\\*Helm")

(oo-bind 'helm-map :ie "TAB" #'helm-next-line)
(oo-bind 'helm-map :ie [backtab] #'helm-previous-line)
(oo-bind 'helm-map :ie "C-j" #'helm-next-line)
(oo-bind 'helm-map :ie "C-k" #'helm-previous-line)

(oo-bind 'helm-map :ie "C-a" #'helm-select-action)
(oo-bind 'helm-map :ie "C-m" #'helm-toggle-visible-mark-forward)
(oo-bind 'helm-map :ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
(oo-bind 'helm-map :ie "S-TAB" #'helm-mark-current-line)

;; (elpaca (helm :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))

;; (elpaca (helm-core :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))

(oo-bind :ie 'helm-map "C-;" #'ace-helm-jump-line)
