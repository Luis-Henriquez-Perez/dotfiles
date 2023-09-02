(require 'oo-base)

(set! helm-candidate-number-limit 50)

(oo-popup-at-bottom "\\*Helm")

(bind! (:map helm-map)
       (:ie "C-;" #'ace-jump-helm-line))

(bind! (:map helm-map)
       (:ie "TAB" #'helm-next-line)
       (:ie "C-j" #'helm-next-line)
       (:ie "C-k" #'helm-previous-line))

(bind! (:map helm-map)
       (:ie "C-a" #'helm-select-action)
       (:ie "C-m" #'helm-toggle-visible-mark-forward)
       (:ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
       ;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
       (:ie "S-TAB" #'helm-mark-current-line))

(bind! (:when (bound-and-true-p helm-mode))
       (:alt oo/set-font-face helm-select-xfont)
       (:alt apropos                  helm-apropos)
       (:alt find-library             helm-locate-library)
       (:alt execute-extended-command helm-M-x)
       (:alt find-file                helm-find-files)
       (:alt locate                   helm-locate)
       (:alt imenu                    helm-semantic-or-imenu)
       (:alt noop-show-kill-ring      helm-show-kill-ring)
       (:alt recentf                  helm-recentf)
       (:alt switch-to-buffer         helm-mini))

(provide 'oo-helm-config)
