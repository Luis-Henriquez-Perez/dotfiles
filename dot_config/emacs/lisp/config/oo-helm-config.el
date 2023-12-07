;; *** helm
;; :PROPERTIES:
;; :ID:       20230825T180146.580851
;; :END:
;; **** show me at least 50 candidates
;; :PROPERTIES:
;; :ID:       20230825T175801.129392
;; :END:
;; The candidate number limit is one indicator of helm's slowness.  Helm will only display.  Whereas
;; vertico has no problem showing me even =29710= candidates.
(set! helm-candidate-number-limit 50)
;; **** have helm buffer take up half of the screen
;; :PROPERTIES:
;; :ID:       fd10f0ce-95e4-4d2a-a0b6-4a841b48693b
;; :END:
(oo-popup-at-bottom "\\*Helm")
;; **** use vim-like commands to go to next and previous in helm
;; :PROPERTIES:
;; :ID:       20230830T080441.918935
;; :END:
;; Instead of what emacs normally uses, =C-n= and =C-p= for next and previous; use =j= instead of =n=
;; and =k= instead of =p=.  Also provide =TAB= and =S-TAB= (backtab) as a reasonable alternatives for
;; =C-j= and =C-k= respectively.
(oo-bind 'helm-map :ie "TAB" #'helm-next-line)
(oo-bind 'helm-map :ie [backtab] #'helm-previous-line)
(oo-bind 'helm-map :ie "C-j" #'helm-next-line)
(oo-bind 'helm-map :ie "C-k" #'helm-previous-line)
;; **** helm bindings
;; :PROPERTIES:
;; :ID:       20230829T191635.767527
;; :END:
(oo-bind 'helm-map :ie "C-a" #'helm-select-action)
(oo-bind 'helm-map :ie "C-m" #'helm-toggle-visible-mark-forward)
(oo-bind 'helm-map :ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
(oo-bind 'helm-map :ie "S-TAB" #'helm-mark-current-line)
;; **** use helm varients of common commands while helm is enabled
;; :PROPERTIES:
;; :ID:       20230829T191338.513238
;; :HEADER-ARGS: :tangle no
;; :END:
;; This should happen immediately.
(bind! (:when (bound-and-true-p helm-mode))
       (:alt oo-set-font-face helm-select-xfont)
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
