(oo-bind :alt #'dired #'dirvish)

(oo-call-after-load 'dirvish #'dirvish-override-dired-mode 1)

(set! dirvish-use-mode-line nil)

(set! dirvish-attributes '(file-size all-the-icons subtree-state))

(set! dirvish-default-layout nil)
