(after! smartparens
  (sp-local-pair 'outshine-pairs "=" "=" :when '(sp-in-comment-p))
  (sp-local-pair 'outshine-pairs "~" "~" :when '(sp-in-comment-p))
  (sp-local-pair 'outshine-pairs "*" "*" :when '(sp-in-comment-p))
  (sp-local-pair 'outshine-pairs "/" "/" :when '(sp-in-comment-p)))

(oo-add-hook 'outshine-mode-hook #'sp-update-local-pairs :args '(outshine-pairs))

(oo-bind 'outshine-mode-map "TAB" #'outline-toggle-children)

(elpaca (outshine :fetcher github :repo "alphapapa/outshine" :ref "bf1eed1"))
