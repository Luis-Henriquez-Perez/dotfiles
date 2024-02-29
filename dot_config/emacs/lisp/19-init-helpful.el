(oo-bind :alt #'describe-function #'helpful-callable :feature 'helpful)
(oo-bind :alt #'describe-command  #'helpful-command  :feature 'helpful)
(oo-bind :alt #'describe-variable #'helpful-variable :feature 'helpful)
(oo-bind :alt #'describe-key      #'helpful-key      :feature 'helpful)

(provide '19-init-helpful)
;; 19-init-helpful.el ends here
