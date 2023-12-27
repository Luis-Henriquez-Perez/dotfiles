(oo-bind :n "g," #'lispyville-comment-or-uncomment)
(oo-bind :n "gc" #'lispyville-comment-and-clone-dwim)
(oo-bind :n "gl" #'lispyville-comment-and-clone-dwim)

(oo-add-hook 'prog-mode-hook #'lispyville-mode)

(oo-bind 'lispyville-mode-map :i "SPC" #'lispy-space)
(oo-bind 'lispyville-mode-map :i ";" #'lispy-comment)

(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)
