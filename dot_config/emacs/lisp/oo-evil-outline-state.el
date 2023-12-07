;; This is a state for.

(require 'evil)
(require 'outline)
(require 'oo-base-library)

(oo-call-after-load '(evil outline) #'require 'oo-evil-outline-state)

(evil-define-state outline ())

(provide 'oo-evil-outline-state)
