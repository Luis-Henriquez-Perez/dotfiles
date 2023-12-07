(require 'oo-bind)
(require 'exwm)

(defun! oo--bind-exwm-key (fns metadata)
  "If map is `exwm-input-keys' use `exwm-input-set-key' instead of `define-key'."
  (let! keymap (map-elt metadata :keymap))
  (if (equal keymap 'exwm-input-keys)
      (-p-> (oo--do-binding metadata #'exwm-input-set-key :key :def)
            (oo-call-after-load 'exwm))
    (oo--resolve-binding fns metadata)))

(provide 'oo-bind-exwm-key)
