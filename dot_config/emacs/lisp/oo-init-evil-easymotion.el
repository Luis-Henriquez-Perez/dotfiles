;; These functions will trigger the loading of `evil-easymotion' which
;; will then trigger the loading of =oo-config-evil-easymotion=.
(autoload #'oo-goto-beginning-of-word "evil-easymotion")
(autoload #'oo-goto-end-of-word "evil-easymotion")
(autoload #'oo-goto-char "evil-easymotion")

(oo-bind :nv "w" #'oo-goto-beginning-of-word)
(oo-bind :nv "e" #'oo-goto-end-of-word)
(oo-bind :nv "f" #'oo-goto-char)

(set! evilem-style 'at)

(set! evilem-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))
