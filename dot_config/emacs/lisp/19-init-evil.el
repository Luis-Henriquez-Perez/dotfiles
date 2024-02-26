(oo-add-hook 'after-init-hook #'evil-mode :depth 90)

;; (oo-add-hook 'after-init-hook #'require :args '(evil) :depth 10)

;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)

(provide '19-init-evil)
