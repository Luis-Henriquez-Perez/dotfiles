;;; hooks
(oo-add-hook 'after-init-hook #'evil-mode :depth 90)

(oo-add-hook 'after-init-hook #'require :args '(evil) :depth 10)

;;; base variables

;;;; don't echo the state 
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(set! evil-echo-state nil)

(set! evil-move-cursor-back nil)

(set! evil-move-beyond-eol nil)

(set! evil-search-wrap nil)

;;; evil-cleverparens
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)

;;; evil-surround
(oo-add-hook 'prog-mode-hook #'evil-surround-mode)
(oo-add-hook 'text-mode-hook #'evil-surround-mode)

;;; evil-operator
(oo-bind :n "gr" #'evil-operator-eval)

;;; evil-easymotion
(autoload #'oo-goto-beginning-of-word "evil-easymotion")
(autoload #'oo-goto-end-of-word "evil-easymotion")
(autoload #'oo-goto-char "evil-easymotion")

(oo-bind :nv "w" #'oo-goto-beginning-of-word)
(oo-bind :nv "e" #'oo-goto-end-of-word)
(oo-bind :nv "f" #'oo-goto-char)

;;; expand-region
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
