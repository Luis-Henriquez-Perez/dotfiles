(oo-add-hook 'emacs-startup-hook #'evil-mode)

;; Don't load everything at once.
(defhook! after-init-hook&load-evil ()
  [:depth 10]
  (require 'evil nil t))

;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)
;;;; evil-surround
(oo-add-hook 'prog-mode-hook #'evil-surround-mode)

(oo-add-hook 'text-mode-hook #'evil-surround-mode)
;;;; evil-cleverparens 
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)

(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;;; evil-operator 
(oo-bind :n "gr" #'evil-operator-eval)
;;;; evil-easymotion 
(autoload #'oo-goto-beginning-of-word "evil-easymotion")
(autoload #'oo-goto-end-of-word "evil-easymotion")
(autoload #'oo-goto-char "evil-easymotion")

(oo-bind :nv "w" #'oo-goto-beginning-of-word)
(oo-bind :nv "e" #'oo-goto-end-of-word)
(oo-bind :nv "f" #'oo-goto-char)
;;;; expand-region 
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
;;;; lispy 
(oo-bind :v "E" #'lispy-eval-and-replace)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)

(provide '19-init-evil)
;; 19-init-evil.el ends here
