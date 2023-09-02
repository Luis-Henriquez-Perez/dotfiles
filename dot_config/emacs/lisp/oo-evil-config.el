(set! evil-search-wrap nil)

(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! preserve-prior-evil-state (minibuffer-setup-hook)
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! restore-prior-evil-state (minibuffer-exit-hook)
  "Restore state after minibuffer."
  (when (bound-and-true-p evil-mode)
    (evil-change-state oo-evil-state-before-minibuffer)
    (setq oo-evil-state-before-minibuffer nil)))

(oo-add-hook 'after-init-hook #'require 'evil :depth 10)

(oo-add-hook 'after-init-hook #'evil-mode :depth 90)

(set! evil-echo-state nil)

;; Whether the cursor is moved backwards when exiting insert state.
(set! evil-move-cursor-back nil)

(set! evil-move-beyond-eol nil)

(defun oo-set-default-evil-cursors (&rest _)
  "Set the evil cursors."
  (when (bound-and-true-p evil-mode)
    (set! evil-insert-state-cursor '((bar . 3) "chartreuse3"))
    (set! evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
    (set! evil-normal-state-cursor '(box "DarkGoldenrod2"))
    (set! evil-visual-state-cursor '((hollow) "dark gray"))
    (set! evil-operator-state-cursor '((hbar . 10) "hot pink"))
    (set! evil-replace-state-cursor '(box "chocolate"))
    (set! evil-motion-state-cursor '(box "plum3"))))

(oo-add-hook 'evil-mode-hook #'oo-set-default-evil-cursors)
(oo-add-advice #'load-theme :after #'oo-set-default-evil-cursors)

(defvar oo-escape-hook nil
  "Hook run after escaping.")

(defun @exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
	     (abort-recursive-edit))
	    ((run-hook-with-args-until-success 'oo-escape-hook))
	    ((or defining-kbd-macro executing-kbd-macro) nil)
	    (t (keyboard-quit))))

(bind! (:ie [escape] #'evil-force-normal-state))

(oo-add-advice #'evil-force-normal-state :after #'@exit-everything)
(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)

(provide 'oo-evil-config)
