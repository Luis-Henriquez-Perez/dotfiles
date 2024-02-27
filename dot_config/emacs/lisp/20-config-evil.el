(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! minibuffer-setup-hook&preserve-prior-evil-state ()
  "Save state before entering the minibuffer and enter insert state."
  ;; :on evil-mode
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! minibuffer-exit-hook&restore-prior-evil-state ()
  "Restore state after minibuffer."
  ;; :on evil-mode
  (when (bound-and-true-p evil-mode)
    (evil-change-state oo-evil-state-before-minibuffer)
    (setq oo-evil-state-before-minibuffer nil)))

(defun oo-set-default-evil-cursors (&rest _)
  "Set the evil cursors."
  (opt! evil-insert-state-cursor '((bar . 3) "chartreuse3"))
  (opt! evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
  (opt! evil-normal-state-cursor '(box "DarkGoldenrod2"))
  (opt! evil-visual-state-cursor '((hollow) "dark gray"))
  (opt! evil-operator-state-cursor '((hbar . 10) "hot pink"))
  (opt! evil-replace-state-cursor '(box "chocolate"))
  (opt! evil-motion-state-cursor '(box "plum3")))

(add-hook 'evil-mode-hook #'oo-set-default-evil-cursors)
(advice-add 'load-theme :after #'oo-set-default-evil-cursors)

(defvar oo-escape-hook nil
  "Hook run after escaping.")

(defun @exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((run-hook-with-args-until-success 'oo-escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        (t (keyboard-quit))))

(oo-bind :ie [escape] #'evil-force-normal-state)

(oo-add-advice #'evil-force-normal-state :after #'@exit-everything)

(oo-bind :n "J" #'evil-scroll-page-down)
(oo-bind :n "K" #'evil-scroll-page-up)

(provide '20-config-evil)
