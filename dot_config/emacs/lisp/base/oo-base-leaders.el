(require 'oo-bind-macro)

(defconst oo-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst oo-normal-localleader-key "SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-normal-localleader-short-key ","
  "A shorter alternative `oo-localleader-key'.")

(defconst oo-insert-leader-key "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst oo-insert-localleader-key "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-localleader-key "C-c l m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-emacs-localleader-short-key "C-c s"
  "A short non-normal `oo-localleader-key'.")

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo/leader-prefix-command 'oo-leader-map)

(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo/window-prefix-command 'oo-window-map)

(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo/buffer-prefix-command 'oo-buffer-map)

(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo/quit-prefix-command 'oo-quit-map)

(defvar oo-quick-map (make-sparse-keymap))
(define-prefix-command 'oo/quick-prefix-command 'oo-quick-map)

(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo/app-prefix-command 'oo-app-map)

(defvar oo-toggle-map (make-sparse-keymap))
(define-prefix-command 'oo/toggle-prefix-command 'oo-toggle-map)

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo/help-prefix-command 'oo-help-map)

(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)

(defvar oo-file-map (make-sparse-keymap))
(define-prefix-command 'oo/file-prefix-command 'oo-file-map)

(defvar oo-search-map (make-sparse-keymap))
(define-prefix-command 'oo/search-prefix-command 'oo-search-map)

(defvar oo-miscellany-map (make-sparse-keymap))
(define-prefix-command 'oo/miscellany-prefix-command 'oo-miscellany-map)

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo/leader-prefix-command 'oo-leader-map)

(bind! (:map oo-leader-map)
       (:wk "buffer" "b" #'oo/buffer-prefix-command))

(bind! (:map oo-leader-map)
       (:wk "quit" "q" #'oo/quit-prefix-command))

(bind! (:map oo-leader-map)
       (:wk "quick" "j" #'oo/quick-prefix-command))

(bind! (:map oo-leader-map)
       (:wk "app" "a" #'oo/app-prefix-command))

(bind! (:map oo-leader-map "t" #'oo/toggle-prefix-command))

(bind! (:map oo-leader-map "h" #'oo/help-prefix-command))

(bind! (:map oo-leader-map)
       (:wk "package" "p" #'oo/package-prefix-command))

(bind! (:map oo-quit-map)
       (:wk "quit emacs" "q" #'save-buffers-kill-emacs))

(bind! (:map oo-quit-map)
       (:wk "quit and restart" "r" #'restart-emacs))

(bind! (:map oo-leader-map)
       (:wk "window" "w" #'oo/window-prefix-command))

(provide 'oo-base-leaders)
