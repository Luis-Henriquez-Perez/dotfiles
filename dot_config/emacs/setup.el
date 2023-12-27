;;; setup leader maps
(defvar oo-toggle-map (make-sparse-keymap))
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(oo-bind 'oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")

(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(oo-bind 'oo-leader-map "p" #'oo/package-prefix-command :wk "package")

(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(oo-bind 'oo-leader-map "f" #'oo-find-prefix-command :wk "find")

(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(oo-bind oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(oo-bind oo-leader-map "l" #'oo-help-prefix-command :wk "help")
(oo-bind oo-leader-map "h" #'oo-help-prefix-command :wk "help")

(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(oo-bind 'oo-leader-map "a" #'oo-app-prefix-command :wk "app")

(oo-bind 'oo-find-map "d" #'switch-to-buffer)
(oo-bind 'oo-find-map "f" #'display-buffer)

(oo-bind 'oo-quit-map "q" #'save-buffers-kill-emacs :wk "quit")
(oo-bind 'oo-quit-map "r" #'restart-emacs :wk "restart")
;; Should make this one restart with no prompt, just automatically save buffers
;; and exit processes.
(oo-bind 'oo-quit-map "R" #'restart-emacs :wk "restart")
(oo-bind 'oo-quit-map "E" #'restart-emacs-start-new-emacs :wk "new instance")

(oo-bind 'oo-app-map "E" #'restart-emacs-start-new-emacs :wk "new instance")

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(oo-bind oo-leader-map "l" #'oo-help-prefix-command :wk "help")
(oo-bind oo-leader-map "h" #'oo-help-prefix-command :wk "help")

(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(oo-bind oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
(oo-bind 'oo-buffer-map "j" #'next-buffer)
(oo-bind 'oo-buffer-map "k" #'previous-buffer)
(oo-bind 'oo-buffer-map "x" #'kill-current-buffer)
(oo-bind 'oo-buffer-map "b" #'switch-to-buffer)

(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(oo-bind oo-leader-map "w" #'oo-window-prefix-command :wk "window")

(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (let! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

(oo-bind 'oo-toggle-map "f" #'oo-set-font-face)

(oo-bind 'oo-package-map "b" #'elpaca-browse     :wk "browse")
(oo-bind 'oo-package-map "U" #'elpaca-update-all :wk "update all")
(oo-bind 'oo-package-map "u" #'elpaca-update     :wk "update")
(oo-bind 'oo-package-map "v" #'elpaca-visit      :wk "visit")
(oo-bind 'oo-package-map "i" #'elpaca-try        :wk "try")
(oo-bind 'oo-package-map "r" #'elpaca-rebuild    :wk "rebuild")
(oo-bind 'oo-package-map "d" #'elpaca-delete     :wk "delete")
(oo-bind 'oo-package-map "l" #'elpaca-log        :wk "log")
(oo-bind 'oo-package-map "m" #'elpaca-manager    :wk "manager")

;; Emacs has a family of describe functions that are used for help and
;; introspection.  To name a few, there's [[file:snapshots/_helpful_command__describe-function_.png][describe-function]], [[file:snapshots/_helpful_command__describe-character_.png][describe-character]].
;; The command =describe-callable=  and =describe-variable= are the ones I use the most
;; by far and I them it to be accessible quickly.  The various snapshots you see
;; named are a result of these functions and you can already guess buy how many
;; such snapshots there are how much I use these commands.
(oo-bind oo-help-map "m" #'describe-mode)
(oo-bind oo-help-map "l" #'describe-function)
(oo-bind oo-help-map "f" #'describe-function)
(oo-bind oo-help-map "j" #'describe-variable)
(oo-bind oo-help-map "v" #'describe-variable)
(oo-bind oo-help-map "h" #'describe-variable)
(oo-bind oo-help-map "C" #'describe-char)
(oo-bind oo-help-map "k" #'describe-key)

;;; maximize a window with =M=
;; Of course sometimes you want to focus on a window more than others.  Typically
;; this is handled with =edwina= because it makes the master window take up more than
;; =50%= of the width--by default =55%= (see [[file:snapshots/_helpful_variable__edwina-mfact_.png][edwina-mfact]]).
(oo-bind 'oo-window-map "M" #'maximize-window :wk "maximize")

(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))

;;; splitting windows
;; I'm unsure about whether to have any bindings splitting windows.  Since =edwina=
;; automates the way I split windows I do not use these splitting commands much.
;; However, I'm not so sure whether I should remove them entirely.
(oo-bind 'oo-window-map "v" #'split-window-horizontally :wk "vsplit")
(oo-bind 'oo-window-map "h" #'split-window-vertically :wk "hsplit")
;; (oo-bind 'oo-window-map "V" #'oo-split-window-right-and-focus :wk "vsplit+focus")
;; (oo-bind 'oo-window-map "v" #'oo-split-window-below-and-focus :wk "split+focus")

;;; undo changes to window configuration with =u=
;; There's a global mode called [[https://www.emacswiki.org/emacs/WinnerMode#:~:text=Winner%20Mode%20is%20a%20global%20minor%20mode%20that,included%20in%20GNU%20Emacs%2C%20and%20documented%20as%20winner-mode.][winner-mode]] that allow you to undo changes to
;; your window configuration.
(oo-bind 'oo-window-map "u" #'winner-undo :wk "undo")

;;; delete a window with =D= or =d=
;; The letter =d= is both mnemonic for deleting windows and it is easy to press
;; because its own the home key.
(oo-bind 'oo-window-map "d" #'delete-window :wk "delete")
(oo-bind 'oo-window-map "D" #'delete-other-windows :wk "delete others")

;;; open a new window with =k=
;; This binding overlaps with.  My reasoning is you can think of it as opening a
;; new window.
(oo-bind 'oo-window-map "k" #'display-buffer :wk "open")

(defhook! enable-orderless (vertico-mode-hook :expire t)
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))
    (alet '(orderless-strict-leading-initialism orderless-initialism orderless-regexp)
      (set! orderless-matching-styles it))))

;;; Set theme to =modus-operandi=
(oo-add-hook 'after-init-hook #'load-theme :args '(modus-operandi))

(oo-bind 'oo-app-map "d" #'dired)

(oo-bind 'oo-toggle-map "s" #'smartparens-mode)

(oo-bind 'oo-toggle-map "r" #'read-only-mode)
(oo-bind 'oo-toggle-map "t" #'load-theme)
(oo-bind 'oo-toggle-map "d" #'toggle-debug-on-error)

(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(oo-add-hook 'prog-mode-hook #'hs-minor-mode)

(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

(oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)

(oo-add-hook 'on-first-input-hook #'minibuffer-depth-indicate-mode)

(oo-add-hook 'prog-mode-hook 'auto-fill-mode)

(oo-add-hook 'text-mode-hook #'visual-line-mode)

(oo-add-hook 'text-mode-hook #'auto-fill-mode)

(oo-bind 'oo-app-map "g" #'gumshoe-peruse-globally)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)

(oo-bind 'oo-toggle-map "c" #'caps-lock-mode)

(oo-bind 'oo-toggle-map "r" #'redacted-mode)

;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defadvice! disable-old-themes (around load-theme)
  "Disable old themes before loading new ones."
  (:args orig-fn &rest args)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(oo-bind :nm "+" #'text-scale-increase)
(oo-bind :nm "-" #'text-scale-decrease)

(oo-bind 'emacs-lisp-mode-map "e" nil :localleader t :wk "eval")
(oo-bind 'emacs-lisp-mode-map "eb" #'eval-buffer :localleader t)
(oo-bind 'emacs-lisp-mode-map "ed" #'eval-defun :localleader t)
(oo-bind 'emacs-lisp-mode-map "ee" #'eval-expression :localleader t)
(oo-bind 'emacs-lisp-mode-map "el" #'eval-last-sexp :localleader t)
(oo-bind 'emacs-lisp-mode-map "ep" #'eval-print-last-sexp :localleader t)

;;; bind =TAB= to toggle children in =outshine-mode=
;; I need a way of folding headings.  This is a quick fix until I can
;; adapt the headline state I wrote about into a generic outline state.
(oo-bind 'outshine-mode-map :n "TAB" #'outline-toggle-children)
