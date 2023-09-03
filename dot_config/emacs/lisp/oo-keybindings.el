(bind! (:map oo-override-mode-map)
       (:g   oo-emacs-leader-key  #'oo/leader-prefix-command)
       (:i   oo-insert-leader-key #'oo/leader-prefix-command)
       (:nmv oo-normal-leader-key #'oo/leader-prefix-command))

(bind! (:map oo-leader-map)
       (:prefix "l")
       ("l" #'dogears-go))

(bind! (:map oo-app-map "d" #'dired))

(bind! (:map dired-mode-map)
       (:nm "h" #'dired-up-directory))

(bind! (:n "gr" 'evil-operator-eval))

(bind! (:when (require 'eros nil t))
       (:alt eval-last-sexp eros-eval-last-sexp))

(bind! (:map evil-inner-text-objects-map "f" #'evil-cp-inner-form)
       (:map evil-outer-text-objects-map "f" #'evil-cp-a-form))

(bind! (:minor-mode org-src-mode)
       (:localleader org-src-mode-map)
       ("," #'org-edit-src-exit)
       ("a" #'org-edit-src-abort)
       ("c" #'org-edit-src-exit))

(defun! +org/dwim-edit-src-code ()
  "Edit nearest source block."
  (interactive)
  (unless (org-in-src-block-p) (org-next-block 1))
  (call-interactively #'org-edit-src-code))

(bind! (:alt org-edit-src-code +org/dwim-edit-src-code))

(bind! (:localleader org-mode-map)
       ("," #'org-edit-src-code)
       ((:wk "edit" :prefix "e")
	    (:wk "source block" "s" #'org-edit-src-code)))

(bind! (:localleader emacs-lisp-mode-map)
       (:wk "macrostep" :prefix "m")
       ("e" #'macrostep-expand)
       ("c" #'macrostep-collapse)
       ("C" #'macrostep-collapse-all))

(bind! (:map lispyville-mode-map)
       (:i "SPC" #'lispy-space)
       (:i ";" #'lispy-comment))

(bind! (:state visual)
       ("V" #'er/contract-region)
       ("v" #'er/expand-region))

(bind! (:map oo-leader-map)
       (:wk "file" "f" #'oo/file-prefix-command))

(bind! (:when (require 'consult nil t))
       (:alt switch-to-buffer consult-buffer)
       (:alt yank-pop consult-yank-pop)
       (:alt apropos consult-apropos)
       (:alt man consult-man))

(bind! (:map oo-leader-map)
       (:wk "search" "s" #'oo/search-prefix-command))

(bind! (:map oo-search-map)
       (:wk "line"        "f" #'consult-line)
       (:wk "line"        "s" #'consult-line)
       (:wk "line"        "l" #'consult-line)
       (:wk "outline"     "h" #'consult-outline)
       (:wk "org heading" "o" #'consult-org-heading))

(bind! (:prefix "g")
       (:n "," #'lispyville-comment-or-uncomment)
       (:n "c" #'lispyville-comment-and-clone-dwim)
       (:n "l" #'lispyville-comment-or-uncomment-line))

(bind! (:map oo-package-map)
       (:wk "browse" "b" #'elpaca-browse)
       (:wk "update all"     "U" #'elpaca-update-all)
       (:wk "update"         "u" #'elpaca-update)
       (:wk "visit"          "v" #'elpaca-visit)
       (:wk "try"            "i" #'elpaca-try)
       (:wk "rebuild"        "r" #'elpaca-rebuild)
       (:wk "delete"         "d" #'elpaca-delete)
       (:wk "log"            "l" #'elpaca-log)
       (:wk "write lockfile" "w" #'elpaca-write-lockfile)
       (:wk "manager"        "m" #'elpaca-manager))

(bind! (:alt org-capture +org/choose-capture-template))

(bind! (:v "E" #'lispy-eval-and-replace))

(bind! (:map oo-app-map)
       ("g" #'gumshoe-peruse-globally))

(bind! (:map org-mode-map)
       (:n "H" #'outline-up-heading))

(bind! (:when (require 'consult nil t))
       (:alt switch-to-buffer consult-buffer)
       (:alt yank-pop consult-yank-pop)
       (:alt apropos consult-apropos)
       (:alt man consult-man))

(bind! (:map oo-find-map)
       (:wk "line"        "s" #'consult-line)
       (:wk "line"        "l" #'consult-line)
       (:wk "outline"     "h" #'consult-outline)
       (:wk "org heading" "o" #'consult-org-heading))

(bind! (:map helm-map)
       (:ie "C-;" #'ace-jump-helm-line))

(bind! (:map oo-miscellany-map)
       ("l" #'consult-bookmark))


(bind! (:when (require 'helpful nil t))
       (:alt describe-function helpful-callable)
       (:alt describe-command helpful-command)
       (:alt describe-variable helpful-variable)
       (:alt describe-key helpful-key))

(bind! (:map vertico-map)
       (:state insert)
       ("TAB" #'vertico-next)
       ("C-k" #'vertico-previous)
       ("C-j" #'vertico-next)
       (";" #'vertico-quick-exit)
       ("C-;" #'vertico-quick-exit)
       ([backtab] #'vertico-previous))

(bind! ((:map vertico-map)
	    (:i "C-o" #'embark-act)
	    (:i "C-," #'embark-collect))
       (:n "C-o" #'embark-act))

(bind! (:map oo-leader-map "SPC" #'execute-extended-command)
       (:map oo-override-mode-map :nmv ";" #'execute-extended-command)
       (:i "A-x" #'execute-extended-command)
       (:i "M-x" #'execute-extended-command))

(bind! (:map oo-toggle-map)
       ("f" #'oo/set-font-face))

(bind! (:map oo-app-map)
       ("E" #'restart-emacs-start-new-emacs))

(bind! ((:map oo-quick-map)
        (:wk "capture" "j" #'org-capture))
       ((:map oo-app-map)
        (:wk "capture" "a" #'org-capture)
        (:wk "capture" "j" #'org-capture)))

(bind! (:localleader org-mode-map)
       (:wk "execute subtree" "E" #'org-babel-execute-subtree)
       ("w" #'widen)
       (:wk "narrow" "n" #'org-narrow-to-subtree)
       (:wk "refile" "r" #'org-refile)
       (:wk "tangle" "t" #'org-babel-tangle))

(bind! (:map oo-leader-map)
       (:wk "eval" :prefix "e")
       ("e" #'eval-expression))

(bind! (:localleader emacs-lisp-mode-map)
       (:wk "eval" :prefix "e")
       ("r" #'lispy-eval-and-replace)
       ("b" #'eval-buffer)
       ("d" #'eval-defun)
       ("e" #'eval-expression)
       ("l" #'eval-last-sexp)
       ("p" #'eval-print-last-sexp))

(bind! (:map oo-app-map "e" #'eshell))

(bind! (:nm "+" #'text-scale-increase)
       (:nm "-" #'text-scale-decrease))

(bind! (:map oo-toggle-map)
       ("r" #'read-only-mode)
       ("t" #'load-theme)
       ("c" #'caps-lock-mode)
       ("r" #'redacted-mode)
       ("s" #'smartparens-mode)
       ("d" #'toggle-debug-on-error))

(bind! (:map oo-miscellany-map "k" #'bookmark-set))

(bind! (:map oo-leader-map)
       (:wk "miscellany" "k" #'oo/miscellany-prefix-command))

(bind! (:map oo-help-map)
       ("i" #'info)
       ("m" #'describe-mode)
       ("f" #'describe-function)
       ("v" #'describe-variable)
       ("h" #'describe-variable)
       ("C" #'describe-char)
       ("k" #'describe-key)
       ("a" #'apropos)
       ("w" #'woman))

(bind! (:map org-mode-map)
       (:n "j" #'+org/dwim-next-visible-heading)
       (:n "k" #'+org/dwim-previous-visible-heading))

(bind! (:map org-mode-map)
       (:n "p" #'ignore))

(bind! (:map org-mode-map)
       (:n "Y" #'org-copy-subtree)
       (:n "D" #'org-cut-subtree)
       (:n "P" #'org-paste-subtree))

(bind! (:map org-mode-map)
       (:n "R" #'org-refile))

(bind! (:map org-mode-map)
       (:n "b" #'+org/dwim-insert-src-block))

(bind! (:map org-mode-map)
       (:n "o" #'+org/open-heading-below)
       (:n "O" #'+org/open-heading-above))

(bind! (:map org-mode-map)
       (:n "e" #'+org/dwim-eval-src-block))

(bind! (:map org-mode-map)
       (:n "t" #'+org/update-tags))

(bind! (:map org-mode-map)
       (:n ">" #'org-demote-subtree)
       (:n "<" #'org-promote-subtree))

(bind! (:map org-mode-map)
       (:n "K" #'org-metaup)
       (:n "J" #'org-metadown))

(bind! (:map oo-window-map)
       (:wk "left"  "h" #'windmove-left)
       (:wk "down"  "j" #'windmove-down)
       (:wk "up"    "k" #'windmove-up)
       (:wk "right" "l" #'windmove-right))

(bind! (:map oo-window-map)
       (:wk "delete"        "d" #'delete-window)
       (:wk "delete others" "D" #'delete-other-windows))

(bind! (:map oo-window-map)
       (:wk "vsplit"       "v" #'split-window-horizontally)
       (:wk "split"        "s" #'split-window-vertically)
       (:wk "vsplit+focus" "V" #'oo/split-window-right-and-focus)
       (:wk "split+focus"  "S" #'oo/split-window-below-and-focus))

(bind! (:map oo-window-map)
       ("M" #'maximize-window)
       ("m" #'minimize-window)
       ("b" #'balance-windows)
       ("z" #'zoom-window-zoom))

(bind! (:map oo-window-map)
       (:wk "left"         "h" #'windmove-left)
       (:wk "down"         "j" #'windmove-down)
       (:wk "up"           "k" #'windmove-up)
       (:wk "right"        "l" #'windmove-right)
       (:wk "ace"          "o" #'ace-window)
       (:wk "delete"       "d" #'delete-window)
       (:wk "delete others""D" #'delete-other-windows)
       (:wk "vsplit"       "v" #'split-window-horizontally)
       (:wk "split"        "s" #'split-window-vertically)
       (:wk "vsplit+focus" "V" #'oo/split-window-right-and-focus)
       (:wk "split+focus"  "v" #'oo/split-window-below-and-focus)
       (:wk "transpose"    "t" #'transpose-frame))

(bind! (:map oo-buffer-map)
       ("w" #'save-buffer)
       ("x" #'buffer-expose)
       ((:wk "kill" :prefix "k")
	    ("c" #'kill-current-buffer))
       ("j" #'pop-to-buffer)
       ("b" #'switch-to-buffer)
       ("n" #'next-buffer)
       ("p" #'previous-buffer)
       ("d" #'display-buffer))

(bind! (:map org-mode-map)
       (:n "C" #'org-refile-copy))

(provide 'oo-keybindings)
