(require 'oo-modification-macros)
(require 'oo-hooking)

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

(defvar oo-initial-evil-key-bindings nil
  "A list of binding forms to be run after evil is loaded.")

(defvar oo-initial-evil-key-bindings-enabled-p nil
  "Whether forms in `oo-initial-evil-key-bindings-' have been evaluated.")

(defafter! evaluate-initial-evil-key-bindings (evil)
  "Evaluate forms in `oo-initial-evil-key-bindings'."
  (oo-log "Evaluating initial bindings...")
  (funcall `(lambda () (ignore-errors ,@oo-initial-evil-key-bindings)))
  (setq oo-initial-evil-key-bindings-enabled-p t))

(defhook! enable-deferred-key-binding-evaluation (emacs-startup-hook :depth 80)
  "Evaluate any deferred bindings.
Also, setup deferred binding evaluation in `after-load-functions'."
  (oo-eval-deferred-key-bindings-maybe)
  (oo-add-hook 'after-load-functions #'oo-eval-deferred-key-bindings-maybe))

(defvar oo-override-mode-map (make-sparse-keymap))

(define-minor-mode oo-override-mode
  "Global minor mode for higher precedence evil keybindings."
  :keymap oo-override-mode-map
  :global t)

(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)

(defun evil-mode-hook&make-intercept-map ()
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map oo-override-mode-map 'all t))

(oo-add-hook 'evil-mode-hook #'evil-mode-hook&make-intercept-map)

(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))

(defvar oo-deferred-key-bindings nil
  "An alist with elements of (KEYMAP . FORMS).
KEYMAP is the name of a keymap.  FORMS are a list of lisp forms that should be
evaluated when KEYMAP is bound.")

(defun! oo-eval-deferred-key-bindings-maybe (&rest _)
  "Evaluate any binding forms whose keymap has been loaded.
Evaluate forms from all elements of `oo-deferred-key-bindings' whose
KEYMAP is bound \(the elements of the form \(KEYMAP . FORMS\)\).  Additionally,
remove those elements from `oo-deferred-key-bindings'."
  (for! ((item &as map . forms) oo-deferred-key-bindings)
    (cond ((boundp map)
	       (pushing! forms `(oo--log-info "Evaluating %s bindings..." ',map))
	       (appending! body forms))
	      (t
	       (pushing! to-keep item))))
  (setq oo-deferred-key-bindings to-keep)
  (funcall `(lambda () ,@body)))

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo/leader-prefix-command 'oo-leader-map)
