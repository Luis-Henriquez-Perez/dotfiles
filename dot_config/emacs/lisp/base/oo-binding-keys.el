(require 'oo-modification-macros)
(require 'oo-base-hook)
(require 'oo-base-definers)

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

(provide 'oo-binding-keys)
