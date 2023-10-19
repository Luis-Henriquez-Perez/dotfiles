(require 'oo-additional-hooks)

;; This file provides a simple, generic extension to binding keys.

(defvar oo-bind-functions '(oo--bind-defer-by-keymap
                            oo--bind-defer-by-evil
                            oo--bind-add-which-key-description
                            oo--bind-ensure-kbd
                            oo--bind-evil-define-key
                            oo--bind-define-key)
  "List of functions.")

;; The idea behind this is using the same "model" as the `use-package' package.
;; We have one.
(defun oo--bind-do-binding (binding-functions metadata)
  (funcall (or (car binding-functions) #'ignore) (cdr binding-functions) metadata))

;; This is the main internal function for defining keys.  By taking the metadata
;; as the argument it is very versatile.
(defun oo--bind-define-key (&rest metadata)
  (oo--bind-do-binding oo-binding-functions metadata))

;; Create a hook that catches when a state is defined.  That way I can latch on
;; bindings that happen when a new state is defined.

;; How do I test this?
;; Defer
(defun oo--bind-defer-by-keymap (fns metadata)
  "Defer binding until its keymap is bound.
If METADATA's map is not bound defer the evaluation of the binding until it is
bound."
  (aif (metadata-get plist :map)
      (oo-call-after-keymap map #'oo--bind-do-binding fns metadata)
    (oo--bind-do-binding fns metadata)))

;; This is separate from `oo--bind-evil-define-key' so that
;; `oo--bind-add-which-key-description' and other future customizations which
;; may depend on `evil' to be loaded can be deferred.
(defun oo--bind-defer-by-evil (fns metadata)
  "Defer evil binding until after `evil' is loaded.
If METADATA has a `:states' keyword, defer it the evaluation of the binding
until after `evil' is loaded."
  (aif (metadata-get plist :states)
      (oo-call-after-load 'evil #'oo--bind-do-binding fns metadata)
    (oo--bind-do-binding fns metadata)))

(defun oo--bind-add-which-key-description (fns metadata)
  "Call proper which-key form."
  (if-let ((keymap (plist-get metadata :map))
           (key (plist-get metadata :key))
           (description (plist-get metadata :which-key))
           (which-key-fn #'which-key-add-keymap-based-replacements))
      (oo-call-after-load 'which-key which-key-fn keymap key description))
  (oo--bind-do-binding fns metadata))

(defun oo--bind-ensure-kbd (fns metadata)
  ""
  (alet (plist-get metadata :key)
    (setf (plist-get metadata :key) (if (stringp it) (kbd it) it))))

(defun oo--bind-evil-define-key (fns metadata)
  "Define an evil keybinding."
  ;; Can assume that `evil' is loaded when this function is called because of
  ;; `oo--bind-defer-by-evil'.
  ;; Check to see if its a state key and if its defined.
  (when-let ((states (plist-get metadata :states))
             (key (plist-get metatdata :key))
             (def (plist-get metadata :def))
             (map (plist-get metadata :map)))
    (dolist (state states)
      (evil-define-key* state keymap key def))
    (oo--bind-do-binding fns metadata)))

(defun oo--bind-define-key (fns metadata)
  "Bind a key normally."
  (when-let ((keymap (plist-get metadata :map))
             (key (plist-get metadata :key))
             (def (plist-get metadata :def)))
    (define-key keymap key def)))

;; These are examples of using the internal `oo--bind-define-key' function.
;; (oo--bind-define-key :map 'global-map :key "d" :def #'foo)
;; (oo--bind-define-key :states '(normal) :map global-map :key "d" :def #'foo)

;; The function variant provides more typical syntax.
(defun oo-define-key (keymap key def &rest metadata)
  (apply #'oo--bind-define-key '(:map keymap :key key :def def)))

;; Macro variant.  The main idea of the macro is to simply to allow.

(provide 'oo-bind-macro)
