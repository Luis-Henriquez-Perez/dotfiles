(require 'oo-base-utils)
(require 'oo-modification-macros)

;;; defer

;;; store the
;; I want the ability to be able to bind keys in keymaps without having to always
;; consider whether the keymap is bound yet or not; and without always having to
;; write non-declarative code such as ~(after! evil (evil-define-key* ...))~.  The
;; function =oo-bind= to be a "smart" binding function that binds keys if and only if
;; the keymap is bound.  Otherwise, it should defer the bindings until said keymap
;; exists.  The contents of this headline provides the framework for =oo-bind= to let
;; me do this.
(defvar oo-after-keymap-alist nil
  "An alist whose elements are of the form (KEYMAP . ((FN . ARGS) ...)).
Each key is a unique keymap symbol.  Every corresponding value is an alist whose
elements are of the form (FN . ARGS).")

;;; add the
;; This function is similar to [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]].  It is designed to be used
;; as the interface for adding stuff into [[id:20231018T175135.214308][oo-after-keymap-alist]].
(defun oo-call-after-keymap (keymap fn &rest args)
  "Call FN with ARGS after KEYMAP is bound.
If KEYMAP is already bound call FN with ARGS immediately.
KEYMAP is a keymap symbol."
  (cond ((or (not (symbolp keymap)) (boundp keymap))
         (apply fn args))
        (t
         (pushing! (alist-get keymap oo-after-keymap-alist) (cons fn args)))))

;;; Create the hook needed
;; To implement this behavior I add hook function to [[file:snapshots/_helpful_variable__after-load-functions_.png][after-load-functions]], an
;; abnormally named hook that is run after any file is loaded.  The hook function
;; evaluates the forms of any item of [[file:snapshots/_helpful_variable__oo-after-keymap-alist_.png][oo-after-keymap-alist]] whose keymap is bound.

;; An alternative to creating an alist would be to just add
;; individual hooks to =after-load-functions= that look something like ~(when (boundp
;; keymap) (do-binding))~.  However, then =N= function calls would happen during
;; after-load-functions and in a wasteful way at that because the same keymap would
;; be checked =N= times where =N= is the number of bindings for said map.  In my
;; variant only one function call happens--the call to
;; =oo-call-after-keymap-functions=--and each keymap symbol is checked only once.
(defun! oo-call-after-keymap-functions (&rest _)
  "Call the functions whose keymap has been loaded.
Evaluate and remove from all elements of `oo-after-keymap-alist'."
  (for! ((item &as keymap . alist) oo-after-keymap-alist)
    (cond ((boundp keymap)
           (for! ((fn . args) (reverse alist)) (apply fn args)))
          (t
           (pushing! remaining item))))
  (setq oo-after-keymap-alist (nreverse remaining)))

(add-hook 'emacs-startup-hook #'oo-call-after-keymap-functions)
(add-hook 'after-load-functions #'oo-call-after-keymap-functions)

;;; provide
(provide 'oo-call-after-keymap)
