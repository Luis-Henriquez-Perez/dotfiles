(require 'oo-base-definers)
(require 'oo-block)

;; An evil state is defined whenever [[file:snapshots/_helpful_function__evil-put-property_.png][evil-put-property]] is invoked with
;; =evil-state-properties= as its first argument.  I know this from the definition of
;; [[file:snapshots/_helpful_macro__evil-define-state_.png][evil-define-state]].  Therefore, to create the proper hook I add an after advice
;; to =evil-put-property= that runs =oo-evil-define-state-hook= whenever its first
;; argument is =evil-state-properties=.  And also from the definition of
;; =evil-define-state= you can see the second argument of =evil-put-property= is the
;; evil state.
(defvar oo-after-define-evil-state-hook nil
  "Hook run after an evil state is defined.
Each function in this hook should accept one argument, the state being
defined.")

(defun! oo-run-after-define-evil-state-hook (var state &rest _)
  "Detect when an evil state is defined."
  (when (equal var 'evil-state-properties)
    (run-hook-with-args 'oo-after-define-evil-state-hook state)))

(oo-add-advice #'evil-put-property :after #'oo-run-after-define-evil-state-hook)

;; This is the same design model as the keymap.
(defvar oo-undefined-state-functions nil
  "An alist of (STATE . ALIST) where state is an evil state.
STATE is either an evil state or an evil state keyword.
ALIST is an alist of (FN . ARGS).")

(defun! oo-call-undefined-state-functions (_)
  "Call the functions."
  (for! ((state . alist) oo-undefined-state-functions)
    ;; If the state is a keyword and the letter matches the first letter of an
    ;; existing evil state, then eval the bindings.
    (if (or (and (keywordp state) (oo-evil-state state))
            (assoc state evil-state-properties))
        (mapc #'apply alist)
      (pushing! updated item)))
  (setq oo-undefined-state-functions (nreverse updated)))

(add-hook 'evil-mode-hook #'oo-call-undefined-state-functions)
(add-hook 'oo-after-define-evil-state-hook #'oo-call-undefined-state-functions)

;; This function is to help me deal with evil states that are not yet defined, but
;; I want to register a binding for them.

;; The way I wrote this function is noteworthy.  I can't just wrap it with an
;; =after!= block as I did initially because it uses the variables =states=, =function=
;; and =args=.
(defun! oo-call-after-evil-state (states function &rest args)
  "Call FUNCTION with ARGS after evil state STATE is defined.
STATE is either a state, list of states or an evil state keyword."
  (flet! load-state-maybe (function args state)
    (cond ((and (keywordp state) (assoc (oo-evil-state state) evil-state-properties))
           (apply function args))
          ((assoc state evil-state-properties)
           (apply function args))
          ((push (cons function args)
                 (alist-get state oo-undefined-state-functions)))))
  (let! fn (-partial #'load-state-maybe function args))
  (oo-call-after-load 'evil #'mapc fn (-list states)))

(provide 'oo-call-after-evil-state)
