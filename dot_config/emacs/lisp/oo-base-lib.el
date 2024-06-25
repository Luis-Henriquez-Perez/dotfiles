;;; oo-base-lib.el --- TODO: add commentary -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; TODO: add commentary
;;
;;; Code:
;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;; Code:
(require 'oo-base-utils)
(require 'oo-base-requirements)
(eval-when-compile (require 'oo-base-macros))
;;;; oo-first-success
;; This function is very similar to dash's [[file:snapshots/_helpful_function__-first_.png][-first]] or cl-lib's [[file:snapshots/_helpful_function__cl-find-if_.png][cl-find-if]].
;; These functions take a predicate and a list and they return the first element of
;; the list for which ~(pred element)~ returns non-nil.  The function =oo-first-success= also takes a
;; predicate and the list, but instead it returns the first non-nil return value of
;; ~(pred element)~.  For example, ~(oo-first-sucess 'numberp '(a t 0))~ would return
;; =t= instead of =0= as it would for =-first= or =cl-find-if= because ~(numberp 0)~ evaluates
;; to =t=. The name of this function is inspired by a similar function designed for
;; hooks [[file:snapshots/_helpful_function__run-hooks-with-args-until-success_.png][run-hook-with-args-until-success]].
(defun! oo-first-success (fn list)
  "Return the first non-nil (fn x) in LIST, else nil."
  (--each-while list (not (set! success (funcall fn it))))
  success)
;;;; oo-once-only-fn
(defun oo-only-once-fn (fn)
  "Return a function behaves the same as FN the first time it is called.
After the first call, it does nothing and returns nil.  Note that this function
must be evaluated with `lexical-binding' enabled."
  (let ((first-call-p t))
    (lambda (&rest args)
      (when first-call-p
        (setq first-call-p nil)
        (apply fn args)))))
;;;; logging
;; TODO: figure out how to change the log format
;; I do not really utilize the logging enough yet because I need to understand
;; `lgr' more.  I considered removing the package, but I still got it to work.
;; And logging a little is better than nothing.
(defvar oo-lgr (progn! (set! logger (lgr-get-logger "oo"))
                       (set! log-buffer (get-buffer-create "*lgr*"))
                       (lgr-add-appender logger (lgr-appender-buffer :buffer log-buffer)))
  "Object used for logging.")

(defmacro info! (msg &rest meta)
  `(lgr-info oo-lgr ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-lgr ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-lgr ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-lgr ,msg ,@meta))
;;;; reporting errors
(defun oo-report-error (fn error)
  "Register ERROR and FN in `oo-errors'."
  (error! "%s raised an %s error because of %s" fn (car error) (cdr error))
  (cl-pushnew (cons fn error) oo-errors :key #'car))

(defun oo-report-error-fn (fn)
  "Return a function that will report error instead of raising it."
  (oo-condition-case-fn fn (lambda (e &rest _) (oo-report-error fn e))))
;;;; silently
(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (shut-up (apply fn args)))
;;;; advices
;; Advices will be named advisee@ADVICE-ABBREVwhat-advice-does.
;;;;; oo-advice-how
(defvar oo-advice-how-alist '((BF . :before)
                              (AF . :after)
                              (AR . :around)
                              (OV . :override)
                              (AU . :after-until)
                              (BU . :before-until)
                              (FA . :filter-args)
                              (FR . :filter-return))
  "An alist of (HOW-ABBREV . HOW).
HOW is the same as in `advice-add'.  HOW-ABBREV is the abbreviation used in
advice names for HOW.")
;;;;; oo-advice-components
(defun! oo-advice-components (fsym)
  "Return a list of."
  (set! rx "\\(?:\\([^[:space:]]+\\)@\\(\\(?:A[FRU]\\|B[FU]\\|F[AR]\\|OV\\)\\)\\([^[:space:]]+\\)\\)")
  (set! name (symbol-name fsym))
  (flet! group (-compose #'intern (-rpartial #'match-string name)))
  (awhen (string-match rx name)
    (mapcar #'group (number-sequence 1 3))))
;;;;; add-advice
(defun! oo-add-advice (symbol how fsym &optional props)
  "Generate a new advice."
  (set! how-name (car (rassoc how oo-advice-how-alist)))
  (aprog1 (intern (format "%s@%s%s" symbol how-name fsym))
    (fset it fsym)
    (advice-add symbol how it props)))
;;;; hooks
;;;;; oo-add-hook
;; No anonymous hooks allowed.
(defun! oo-add-hook (hook fsym &optional depth local)
  "Generate a function from FSYM and add it to HOOK.
Unlike `add-hook'."
  (aprog1 (intern (format "%s&%s" hook fsym))
    (fset it (oo-report-error-fn fsym))
    (add-hook hook it depth local)))
(defalias 'oo-generate-hook 'oo-add-hook)
(defalias 'oo-gen-hook 'oo-add-hook)
;;;;; oo-remove-hook
(defun oo-remove-hook (fsym &optional hook)
  "Remove FSYM from HOOK."
  (if (and fsym hook)
      (remove-hook hook fsym)
    (remove-hook (oo-hook fsym) fsym)))
;;;;; oo-hook
(defun! oo-hook (fsym)
  "Return the hook symbol for FSYM."
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp fsym))
  (alet (symbol-name fsym)
    (when (string-match "\\(.+\\)&.+" it)
      (intern (match-string 1 it)))))
;;;;; oo-hook-p
(defalias 'oo-hook-p 'oo-hook "Return non-nil if FSYM is a hook symbol.")
;;;; popup
;; I don't yet know where to put this function.  So for now, here it goes.
(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (alet `(,regexp
          (display-buffer-at-bottom)
          (side bottom)
          (slot 1)
          (window-height 0.5)
          (window-parameters ((no-other-window t))))
    (push it display-buffer-alist)))
;;;; oo-call-after-load
(defun oo--call-after-load (expr fn)
  "Call FN after EXPR is met."
  (pcase expr
    (`(:or . ,exprs)
     (--each exprs (oo--call-after-load it fn)))
    (`(:and . ,exprs)
     (apply #'oo--call-after-load exprs fn))
    ((or (pred null) (and (pred symbolp) (pred featurep)))
     (funcall fn))
    (`(,expr . ,exprs)
     (oo--call-after-load expr (apply-partially #'oo--call-after-load exprs fn)))
    ((and feature (pred symbolp))
     (if (featurep feature)
         (funcall fn)
       (eval-after-load feature fn)))
    (_
     (error "invalid expression `%S'" expr))))

;; This macro is designed with the following goals in mind.
;; 1 - use one generic macro for most binding needs
;; 2 - log the variables I set and when they are being set
;; You'll get a warning when trying to bind a symbol that hasn't been defined yet.
;; So it's best to bind a package symbol only after the package has been loaded.
;; 3 - stop worrying about variables that haven't been bound
;; 4 - stop worrying about whether a variable is a custom variable or not
;; Some variables are custom variables.  Meaning they have some function that.
(defun! oo-call-after-load (expr fn &rest args)
  "Call FN with ARGS after EXPR resolves.
EXPR can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of EXPRS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-after-load'.  If EXPR is a list of
EXPRS, call FN with ARGS only after all CONDITIONS have been met.  If
EXPR is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If EXPR is a list whose CAR is `:or', call FN with ARGS after any of
EXPRS in (CDR CONDITION) is met."
  (alet (oo-only-once-fn (oo-report-error-fn (apply #'apply-partially fn args)))
    (oo--call-after-load expr it)))
;;;; customize variables
;;;;; oo-unbound-symbol-alist
(defvar oo-unbound-symbol-alist nil
  "An alist mapping an unbound symbol to an expression.
This alist is checked by the hook `after-load-functions&set-bound-symbols' for
any symbols that are now bound.")
;;;;; after-load-functions
;; I'll note that I push all the forms into a list and evaluate them all in the
;; body of one lambda as opposed to evaluating one lambda per form.  This is
;; important because lambda calls have an overhead that adds up.  It is far less
;; costly to invoke one lambda over N lambdas.
(defhook! after-load-functions&set-bound-symbols (&rest _)
  "Set symbols that have been bound to the result of their corresponding expr.
Check each symbol in `oo-unbound-symbol-alist', removing those that have already been
bound and setting them to the result of evaluating expr."
  (for! ((&as elt (symbol . expr)) oo-unbound-symbol-alist)
    (cond ((boundp symbol)
           (pushing! exprs `(opt! ,symbol ,expr)))
          (t
           (pushing! updated elt))))
  (setq oo-unbound-symbol-alist (nreverse updated))
  (when exprs (funcall `(lambda () ,@exprs))))
;;;; oo-call-after-bound
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

;; This function is similar to [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]].  It is designed to be used
;; as the interface for adding stuff into [[id:20231018T175135.214308][oo-after-keymap-alist]].
(defun oo-call-after-bound (keymap fn &rest args)
  "Call FN with ARGS after KEYMAP is bound.
If KEYMAP is already bound call FN with ARGS immediately.
KEYMAP is a keymap symbol."
  (cond ((or (not (symbolp keymap)) (boundp keymap))
         (apply fn args))
        (t
         (pushing! (alist-get keymap oo-after-keymap-alist) (cons fn args)))))

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
  (for! ((&as item (keymap . alist)) oo-after-keymap-alist)
    (cond ((boundp keymap)
           (for! ((fn . args) (reverse alist)) (apply fn args)))
          (t
           (pushing! remaining item))))
  (setq oo-after-keymap-alist (nreverse remaining)))

(defhook! emacs-startup-hook&setup-call-after-keymap-fns ()
  (oo-call-after-keymap-functions)
  (oo-add-hook 'after-load-functions #'oo-call-after-keymap-functions))
;;;; oo-call-after-evil-state
(defun! oo-evil-state (evil-keyword)
  "Return the state that corresponds to the state keyword."
  (flet! letter (state) (seq-first (symbol-name state)))
  (set! target (seq-first (seq-rest (symbol-name evil-keyword))))
  (--first (equal (letter it) target) (map-keys evil-state-properties)))

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

(defun! oo-call-undefined-state-functions (&rest _)
  "Call the functions."
  (for! ((&as item (state . alist)) oo-undefined-state-functions)
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
  (set! fn (-partial #'load-state-maybe function args))
  (oo-call-after-load 'evil #'mapc fn (-list states)))
;;; provide
(provide 'oo-base-lib)
;;; oo-base-lib.el ends here
