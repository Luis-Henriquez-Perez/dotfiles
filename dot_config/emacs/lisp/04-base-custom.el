;;; 04-base-custom.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require '02-base-lib)
(require 'anaphora)
(require 'dash)
(require 'lgr)
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
;;;; if-not!
;; More often than not when I am using `if', the default else clause is simpler than
;; the then clause.  And in that case I end up having to wrap the then clause in
;; a `progn'. I want to invert the else clause and the if clause so I do not
;; need to include the extra `progn' in that case.  I also considered just
;; writing a macro that expands to an `if' with the then and else reversed, but
;; I think it might be confusing.
(defmacro if-not! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond) ,then ,@else))
;;;; autoloading
;; I tried making this as a macro called [[][catch-autoloads!]] but I ran in to
;; some issues.  First, my initial implementation ran into infinite recursion
;; during macroexpansion.  And then after I fixed that I had problems with lexical
;; binding.
(defun! oo-autoload-function (fn)
  "Call FN with ARGS trying to load features of any undefined symbols.
If an void-function or void-variable error is raised try to guess the parent
feature."
  (oo-cond-case-fn fn #'oo-autoload-action-function '(void-function void-variable)))

(defun! oo-candidate-features (fn paths)
  "Return a list of candidate features for FN.
FN is a function symbol."
  (set! fname (symbol-name fn))
  (for! (path paths)
    (set>>! base
      (directory-file-name path)
      (file-name-nondirectory)
      (file-name-sans-extension))
    (when (s-prefix-p base fname)
      (pushing! candidates (intern base))))
  (seq-sort-by (-compose #'length #'symbol-name) #'> candidates))

(defun! oo-autoload-action-function (error function args)
  "Try guess if feature is bound.
ERROR is either a void-variable or void-function error."
  (set! (type symbol . rest) error)
  (cl-assert (member type '(void-variable void-function)))
  (set! bound-fn (cl-case type (void-variable #'boundp) (void-function #'fboundp)))
  (set! candidates (oo-candidate-features symbol load-path))
  (for! (feature candidates)
    (require feature nil t)
    (when (funcall bound-fn symbol)
      (return! (apply #'oo-funcall-autoload function args))))
  (signal (car error) (cdr error)))
;;;; logging
(defvar oo-lgr (lgr-add-appender (lgr-get-logger "oo") (lgr-appender-buffer :buffer (get-buffer "*Messages*")))
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
  "Return a function that will report error instead of raising an error."
  (oo-condition-case-fn fn (lambda (e &rest _) (oo-report-error fn e))))
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
    (advice-add symbol how it)))
;;;;; defadvice!
(defmacro! defadvice! (name args &rest body)
  "Define an advice."
  (declare (indent defun))
  (set! (symbol how-name _) (oo-advice-components name))
  (set! how (cdr (assoc how-name oo-advice-how-alist)))
  `(progn
     (fset ',name (lambda ,args (block! ,@body)))
     (advice-add ',symbol ,how ',name)))
;;;;; silently
(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (shut-up (apply fn args)))
;;;; hooks
;;;;; oo-add-hook
;; No anonymous hooks allowed.
(defun! oo-add-hook (hook fsym &optional depth local)
  "Generate a function from FSYM and add it to HOOK.
Unlike `add-hook'."
  (aprog1! (intern (format "%s&%s" hook fsym)))
  (fset it (oo-report-error-fn fsym))
  (add-hook hook it depth local))
(defalias 'oo-generate-hook 'oo-add-hook)
(defalias 'oo-gen-hook 'oo-add-hook)
;;;;; oo-remove-hook
(defun oo-remove-hook (fsym &optional hook)
  "Remove FSYM from HOOK."
  (if (and function hook)
      (remove-hook hook fsym)
    (remove-hook (oo-hook fsym) fsym)))
;;;;; oo-hook
(defun! oo-hook (fsym)
  "Return the hook symbol for FSYM."
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp fsym))
  (alet! (symbol-name fsym))
  (when (string-match "\\(.+\\)&.+" it)
    (intern (match-string 1 it))))
;;;;; oo-hook-p
(defalias 'oo-hook-p 'oo-hook "Return non-nil if FSYM is a hook symbol.")
;;;;; defhook!
(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME.
NAME should be a hook symbol."
  (declare (indent defun))
  (set! hook (oo-hook name))
  (cl-assert hook t "%s is not a hook symbol" hook)
  (when (vectorp (car body))
    (aset! (append (pop body) nil))
    (set! params (list (or (map-elt it :depth) (map-elt it :append))
                       (map-elt it :local))))
  `(prog1 ',name
     (fset ',name (lambda ,args (block! ,@body)))
     (add-hook ',hook ',name ,@params)))
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
     (--each exprs (apply #'oo--call-after-load it fn)))
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
     (error "invalid condition `%S'" condition))))

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
  (aset! (oo-only-once-fn (oo-report-error-fn (apply #'apply-partially fn args))))
  (oo--call-after-load expr it))

;; (defmacro! defafter! (name expr &rest body)
;;   (declare (indent defun))
;;   (set! fn (intern (format "oo-after-load/%s" name)))
;;   (oo-call-after-load ',expr (lambda () (block! ,@body))))
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
;;;;; opt!
;; The reason this needs to be a macro is because `value' might not be evaluated
;; immediately.
;; TODO: need better error handling for when value producess an error.
(defmacro! opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (gensym! value-var)
  `(if (not (boundp ',symbol))
       (push (cons ',symbol ',value) oo-unbound-symbol-alist)
     (let ((,value-var (with-demoted-errors "Error: %S" ,value)))
       (aif (get ',symbol 'custom-set)
           (funcall it ',symbol ,value-var)
         (with-no-warnings (setq ,symbol ,value-var))))))
;;;; threading
;; I have written several functions whose last arguments are =FN= and =&rest args= such
;; as [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]] and [[file:snapshots/_helpful_function__oo-call-after-keymap_.png][oo-call-after-keymap]].  And when I was trying to compose
;; these functions together in the body of multiple =oo-bind= functions I realized
;; that composing more than two of such functions produced a very long line.  The
;; line would have several sharp quoted functions. And I found it difficult to read
;; and understand what's going on.  Usually, I try to imagine what I want and when
;; I did that I found myself writing a [[file:snapshots/_helpful_macro__thread-last_.png][thread-last]] form. This is similar to
;; [[file:snapshots/_helpful_macro__thread-last_.png][thread-last]] and [[file:snapshots/_helpful_macro__->>_.png][->>]].  Except it is designed for functions that need a function
;; symbol and function arguments as their arguments such as [[file:snapshots/_helpful_function__funcall_.png][funcall]] and
;; [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]].
(defmacro! thread-partial! (&rest body)
  (flet! sharpquote (obj) `(function ,obj))
  (flet! sharpquote-first (obj) (cons (sharpquote (car obj)) (cdr obj)))
  (append (-last-item body)
          (apply #'append (reverse (mapcar #'sharpquote-first (-butlast body))))))

(defalias '-partial-> 'thread-partial!)
(defalias '-p-> 'thread-partial!)

;; This function is used by captain and abbrev.  Instead of redefining it twice,
;; I prefer to place it here.
(defun! oo--in-string-or-comment-p ()
  "Return 'string if point is in a string, 'comment if in a comment, nil otherwise."
  (interactive)
  (set! ppss (syntax-ppss))
  (cond ((nth 3 ppss) 'string)   ; Inside a string
        ((nth 4 ppss) 'comment)  ; Inside a comment
        (t nil)))
;;; provide
(provide '04-base-custom)
;;; 04-base-custom.el ends here
