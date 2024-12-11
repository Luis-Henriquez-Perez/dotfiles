;;; base-lib.el --- external package library -*- lexical-binding: t; -*-
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
;; Compared to `base-utils' this library has functions that on external
;; packages loaded in `base-requirements'.

;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;
;;; Code:
;;;; requirements
(require 'base-packages)
(eval-when-compile (require 'lgr))
(require 'shut-up)
(eval-when-compile (require 'base-macros-hook))
(eval-when-compile (require 'base-macros-setters))
(eval-when-compile (require 'base-macros-for))
(eval-when-compile (require 'base-macros-let))
(eval-when-compile (require 'base-macros-lef))
(eval-when-compile (require 'base-macros-autolet))
(eval-when-compile (require 'base-macros-with-map))
(eval-when-compile (require 'base-macros-definers))
(eval-when-compile (require 'base-macros-bind))

(defvar evil-state-properties)
(declare-function evil-define-key* "evil")
;;;; anaphoric macros
(defmacro alet! (value &rest body)
  "Bind value to `it' and evaluate body."
  (declare (debug let) (indent 1))
  `(let ((it ,value))
     ,@body))

(defmacro aand! (&rest conditions)
  "Similar to `aand' but only bindings first condition to `it'."
  `(alet! ,(car conditions)
     (and it ,@(cdr conditions))))

(defmacro aif! (cond then &rest else)
  "Similar to `aand' but only bindings first condition to `it'."
  (declare (debug t) (indent 2))
  `(alet! ,cond (if it ,then ,@else)))

(defmacro awhen! (cond &rest body)
  "Similar to `aand' but only bindings first condition to `it'."
  (declare (debug when) (indent 1))
  `(aif! ,cond (progn ,@body) nil))
;;;; hooks
(defun! oo--hook-docstring (hook function)
  "Generate a docstring for hook function."
  ;; This is taken directly from the `s' library.  Right now, it is the only
  ;; function from there I use.  Not wanting to require s for just one short
  ;; function, I copied it is body here.
  (flet! word-wrap (len s)
    (save-match-data
      (with-temp-buffer
        (insert s)
        (let ((fill-column len))
          (fill-region (point-min) (point-max)))
        (buffer-substring (point-min) (point-max)))))
  (flet! docstring (&rest lines)
    (cond ((null lines)
           "")
          ((cdr lines)
           (concat (car lines) "\n" (word-wrap 80 (string-join (cdr lines) "\s\s"))))
          ((word-wrap 80 (car lines)))))
  (docstring (format "Call `%s' from `%s'." function hook)
             (format "Log call to `%s'." function)
             (format "If `oo-debug-p' is non-nil suppress and log any error raised by `%s'." function)))

(defun! oo-add-hook (hook function &rest args)
  "Generate a function that calls FUNCTION and add it to HOOK.
Generated function call FUNCTION and logs any errors.  If IGNORE-ARGS, then do
generated function does not pass in any of its given arguments to FUNCTION."
  (set! fname (intern (format "oo--%s--%s" hook function)))
  (set! depth (plist-get args :depth))
  (set! local (plist-get args :local))
  (set! ignore-args (plist-get args :ignore-args))
  (set! funcall-form (if ignore-args `(,function) `(apply #',function arglist)))
  (unless (fboundp fname)
    (fset fname `(lambda (&rest arglist)
                   (ignore arglist)
                   ,(oo--hook-docstring hook function)
                   (info! "HOOK: %s -> %s" ',hook ',function)
                   (condition-case err
                       ,funcall-form
                     (error
                      (if oo-debug-p
                          (signal (car err) (cdr err))
                        (error! "%s error from calling %s from %s because of %s"
                                (car err)
                                #',function
                                ',hook
                                (cdr err))))))))
  (add-hook hook fname depth local))
;;;; miscellaneous
(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms to be wrapped.  WRAPPERS are a list of forms
representing the wrappers to apply.  If WRAPPERS is empty, `progn' is added to
ensure the result is syntactically valid."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (append wrapper (list forms))))
  forms)
;;;; oo-in-string-or-comment-p
;; This function is used by captain and abbrev.
(defun oo-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment.
Specifically, return the symbol `string' if point is in a string, the symbol
`comment' if in a comment and nil otherwise."
  (declare (pure t) (side-effect-free t))
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))
;;;; oo-funcall-silently
(defun oo-funcall-silently (fn &rest args)
  "Call FN with ARGS without producing any output."
  (shut-up (apply fn args)))
;;;; logging
(defvar oo-logger (lgr-get-logger "main")
  "Object used for logging.")

(defvar oo-error-logger (lgr-get-logger "error")
  "Object used for logging errors.")

(autolet!
 ;; Define a formatter.
 (set! ts "%Y-%m-%d %H:%M:%S")
 (set! format "%t [%L] %m")
 (set! formatter (lgr-layout-format :format format :timestamp-format ts))
 (set! message-format "[%L] %m")
 (set! message-formatter (lgr-layout-format :format message-format))
 ;; Define the appenders.
 (set! log-buffer-appender (lgr-appender-buffer :buffer (get-buffer-create "*log*")))
 (set! message-buffer-appender (lgr-appender-buffer :buffer (get-buffer "*Messages*")))
 ;; Add the formatter to the appenders.
 (lgr-set-layout log-buffer-appender formatter)
 (lgr-set-layout message-buffer-appender message-formatter)
 ;; Add the appenders to the logger.
 (lgr-add-appender oo-logger log-buffer-appender)
 (lgr-add-appender oo-error-logger message-buffer-appender)
 (lgr-add-appender oo-error-logger log-buffer-appender))

;; I do not want to have to pass in the logger every single time.
(defmacro info! (msg &rest meta)
  `(lgr-info oo-logger ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-error-logger ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-logger ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-logger ,msg ,@meta))

(defmacro trace! (msg &rest meta)
  `(lgr-trace oo-logger ,msg ,@meta))

(defmacro debug! (msg &rest meta)
  `(lgr-debug oo-logger ,msg ,@meta))
;;;; popup
;; I don't yet know where to put this function.  So for now, here it goes.
(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (alet! `(,regexp
          (display-buffer-at-bottom)
          (side bottom)
          (slot 1)
          (window-height 0.5)
          (window-parameters ((no-other-window t))))
    (push it display-buffer-alist)))
;;;; keybinding stuff
(defun! oo-localleader-bind (keymap key def)
  "Convenience function for defining localleader bindings."
  (flet! leader (leader)
    (kbd (concat leader "\s" key)))
  (define-key keymap (leader oo-emacs-localleader-key) def)
  (with-eval-after-load 'evil
    (evil-define-key* 'emacs keymap (leader oo-emacs-localleader-key) def)
    (evil-define-key* 'normal keymap (leader oo-normal-localleader-key) def)
    (evil-define-key* 'normal keymap (leader oo-normal-localleader-short-key) def)
    (evil-define-key* 'insert keymap (leader oo-insert-localleader-key) def)
    (evil-define-key* 'insert keymap (leader oo-insert-localleader-short-key) def)))
;;;; oo-call-after-load
(defun oo--call-after-load (expr fn)
  "Call FN after EXPR is met."
  (pcase expr
    ((pred null)
     (funcall fn nil))
    (`(:or . ,exprs)
     (dolist (expr exprs)
       (oo-call-after-load expr fn)))
    (`(:and . ,exprs)
     (oo--call-after-load exprs fn))
    ((or `(,(and feature (pred symbolp))) (and feature (pred symbolp)))
     (if (featurep feature)
         (funcall fn feature)
       (eval-after-load feature (apply-partially #'oo--call-after-load feature fn))))
    (`(,expr . ,exprs)
     (oo--call-after-load expr `(lambda (_) (oo--call-after-load ',exprs #',fn))))
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
(defun oo-call-after-load (expr fn)
  "Call FN with ARGS after EXPR resolves.
EXPR can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of EXPRS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-after-load'.  If EXPR is a list of
EXPRS, call FN with ARGS only after all CONDITIONS have been met.  If
EXPR is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If EXPR is a list whose CAR is `:or', call FN with ARGS after any of
EXPRS in (CDR CONDITION) is met."
  (alet! (eval `(let ((first-call-p t))
                 (lambda (&optional feature)
                   (when first-call-p
                     (setq first-call-p nil)
                     (info! "AFTER-LOAD: %s -> %s" feature #',fn)
                     (condition-case err
                         (funcall #',fn)
                       (error
                        (cond (oo-debug-p
                               (signal (car err) (cdr err)))
                              (t
                               (error! "Error calling %s in %s because of %s"
                                       ',fn
                                       (car err)
                                       (cdr err)))))))))
              t)
    (oo--call-after-load expr it)))
;;;; oo-after-load-hash-table
;; This alist is meant to call certain functions whenever a file is loaded.  It
;; is meant for things could happen at any time.  Right now I use it for evil
;; state characters and knowing when a symbol is defined--specifically keymaps
;; which I use for keybindings and variable symbols used in `opt!'.

(defvar oo-after-load-hash-table (make-hash-table :size 100)
  "A hash table whose elements are (ITEM . FUNCTIONS).
ITEM is either a symbol or a character (an integer).  FUNCTIONS is a list of
functions.")

;; So remember with `oo-after-load-hash-table' that we push the elements in so if we
;; loop through it normally the first item processed is actually the last item
;; we entered into the list. I personally would expect the items to be processed
;; in the order I added them.

;; This is actually trickier than it seems.  Before I used to push the elements
;; I did not touch into a list and simply set the value of
;; `oo-after-load-hash-table' to that list.  But surprisingly, some
;; elements of the alist would disappear.  A long while later I realized why: a
;; side-effect of this looping is modifying the list.  By setting.  Instead, I
;; need to keep track of the elements I will remove.
(defun! oo-call-after-load-functions (&rest _)
  "Call functions in `oo-after-load-hash-table' that need to be called.
Also, update `oo-after-load-hash-table' to reflect functions called."
  (for! (reverse it (hash-table-keys oo-after-load-hash-table))
    (cond ((and (symbolp it) (boundp it))
           (for! (reverse fn (gethash it oo-after-load-hash-table))
             (funcall fn))
           (remhash it oo-after-load-hash-table))
          ((and (integerp it)
                (featurep 'evil)
                (set! state (oo--evil-char-to-state it)))
           (for! (reverse fn (gethash it oo-after-load-hash-table))
             (funcall fn state))
           (remhash it oo-after-load-hash-table)))))

(defun oo-call-after-bound (symbol fn)
  "Call FN after SYMBOL is bound.
Call FN immediately if SYMBOL is already bound.  Otherwise, register
SYMBOL and FN in `oo-after-load-hash-table'."
  (if (boundp symbol)
      (funcall fn)
    (push fn (gethash symbol oo-after-load-hash-table))))

(defun oo--evil-char-to-state (char)
  "Return state whose first letter is CHAR."
  (cl-find-if (lambda (state) (= char (string-to-char (symbol-name state))))
              (mapcar #'car evil-state-properties)))

;; These functions and variables have to be outside.  If you provide a symbol
;; for a nonexistent state evil will actually create a keymap for it.  When you
;; do define the state that corresponds to the symbol then it will end up
;; working out and that keymap that was initially created will be used.  This
;; means I actually do not need this for evil state symbols.  The reason then
;; that I need this is for the state characters I use to abbrev evil states.
(defun oo-call-after-evil-state-char (char fn)
  "Call FN with state after state starting with CHAR is defined."
  (aif! (and (bound-and-true-p evil-mode) (oo--evil-char-to-state char))
      (funcall fn it)
    (push fn (gethash char oo-after-load-hash-table))))
;;;; opt!
;; The reason this needs to be a macro is because `value' might not be evaluated
;; immediately.
;; TODO: need better error handling for when value producess an error.
(defmacro! opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let ((value-var (gensym "value")))
    `(if (not (boundp ',symbol))
         ;; This quote on he lambda is needed to avoid infinite recursion.
         (push '(lambda () (opt! ,symbol ,value))
               (gethash ',symbol oo-after-load-hash-table))
       (let ((,value-var (with-demoted-errors "Error: %S" (with-no-warnings ,value))))
         (aif! (get ',symbol 'custom-set)
             (funcall it ',symbol ,value-var)
           (with-no-warnings (setq ,symbol ,value-var)))))))
;;;; alternate bindings
;; https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding
(defvar oo-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(defun! oo-alternate-command-choose-fn (command)
  "Return an alternate command that should be called instead of COMMAND."
  (or (dolist (it (gethash command oo-alternate-commands))
        (aand! (funcall it) (break! it)))
      command))

;; (defun! oo-alt-bind (map orig alt &optional condition)
;;   "Remap keys bound to ORIG so ALT is called if CONDITION returns non-nil.
;; ORIG and ALT are command symbols.  CONDITION is a function that returns non-nil
;; when ALT should be invoked instead of ORIG."
;;   (flet! oo-when-fn (condition fn)
;;     `(lambda (&rest _) (when (funcall #',condition) #',alt)))
;;   (push (oo-when-fn (or condition #'always) alt) (gethash orig oo-alternate-commands))
;;   (define-key map `[remap ,orig] `(menu-item "" ,orig :filter oo-alternate-command-choose-fn)))

;; (defun oo-alt-bind (orig def)
;;   (let ((,orig ,key)
;;         (,alt ,def))
;;     (setq ,key (vconcat (list 'remap ,key)))
;;     (setq ,def (list 'menu-item "" ,alt :filter #'oo-alternate-command-choose-fn))
;;     (push ,(oo--lambda-form alt '(&rest ) `(when ,condition ,alt)) (gethash ,orig oo-alternate-commands))
;;     ,@(oo--bind-generate-body metadata steps)))
;;;; alt!
(defmacro alt! (old new feature)
  `(progn (push (lambda (&rest _) (when (or (featurep ',feature) (require ',feature nil t)) ',new))
                (gethash ',old oo-alternate-commands))
          (define-key global-map [remap ,old] '(menu-item "" ,old :filter oo-alternate-command-choose-fn))))
;;; provide
(provide 'base-lib)
;;; base-lib.el ends here
