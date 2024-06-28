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
;; Compared to `oo-base-utils' this library has functions that on external
;; packages loaded in `oo-base-requirements'.

;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;
;;; Code:
;;;; requirements
(require 'oo-base-utils)
(require 'oo-base-requirements)
(eval-when-compile (require 'oo-base-macros))

(defvar evil-state-properties)
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
  (--each-r (hash-table-keys oo-after-load-hash-table)
    (cond ((and (symbolp it) (boundp it))
           ;; (info! "Symbol `%s' is bound.  Evaluating corresponding forms..." item)
           (-each-r (gethash it oo-after-load-hash-table) #'funcall)
           (remhash it oo-after-load-hash-table))
          ((and (integerp it)
                ;; TODO: prevent it from calling featurep evil multiple times.
                (featurep 'evil)
                (set! state (oo--evil-char-to-state it)))
           ;; (info! "Evil %s state is defined.  Evaluating corresponding forms..." state)
           (-each-r (gethash it oo-after-load-hash-table) (-rpartial #'funcall state))
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
  (aif (and (bound-and-true-p evil-mode) (oo--evil-char-to-state char))
      (funcall fn it)
    (push fn (gethash char oo-after-load-hash-table))))
;;;; initial buffer choice
(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
              (get-buffer-create "*scratch*"))
    (lgr-info oo-lgr "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)
;;;; alternate bindings
;; Inspired by [[https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding][this]] stackoverflow question, this macro lets me create conditional
;; bindings for commands giving me a flexible and robust experience with key
;; bindings.  By "condition bindings" I mean key bindings that can invoke a
;; particular command based on certain conditions.  For example, =SPC h f=  might
;; invoke [[file:snapshots/_helpful_command__helpful_callable_.png][helpful-callable]] if the package helpful is present (see [[][]]), otherwise it
;; would fallback to [[file:snapshots/_helpful_command__describe-function_.png][describe-function]] instead.

;; As opposed to [[file:snapshots/_helpful_special_form__cond_.png][cond]], for example, which requires multiple conditions I designed
;; this macro to add one condition at a time.  I do not want to be tied to naming
;; all the conditions at once in general I write my configuration in such a way
;; that I can augment it incrementally as opposed to building one big block of
;; code.
(defvar oo-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(defun oo-alternate-command-choose-fn (command)
  "Return an alternate command that should be called instead of COMMAND."
  (or (oo-first-success #'funcall (gethash command oo-alternate-commands))
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
;;; provide
(provide 'oo-base-lib)
;;; oo-base-lib.el ends here
