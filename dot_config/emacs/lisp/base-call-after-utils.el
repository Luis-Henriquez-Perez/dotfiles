;;; base-call-after-utils.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(defvar evil-state-properties)

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
       (eval-call-after feature fn)))
    (_
     (error "invalid expression `%S'" expr))))

(defun! oo-call-after-load (expr fn)
  "Call FN with ARGS after EXPR resolves.
EXPR can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of EXPRS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-call-after'.  If EXPR is a list of
EXPRS, call FN with ARGS only after all CONDITIONS have been met.  If
EXPR is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If EXPR is a list whose CAR is `:or', call FN with ARGS after any of
EXPRS in (CDR CONDITION) is met."
  (oo--call-after-load expr (oo-only-once-fn (oo-report-error-fn fn))))

;; This alist is meant to call certain functions whenever a file is loaded.  It
;; is meant for things could happen at any time.  Right now I use it for evil
;; state characters and knowing when a symbol is defined--specifically keymaps
;; which I use for keybindings and variable symbols used in `opt!'.

(defvar oo-call-after-hash-table (make-hash-table :size 100)
  "A hash table whose elements are (ITEM . FUNCTIONS).
ITEM is either a symbol or a character (an integer).  FUNCTIONS is a list of
functions.")

;; So remember with `oo-call-after-hash-table' that we push the elements in so if we
;; loop through it normally the first item processed is actually the last item
;; we entered into the list. I personally would expect the items to be processed
;; in the order I added them.

;; This is actually trickier than it seems.  Before I used to push the elements
;; I did not touch into a list and simply set the value of
;; `oo-call-after-hash-table' to that list.  But surprisingly, some
;; elements of the alist would disappear.  A long while later I realized why: a
;; side-effect of this looping is modifying the list.  By setting.  Instead, I
;; need to keep track of the elements I will remove.
(defun! oo-call-after-functions (&rest _)
  "Call functions in `oo-call-after-hash-table' that need to be called.
Also, update `oo-call-after-hash-table' to reflect functions called."
  (--each-r (hash-table-keys oo-call-after-hash-table)
    (cond ((and (symbolp it) (boundp it))
           ;; (info! "Symbol `%s' is bound.  Evaluating corresponding forms..." item)
           (-each-r (gethash it oo-call-after-hash-table) #'funcall)
           (remhash it oo-call-after-hash-table))
          ((and (integerp it)
                ;; TODO: prevent it from calling featurep evil multiple times.
                (featurep 'evil)
                (set! state (oo--evil-char-to-state it)))
           ;; (info! "Evil %s state is defined.  Evaluating corresponding forms..." state)
           (-each-r (gethash it oo-call-after-hash-table) (-rpartial #'funcall state))
           (remhash it oo-call-after-hash-table)))))

(defun oo-call-after-bound (symbol fn)
  "Call FN after SYMBOL is bound.
Call FN immediately if SYMBOL is already bound.  Otherwise, register
SYMBOL and FN in `oo-call-after-hash-table'."
  (if (boundp symbol)
      (funcall fn)
    (push fn (gethash symbol oo-call-after-hash-table))))

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
    (push fn (gethash char oo-call-after-hash-table))))
;;; provide
(provide 'base-call-after-utils)
;;; base-call-after-utils.el ends here
