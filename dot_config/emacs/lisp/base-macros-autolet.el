;;; base-macros-autolet.el -*- lexical-binding: t; -*-
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
;; This file provides a macro that automatically let bindings symbol based on
;; certain indicators.  Thereby saving the user from having to.
;;
;;; Code:
;;;; requirements
(require 'cl-lib)
(require 'pcase)
(require 'base-macros-let)
(require 'base-macros-lef)
(require 'base-macros-setters)
;;;; helpers
(defmacro return! (&optional value)
  "Exit `autolet!' and return VALUE."
  `(throw 'return! ,value))

(defmacro done! ()
  "Same as `return!' but always throws nil."
  `(return! nil))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of loop."
  `(throw 'continue! nil))
(defalias 'skip! 'continue!)

(defmacro exclude! (&rest _)
  "Signal to `autolet!' not to let bind VARS.")
(defalias 'without! 'exclude!)

(defmacro stub! (name args &rest body)
  "Define a local function definition with `cl-flet'.
NAME, ARGS and BODY are the same as in `defun'.
Must be used in `autolet!'."
  (declare (indent defun))
  (ignore name args body))
(defalias 'flet! 'stub!)
(defalias 'noflet! 'stub!)
(defalias 'nflet! 'stub!)
;;;; main macro
;; Sometimes you do not want symbol to be auto let-bound to nil, you actually
;; want to just modify the original symbol without let-binding it at all.  In
;; that case use `:noinit' which tells `autolet!' not to bind specified symbols
;; at all.  Other times you want a symbol to be bound to something else than the
;; default.  For example, counting! starts at 0 by default but maybe you want to
;; start at 10, in that case you can do `:init' ((count 10)).  I suppose init
;; can be used as a single-line alternative to `let*'.
(defmacro autolet! (&rest body)
  "Automatically and bind variables based on indicators in BODY.

(set! SYM _) Let bind SYM to nil.  If :init VAL is specified, let BIND SYM to EXPR.
(maxing! SYM _) Let bind SYM to `least-positive-fixnum'.
(minning! SYM _) Let bind SYM to `most-positive-fixnum'.
(counting! SYM _) Let bind SYM to 0.
(INGMAC SYM VAL) INGMAC is a macro ending in \"ing!\" such as `appending!', `pushing!',
`collecting!', etc.
(stub! NAME ARGS . BODY) Wrap subsequent forms with `(cl-flet ((NAME ARGS . BODY)))'.
(nflet! NAME ARGS . BODY) Wrap subsequent forms with `(lef! ((NAME ARGS)))'.
(LOOP ...) Wrap the loop with `(catch \\='break!) and its body with `(catch \\='continue!)'. LOOP can be `for!',
`dolist', `dolist!' or `while'."
  (let ((bindings nil)
        (temp nil)
        (noinit nil)
        (init nil)
        (forms body)
        (quote-constructs '(quote function backquote cl-function))
        (lets '((nflet! . lef!)
                (noflet! . lef!)
                (flet! . cl-flet)
                (stub! . cl-flet)
                (label! . cl-labels)
                (labels! . cl-labels)))
        (loops '(for! loop! dolist! while dolist dotimes)))
    ;; Process initial settings in beginning of body.  Special keywords will be
    ;; :noinit and :init.
    (while (member (car forms) '(:noinit :init :let))
      (pcase (car forms)
        ((or :init :let)
         (pop forms)
         (setq init (append init (pop forms))))
        (:noinit
         (pop forms)
         (setq noinit (append noinit (ensure-list (pop forms)))))))
    (cl-labels ((process-form (form)
                  (pcase form
                    ;; Leave quoted forms as-is.
                    (`(,(pred (lambda (x) (memq x quote-constructs))) . ,_)
                     form)
                    ;; Match `(set! VAR VALUE)` and collect VARIABLE.
                    (`(set! ,(and symbol (pred symbolp)) ,_ . ,(guard t))
                     (cl-pushnew (list symbol nil) bindings :key #'car)
                     form)
                    ;; Handle set forms with mutiple values.
                    (`(set! ,(and pattern (or (pred listp) (pred vectorp))) ,_ . ,(guard t))
                     (dolist (sym (reverse (oo--set-flatten pattern)))
                       (cl-pushnew (list sym nil) bindings :key #'car))
                     form)
                    ;; Surround loops with a catch.
                    (`(,(and loop (pred (lambda (x) (memq x loops)))) ,pred . ,(and body (guard t)))
                     `(catch 'break! (,loop ,pred (catch 'continue! ,@(process-form body)))))
                    ;; Properly initialize variables in ingmacro declarations.
                    ;; Just for brevity I use string-match to check instead
                    ;; of listing all my ing macros but there has been a clash
                    ;; with org-ml that uses some macros that end in "ing!".
                    (`(,(and name (pred (lambda (x) (and (symbolp x) (string-match-p "ing!$" (symbol-name x)))))) ,symbol . ,(guard t))
                     (cl-case name
                       ((maxing! maximizing!)
                        (push `(,symbol most-negative-fixnum) bindings))
                       ((minning! minimizing!)
                        (push `(,symbol most-positive-fixnum) bindings))
                       ((summing! adding! counting!)
                        (push `(,symbol 0) bindings))
                       (t
                        (push `(,symbol nil) bindings)))
                     form)
                    ;; Handle special let shortcuts.
                    (`((,(and macro (pred (lambda (x) (assoc x lets)))) . ,args) . ,(and rest (guard t)))
                     `((,(alist-get macro lets) ((,@args)) ,@(process-form rest))))
                    ((pred null)
                     form)
                    ;; Recurse into lists.
                    ((pred listp)
                     (cons (process-form (car form)) (process-form (cdr form))))
                    ;; Recurse into vectors.
                    ((pred vectorp)
                     (vconcat (process-form (append form nil))))
                    ;; Leave other forms untouched.
                    (_
                     form))))
      (setq body (process-form forms)))
    ;; Normalize init format.
    (setq init (mapcar (lambda (x) (cond ((symbolp x)
                                          (list x nil))
                                         ((not (nthcdr 1 x))
                                          (append x (list nil)))
                                         (t
                                          x)))
                       init))
    (dolist (binding bindings)
      (when (and (not (member (car-safe binding) noinit))
                 (not (assoc (car-safe binding) init)))
        (push binding temp)))
    (setq bindings (append init temp))
    `(let ,bindings (catch 'return! ,@body))))
;;; provide
(provide 'base-macros-autolet)
;;; base-macros-autolet.el ends here
