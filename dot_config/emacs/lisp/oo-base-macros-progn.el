;;; oo-progn-macro.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'cl-lib)
(require 'pcase)
(require 'oo-base-utils)
(require 'oo-base-macros-let)
(require 'oo-base-macros-loop)
(require 'oo-base-macros-ing)
;;;; block!
;;;;; helpers
(defun oo--parse-progn-bang (data forms)
  "Return an updated list of (DATA FORMS) based on contents of FORMS.
DATA is a plist.  Forms is a list of forms.  For how FORMS is interpreted see
`block!'."
  (pcase forms
    (`(,(or 'cl-function 'function 'quote 'backquote) . ,_)
     (list data forms))
    (`(,(and loop (or 'for! 'loop! 'dolist! 'while 'dolist 'dotimes)) ,pred . ,body)
     (pcase-let* ((`(,data1 ,forms) (oo--parse-progn-bang nil body)))
       (list (map-merge-with 'plist #'append data data1)
             `(catch 'break! (,loop ,pred (catch 'continue! ,@forms))))))
    (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
     (let ((value (cl-case name
                    ((maxing! maximizing!) most-negative-fixnum)
                    ((minning! minimizing!) most-positive-fixnum)
                    (counting! 0))))
       (adjoining! (map-elt data :let) (list symbol value) :test #'equal :key #'car))
     (list data forms))
    (`(,(or 'with! 'wrap!) . ,(and wrappers (pred listp)))
     (appending! (map-elt data :wrappers) wrappers)
     (list data nil))
    (`(,(or 'without! 'exclude!) . ,(and symbols (guard (cl-every #'symbolp symbols))))
     (unioning! (map-elt data :nolet) symbols)
     (list data nil))
    (`(,(or 'aset! 'alet! 'aset> 'aset>>) ,value)
     (adjoining! (map-elt data :let) (list 'it nil))
     (list data forms))
    (`((,(or 'aprog1! 'aprog!) ,value) . ,rest)
     (cl-pushnew (list 'it nil) (map-elt data :let) :test #'equal :key #'car)
     (pcase-let ((`(,data1 ,body) (oo--parse-progn-bang nil rest)))
       (list (append data data1) `((prog1 (setq it ,value) ,@body)))))
    (`(gensym! . ,(and names (guard (cl-every #'symbolp names))))
     (dolist (name names)
       (cl-pushnew (list name nil) (map-elt data :let) :key #'car :test #'equal))
     (list data forms))
    (`(set! ,(and pattern (or (pred listp) (pred vectorp))) ,value)
     (dolist (sym (oo--set-flatten pattern))
       (cl-pushnew (list sym nil) (map-elt data :let) :test #'equal :key #'car))
     (list data forms))
    (`(set! ,(and sym (pred symbolp)) ,value . ,(and plist (guard t)))
     (pushing! (map-elt data :let) (list sym (map-elt plist :init)))
     (list data `(setq ,sym ,value)))
    (`((,(or 'stub! 'flet!) . ,args) . ,rest)
     (let! (((data1 rest) (oo--parse-progn-bang nil rest)))
       (list (map-merge 'plist data data1) `((cl-flet ((,@args)) ,@rest)))))
    (`((,(or 'nflet! 'noflet!) . ,args) . ,rest)
     (let! (((data1 rest) (oo--parse-progn-bang nil rest)))
       (list (map-merge 'plist data data1) `((lef! ((,@args)) ,@rest)))))
    ((and (pred listp) (pred (not null)) (guard (listp (cdr forms))))
     (pcase-let ((`(,data1 ,forms1) (oo--parse-progn-bang nil (car forms)))
                 (`(,data2 ,forms2) (oo--parse-progn-bang nil (cdr forms))))
       (list (map-merge-with 'plist #'append data data1 data2)
             (cons forms1 forms2))))
    (_
     (list data forms))))
;;;;; helpers
(defmacro return! (&optional value)
  "Cause `block!' to exit and return VALUE.
See `block!'."
  `(throw 'return! ,value))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
See `block!'."
  `(throw 'break! ,value))

(defmacro gensym! (sym &rest syms)
  (macroexp-progn (mapcar (lambda (sym) `(setq ,sym (cl-gensym (symbol-name ',sym))))
                          (cons sym syms))))

(defmacro continue! ()
  "Skip the current iteration of loop.
See `block!'."
  `(throw 'continue! nil))
(defalias 'skip! 'continue!)

(defmacro exclude! (&rest _)
  "Signal to `block!' not to let bind VARS.
See `block!'.")
(defalias 'without! 'exclude!)

(defmacro stub! (name args &rest body)
  "Define a local function definition with `cl-flet'.
NAME, ARGS and BODY are the same as in `defun'.
Must be used in `block!'."
  (declare (indent defun))
  (ignore name args body))
(defmacro aprog1! (_))
(defalias 'aprog! 'aprog1!)
(defmacro aset! (value)
  `(setq it ,value))
(defalias 'alet! 'aset!)
(defalias 'flet! 'stub!)
(defalias 'noflet! 'stub!)
(defalias 'nflet! 'stub!)
;;;;; main macro
(defmacro progn! (&rest body)
  "Same as `cl-block' but modify BODY depending on particular forms.
The following describes possible modifications.

- (alet! VALUE)
- (aset! VALUE)
- (aset>! VALUE)
- (aset>>! VALUE)
Let bind the symbol `it' to VALUE.

- (set! SYM _ [:init EXPR])
Let bind SYM to nil.  If :init VAL is specified, let BIND SYM to EXPR.

- (maxing! SYM _ [:init EXPR])
Let bind SYM to `least-positive-fixnum'.

- (minning! SYM _ [:init EXPR])
Let bind SYM to `most-positive-fixnum'.

- (counting! SYM _ [:init EXPR])
Let bind SYM to 0.

- (INGMAC SYM VAL [:init EXPR])
INGMAC is a macro ending in \"ing!\" such as `appending!', `pushing!',
`collecting!', etc.

- (gensym! SYM)
Let bind SYM to nil.

- (wrap! . WRAPPERS)
Surround block body with WRAPPERS. WRAPPERS is as in `oo-wrap-forms'.

- (with! . WRAPPERS)
Same as wrap!.

- (exclude! . VARS)
Do not let bind any vars in VARS.

- (without! . VARS)
Same as `exclude!'.

- (stub! NAME ARGS . BODY)
Wrap subsequent forms with `(cl-flet ((NAME ARGS . BODY)))'.

- (nflet! NAME ARGS . BODY)
Wrap subsequent forms with `(lef! ((NAME ARGS)))'.

- (LOOP ...)
Wrap the loop with `(catch \\='break!) and its body.
with `(catch \\='continue!)'. LOOP can be `for!',
`dolist', `dolist!' or `while'.

Like `cl-block' `cl-return' and `cl-return-from' work in BODY."
  (declare (indent 0))
  (let! (((data body) (oo--parse-progn-bang nil body))
         ;; lets is an alist.
         (lets (map-elt data :let))
         ;; nolets is a list of symbols.
         (nolets (map-elt data :nolet))
         (binds (cl-remove-if (lambda (bind) (member (car bind) nolets)) lets))
         (wrappers `((catch 'return!) (let ,binds) ,@(map-elt data :wrappers))))
    (oo-wrap-forms wrappers body)))

(defmacro lambda! (args &rest body)
  `(lambda ,args (progn! ,@body)))

(provide 'oo-base-macros-progn)
;;; oo-progn-macro.el ends here
