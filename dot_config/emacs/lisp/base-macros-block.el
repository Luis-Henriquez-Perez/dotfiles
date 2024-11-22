;;; base-macros-block.el -*- lexical-binding: t; -*-
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
;; This file provides a macro that makes assumption based on to
;;
;;; Code:
(require 'cl-lib)
(require 'pcase)
(require 'base-requirements)
(require 'base-utils)
(require 'base-macros-lef)
(require 'base-macros-let)
(require 'base-macros-for)
(require 'base-macros-setters)
;;;; block!
;;;;; helpers
(defun oo--parse-block (data forms)
  "Return an updated list of (DATA FORMS) based on contents of FORMS.
DATA is a plist.  FORMS is a list of forms.  For how FORMS is interpreted see
`block!'."
  (let ((zipper (treepy-list-zip forms)))
    (while (not (treepy-end-p zipper))
      (pcase (treepy-node zipper)
        (`(,(or 'cl-function 'function 'quote 'backquote) . ,_)
         (setq zipper (treepy-skip zipper)))
        (`(,(and loop (or 'for! 'loop! 'dolist! 'while 'dolist 'dotimes)) ,pred . ,body)
         (alet `(catch 'break! (,loop ,pred (catch 'continue! ,@body)))
           (setq zipper (treepy-replace zipper it)))
         (loop! (repeat 7) (setq zipper (treepy-next zipper))))
        (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
         (alet (cl-case name
                 ((maxing! maximizing!) most-negative-fixnum)
                 ((minning! minimizing!) most-positive-fixnum)
                 ((summing! adding! counting!) 0))
           (adjoining! (map-elt data :let) (list symbol it) :test #'equal :key #'car))
         (setq zipper (treepy-next zipper)))
        (`(set! ,(and sym (pred symbolp)) ,value . ,(and plist (guard t)))
         (adjoining! (map-elt data :let) (list sym (map-elt plist :init)) :key #'car)
         (setq zipper (treepy-replace zipper `(set! ,sym ,value)))
         (setq zipper (treepy-next zipper)))
        (`(set! ,(and pattern (or (pred listp) (pred vectorp))) ,_)
         (dolist (sym (oo--set-flatten pattern))
           (cl-pushnew (list sym nil) (map-elt data :let) :test #'equal :key #'car))
         (setq zipper (treepy-next zipper)))
        (`(,(or 'without! 'exclude!) . ,(and symbols (guard (cl-every #'symbolp symbols))))
         (setf (map-elt data :nolet) (cl-union (map-elt data :nolet) symbols))
         (setq zipper (treepy-remove zipper)))
        (`(,(and (pred (lambda (x) (member x '(nflet! noflet! flet! stub! label!
                                                labels!)))) stub) . ,args)
         (let* ((macro (or (and (member stub '(nflet! noflet!)) 'lef!)
                           (and (member stub '(flet! stub!)) 'cl-flet)
                           (and (member stub '(label! labels!)) 'cl-labels)))
                (form `(,macro ((,@args)) ,@(treepy-rights zipper))))
           (setq zipper (treepy-replace zipper form))
           (while (treepy-right zipper)
	         (setq zipper (treepy-remove (treepy-right zipper))))))
        (`(gensym! . ,symbols)
         (appending! (map-elt data :let) symbols)
         (setq zipper (treepy-skip zipper)))
        (_
         (setq zipper (treepy-next zipper)))))
    (list data (treepy-node zipper))))
;;;;; helpers
(defmacro return! (&optional value)
  "Exit `block!' and return VALUE."
  `(throw 'return! ,value))

(defmacro done! ()
  (return! nil))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of loop."
  `(throw 'continue! nil))
(defalias 'skip! 'continue!)

(defmacro gensym! (sym &rest syms)
  "Set symbol to."
  (macroexp-progn (mapcar (lambda (sym) `(setq ,sym (cl-gensym (symbol-name ',sym))))
                          (cons sym syms))))

(defmacro exclude! (&rest _)
  "Signal to `block!' not to let bind VARS.")
(defalias 'without! 'exclude!)

(defmacro stub! (name args &rest body)
  "Define a local function definition with `cl-flet'.
NAME, ARGS and BODY are the same as in `defun'.
Must be used in `block!'."
  (declare (indent defun))
  (ignore name args body))
(defalias 'flet! 'stub!)
(defalias 'noflet! 'stub!)
(defalias 'nflet! 'stub!)
;;;;; generate the body of block!
(defun oo--generate-block-body (forms &optional lets nolets wrappers)
  "Return the body for `block!'.
FORMS is the set of froms from which the resulting body will be generated.  LETS
is a list of symbols to be bound.  NOLETS is a list of symbols that should not
be bound and which takes precedence over LETS.  WRAPPERS a list of forms to wrap
around the resulting body."
  (let! (((data body) (oo--parse-block nil forms))
         (lets (append (mapcar #'list lets) (map-elt data :let)))
         (nolets (append nolets (map-elt data :nolet)))
         (wrappers (append wrappers (map-elt data :wrappers)))
         (binds (cl-remove-if (lambda (bind) (member (car bind) nolets)) lets)))
    (appending! wrappers `((catch 'return!) (let ,binds)))
    (oo-wrap-forms wrappers body)))
;;;;; main macro
(defmacro block! (&rest body)
  "Same as `cl-block' but modify BODY depending on particular forms.
The following describes possible modifications.

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
  (oo--generate-block-body body))

(provide 'base-macros-block)
;;; base-macros-block.el ends here
