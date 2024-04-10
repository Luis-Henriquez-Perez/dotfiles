;;; oo-let-macro.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-ing-macros)
(require 'oo-utilities)
;;;; lef!
(defmacro lef! (bindings &rest body)
  "Bind each symbol in BINDINGS to its corresponding function during BODY.
BINDINGS is a list of either (SYMBOL FUNCTION), where symbol is the symbol to be
bound and FUNCTION is the function to bind it to; or (SYMBOL ARGS BODY).  In
each of BINDINGS if the symbol is an existing function symbol let-bind the
original function to `this-fn', otherwise bind `this-fn' to nil."
  (declare (indent 1))
  (let (binds orig-fn)
    (pcase-dolist (`(,sym . ,rest) bindings)
      (setq orig-fn (gensym "orig-fn"))
      (push `(,orig-fn (when (fboundp ',sym) (symbol-function ',sym))) binds)
      (push (list `(symbol-function ',sym)
                  (pcase rest
                    (`(,fn . nil)
                     `(lambda (&rest args)
                        (let ((this-fn ,orig-fn))
                          (apply ,fn args))))
                    (`(,args . ,function-body)
                     `(lambda ,args
                        (let ((this-fn ,orig-fn))
                          ,@function-body)))))
            binds))
    `(cl-letf* ,(nreverse binds) ,@body)))
;;;; let!
;; I want `oo-tree-map-nodes' to be more flexible.  I want it to accept maybe an
;; alist of (PRED . FN) as opposed to a PRED, FN.
(defun oo-tree-map-nodes (pred fn tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fn tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fn (car tree))
               (oo-tree-map-nodes pred fn (cdr tree))))
        ((vectorp tree)
         `[,@(mapcar (apply-partially #'oo-tree-map-nodes pred fn)
                     (append tree nil))])
        (t
         tree)))

(defun oo--to-pcase (match-form)
  "Return MATCH-FORM as pcase syntax.
MATCH form is a potentially nested structure of only list, vectors and symbols."
  (if (symbolp match-form)
      match-form
    (cl-flet ((true-symbolp (o) (and o (symbolp o)))
              (add-comma (o) (list '\, o)))
      (list '\` (oo-tree-map-nodes #'true-symbolp #'add-comma match-form)))))

(defvar oo--destruc-alist '((oo--&as-mf-p . oo--&as-mf-destruc)))

(defun oo--&as-mf-p (form)
  "Return non-nil if FORM is an `&as' match form pattern."
  (pcase form
    (`(,(or '&as '&whole) ,(and (pred symbolp) whole) ,part)
     t)
    (_
     nil)))

(defun oo--&as-mf-destruc (as-match-form value)
  "Return friendly bindings."
  (pcase-let* ((`(&as ,whole ,parts) as-match-form)
               (gsym (cl-gensym "special-&as-match-form")))
    `((,gsym ,value)
      (,whole ,gsym)
      (,parts ,gsym))))

(defun oo--mf-match-p (match-form)
  "Return non-nil if FORM matches any special match forms."
  (cl-some (lambda (pred) (funcall pred match-form))
           (mapcar #'car oo--destruc-alist)))

(defun oo--mf-destruc (match-form value)
  "Return"
  (cl-flet ((match-p (pcase-lambda (`(,pred . ,_)) (funcall pred match-form))))
    (pcase-let ((`(,_ . ,destruc-fn) (cl-find-if #'match-p oo--destruc-alist)))
      (funcall destruc-fn match-form value))))

(defun oo--mf-replace (match-form value)
  "Return list of bindings."
  (list  value)
  (let (other-bindings)
    (cl-flet ((replace (lambda (mf)
                         (cl-with-gensyms (mf-value)
                           (appending! other-bindings (oo--mf-destruc mf mf-value))
                           mf-value))))
      `((,(oo-tree-map-nodes #'oo--mf-match-p #'replace match-form) ,value)
        ,@other-bindings))))

(defun oo--to-pcase-let (match-form value)
  "Return BINDINGS."
  (mapcar (pcase-lambda (`(,mf ,val)) (list (oo--to-pcase mf) val))
          (oo--mf-replace match-form value)))

(defun oo--let-bind (bind)
  "Return a list of wrappers for binding BIND."
  (pcase bind
    ((pred symbolp)
     `((let* (,bind))))
    (`(,(pred symbolp) ,_)
     `((let* (,bind))))
    (`(,(or ':flet :stub) ,(and name (pred symbolp)) ,lambda)
     `((cl-flet ((,name ,lambda)))))
    (`(,(or ':flet :stub) ,(and name (pred symbolp)) ,args . ,body)
     `((cl-flet ((,name ,args . ,body)))))
    (`(#',(and fn (pred symbolp)) ,lambda)
     `((lef! ((,fn ,lambda)))))
    (`(,(or ':noflet :nflet) ,(and fn (pred symbolp)) ,lambda)
     `((lef! ((,fn ,lambda)))))
    (`(,(and mf (or (pred listp) (pred vectorp))) ,value)
     `((pcase-let* ,(oo--to-pcase-let mf value))))
    (_
     (error "Unknown predicate %S" bind))))

;; I do not think that sef extensions are crazy useful for `cl-letf' except for
;; `(symbol-function)'.  In terms of implementation I want to create a macro
;; that ties together all the "letters"--cl-letf, cl-flet, cl-labels,
;; cl-macrolet, etc.
(defmacro let! (bindings &rest body)
  "Bind BINDINGS during BODY.
This is like `let*' but it has two more kinds of possible bindings.

- (let! ((MATCH-FORM VALUE)) . BODY)
MATCH-FORM a nested form of vectors, list and non-nil symbols.
- (let!)

- (let! ((:lef foo FUNCTION)) . BODY)
- (let! ((:noflet foo FUNCTION)) . BODY)
- (let! ((#'foo FUNCTION)) . BODY)
Bind function via.

- (let! ((:lef foo FUNCTION)) . BODY)
- (let! ((:noflet foo FUNCTION)) . BODY)
- (let! ((#'foo FUNCTION)) . BODY)

- (let! ((:stub SYMBOL FN)) . BODY)
- (let! ((:flet SYMBOL FN)) . BODY)
Let bind function."
  (declare (indent 1))
  (when (vectorp bindings)
    (setq bindings `((,(aref bindings 0) ,(aref bindings 1)))))
  (oo-wrap-forms (mapcan #'oo--let-bind bindings) body))
;;;; set!
(defun oo-list-marker-p (obj)
  "Return non-nil if OBJ is a list marker.
List markers are symbols that begin with `&' such as are `&rest' and
`&optional'."
  (declare (pure t) (side-effect-free t))
  (char-equal ?& (seq-first (symbol-name obj))))

(defun oo--mf-flatten (pattern)
  "Same as `flatten-tree' but also flattens vectors."
  (let ((stack (list (if (vectorp pattern) (append pattern nil) pattern)))
        (symbols nil)
        (node nil))
    (while stack
      (cond ((null (car stack))
             (pop stack))
            ((listp (car stack))
             (setq node (pop (car stack)))
             (cond ((symbolp node)
                    (cl-pushnew node symbols))
                   ((nlistp (cdr-safe node))
                    (push (list (car node) (cdr node)) stack))
                   ((listp node)
                    (push node stack))
                   ((vectorp node)
                    (push (append node nil) stack))))
            (t
             (cl-pushnew (pop stack) symbols))))
    symbols))

(defun oo--set-flatten (pattern)
  "Same as `oo--mf-flatten' but do not include `,' or backquotes in output."
  (cl-set-difference (oo--mf-flatten pattern) '(\, \`)))

(defmacro set! (pattern value)
  "Like `pcase-setq' but use the syntax of `let!'."
  (if (symbolp pattern)
      `(setq ,pattern ,value)
    ;; Damn I did not realize I need to know the gensym values.  I need to make
    ;; sure not to bind the gensym values.
    ;; I need flatten to also work for vectors.
    (let* ((binds (oo--to-pcase-let pattern value))
           (non-gensyms (cl-remove-if #'oo-list-marker-p (oo--set-flatten pattern)))
           (all (oo--set-flatten (mapcar #'car binds)))
           (gensyms (cl-set-difference all non-gensyms)))
      `(let ,gensyms
         ,(macroexp-progn (mapcar (apply-partially #'cons 'pcase-setq) binds))))))
;;; provide
(provide 'oo-let-macro)
;;; oo-let-macro.el ends here
