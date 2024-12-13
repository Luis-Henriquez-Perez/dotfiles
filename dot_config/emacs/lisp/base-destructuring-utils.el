;;; base-destructuring-utils.el --- let-binding macro -*- lexical-binding: t; -*-
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
;; These functions build upon `pcase` to offer a customized destructuring mechanism.
;; At its core, this implementation simplifies the syntax compared to `pcase` by
;; eliminating the need for backquotes and commas, resulting in a lighter, more
;; concise syntax.
;;
;; Additionally, the design is influenced by destructuring patterns found in the
;; `dash` library, integrating similar concepts while tailoring them to fit this
;; implementation.
;;
;;; Code:
(require 'cl-lib)
(require 'pcase)

(defun oo-tree-map-nodes (pred fn tree)
  "Recursively map FN over tree nodes satisfying PRED.

PRED is a predicate function applied to each node in TREE.  TREE can be a nested
list, vector or improper list.  Return a new tree with FN applied to the nodes
matching PRED."
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

(defun oo-into-pcase-pattern (match-form)
  "Convert MATCH-FORM into a `pcase` pattern.

MATCH-FORM is a potentially nested structure containing lists, vectors, or
symbols.  This function transforms symbols in MATCH-FORM into pcase-compatible
patterns using backquote and comma syntax.

Return a pcase-compatible pattern."
  (if (symbolp match-form)
      match-form
    (cl-flet ((true-symbolp (o) (and o (symbolp o)))
              (add-comma (o) (list '\, o)))
      (list '\` (oo-tree-map-nodes #'true-symbolp #'add-comma match-form)))))

(defun oo-destructure-special-match-form (match-form value)
  "Generate `let*` bindings for handling special match forms.

MATCH-FORM is a destructuring pattern to be matched.  A special match-form
constitutes one of the following structures.

(&as WHOLE PARTS) Bind the value of current expression to WHOLE.

(&key KEY . KEYS) Bind each symbol in KEYS to (plist-get MATCH-FORM KEY)

(&map KEY . KEYS) Bind each symbol in key to (map-elt MATCH-FORM . KEY).

VALUE is the value being destructured.

If MATCH-FORM is not a special form, return nil."
  (pcase match-form
    (`(,(or '&as '&whole) ,(and whole (pred symbolp)) ,parts)
     (let ((it (cl-gensym "special-&as-match-form")))
       `((,it ,value)
         (,whole ,it)
         (,parts ,it))))
    (`(&key ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((it (cl-gensym "special-&key-match-form"))
           (bindings nil))
       (dolist (s (cons symbol symbols))
         (push `(,s (plist-get ,it ,(intern (concat ":" (symbol-name s))))) bindings))
       (push `(,it ,value) bindings)
       (nreverse bindings)))
    (`(&map ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((it (cl-gensym "special-&map-match-form"))
           (bindings nil))
       (dolist (s (cons symbol symbols))
         (push `(,s (map-elt ,it ,(intern (concat ":" (symbol-name s))))) bindings))
       (push `(,it ,value) bindings)
       (nreverse bindings)))
    (_
     nil)))

(defun oo-generate-special-match-form-bindings (match-form value)
  "Generate bindings for special forms in MATCH-FORM relative to VALUE.

Process MATCH-FORM to identify and replace any special forms, returning a list
where the first element is a transformed match-form with special forms replaced
and subsequent elements are additional bindings required to handle the special
forms.

MATCH-FORM is a destructuring pattern that may include special forms (see
`oo-destructure-special-match-form').  VALUE is the value to be matched and
destructured."
  (let (bindings match-form-value)
    (setq match-form-value (gensym "match-form-value"))
    (cl-flet ((special-mf-p (mf)
                (aprog1 (oo-destructure-special-match-form mf match-form-value)
                  (when it
                    (setq bindings (append bindings it))
                    (setq match-form-value (gensym "match-form-value")))))
              (replace-with-value (lambda (_) match-form-value)))
      `((,(oo-tree-map-nodes #'special-mf-p #'replace-with-value match-form) ,value)
        ,@bindings))))

(defun oo-pcase-bindings (match-form value)
  "Generate `pcase`-compatible bindings from MATCH-FORM and VALUE.

MATCH-FORM is the destructuring pattern that specifies how VALUE should be decomposed.
VALUE is the data to be matched and destructured.

Return a list of bindings compatible with `pcase`."
  (mapcar (pcase-lambda (`(,mf ,val)) (list (oo-into-pcase-pattern mf) val))
          (oo-generate-special-match-form-bindings match-form value)))

(defun oo-flatten-pcase-match-form (match-form)
  "Flatten MATCH-FORM into a list of components.

MATCH-FORM can contain nested lists or vectors. This function extracts all
symbols and other components, ensuring no duplicates.

Return a flat list of unique components in MATCH-FORM."
  (cl-flet ((flatten-pattern (match-form)
              (let ((stack (list (if (vectorp match-form) (append match-form nil) match-form)))
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
                symbols)))
    (cl-set-difference (flatten-pattern match-form) '(\, \`))))

(defmacro set! (pattern value)
  "Like `pcase-setq' but use the syntax of `let!'."
  (if (symbolp pattern)
      `(setq ,pattern ,value)
    ;; Damn I did not realize I need to know the gensym values.  I need to make
    ;; sure not to bind the gensym values.
    ;; I need flatten to also work for vectors.
    (let* ((binds (oo-pcase-bindings pattern value))
           (non-gensyms (cl-remove-if #'oo-list-marker-p (oo-flatten-pcase-match-form pattern)))
           (all (oo-flatten-pcase-match-form (mapcar #'car binds)))
           (gensyms (cl-set-difference all non-gensyms)))
      `(let ,gensyms
         ,(macroexp-progn (mapcar (apply-partially #'cons 'pcase-setq) binds))))))
;;; provide
(provide 'base-destructuring-utils)
;;; base-destructuring-utils.el ends here
