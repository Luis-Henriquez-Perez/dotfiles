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
;; These are functions that work together to destructure patterns provided by pcase.
;;
;;; Code:
(require 'cl-lib)
(require 'pcase)

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

(defun oo-into-pcase-pattern (match-form)
  "Return MATCH-FORM as a pcase pattern.
MATCH form is a potentially nested structure of only list, vectors and symbols."
  (if (symbolp match-form)
      match-form
    (cl-flet ((true-symbolp (o) (and o (symbolp o)))
              (add-comma (o) (list '\, o)))
      (list '\` (oo-tree-map-nodes #'true-symbolp #'add-comma match-form)))))

(defun oo-destructure-match-form (match-form value)
  (pcase match-form
    (`(,(or '&as '&whole) ,(pred symbolp) ,as-match-form)
     (alet! (cl-gensym "special-&as-match-form")
       `((,it ,value)
         (,whole ,it)
         (,parts ,it))))
    (`(&key ,(and symbol (pred symbolp)) . ,(and symbols (guard . t)))
     (let ((it (cl-gensym "special-&key-match-form"))
           (bindings))
       (dolist (s (cons symbol symbols))
         (push `(,symbol (plist-get ,it ,(oo-keyword-intern ,symbol))) temp))
       (push `(,it ,value) bindings)
       (nreverse bindings)))
    (_
     nil)))

(defun oo--mf-replace (match-form value)
  (let (other-bindings match-form-value)
    (setq match-form-value (gensym "match-form-value"))
    (cl-flet ((match-p ()
                (aprog1! (oo-destructure-match-form mf match-form-value)
                         (nif! it
                             (+ 1 1)
                           (setq other-bindings (append other-bindings it))
                           (setq match-form-value mf-value))))
              (replace (lambda (_) match-form-value)))
      `((,(oo-tree-map-nodes #'match-p #'replace match-form) ,value)
        ,@other-bindings))))

;; The first thing I need to do is replace the matches with their equivalent.
(defun oo-pcase-bindings (match-form value)
  "Return pcase-friendly list of bindings from BINDING."
  (mapcar (pcase-lambda (`(,mf ,val)) (list (oo-into-pcase-pattern mf) val))
          (oo--mf-replace match-form value)))

(defun oo-flatten-pcase-match-form (match-form)
  "Flatten MATCH-FORM into components.
This is similar to `flatten-list' but convert."
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
                symbols))))
  (cl-set-difference (flatten-pattern match-form) '(\, \`)))
;;; provide
(provide 'base-destructuring-utils)
;;; base-destructuring-utils.el ends here
