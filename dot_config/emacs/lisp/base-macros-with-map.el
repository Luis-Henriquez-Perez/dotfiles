;;; base-macros-with-map.el -*- lexical-binding: t; -*-
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
;; Provide a macro `with-map!' that let-binds symbols in body based on a regular
;; expression.
;;
;;; Code:
(require 'base-macros-setters)
;;;; map!
(defun oo--generate-with-map-body (map body &optional use-keywords-p)
  "Return a list of let-bindings for `with-map!'.
Collect symbols matching REGEXP in BODY into an alist."
  (cl-flet* ((into-symbol (&rest args)
               (intern (intern (with-output-to-string (mapc #'princ args)))))
             (into-keyword (obj)
               (apply #'into-symbol ":" args)))
    (let* ((mapsym (gensym "map"))
           (let-binds `((,mapsym ,map)))
           (name nil)
           (symbol nil)
           (key nil)
           (key-fn (if use-keywords-p #'into-keyword #'into-symbol)))
      (dolist (obj (flatten-tree body))
        (when (and obj
                   (symbolp obj)
                   (string-match "\\(!\\{1,2\\}\\)\\([^[:space:]]+\\)"
                                 (symbol-name obj))
                   (not (assoc obj let-binds)))
          (setq symbol obj)
          (setq name (symbol-name symbol))
          (setq key (funcall key-fn (match-string 2 name)))
          (if (= 1 (length (match-string 1 name)))
              (push `(,symbol (map-elt ,mapsym ',key)) let-binds)
            (push `(,symbol (map-contains-key ,mapsym ',key)) let-binds))))
      (nreverse let-binds))))

(defmacro with-map-keywords! (map &rest body)
  "Let-bind bang symbols in BODY corresponding to keywords in MAP."
  (declare (indent 1))
  `(let* ,(oo--generate-with-map-body map body :use-keywords)
     ,@body))

(defmacro with-map! (map &rest body)
  "Let-bind bang symbols in BODY to corresponding keys in MAP.
Occurrences of !SYMBOL are let-bound to the result of evaluating (map-elt MAP
SYMBOL).  Occurrences of !!SYMBOL is let-bound to the result of evaluating
(map-contains-key MAP SYMBOL)."
  (declare (indent 1))
  `(let* ,(oo--generate-with-map-body map body)
     ,@body))
;;; provide
(provide 'base-macros-with-map)
;;; base-macros-with-map.el ends here
