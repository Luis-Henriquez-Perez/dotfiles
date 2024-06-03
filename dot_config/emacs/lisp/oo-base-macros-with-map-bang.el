;;; oo-base-macros-with-map-bang.el -*- lexical-binding: t; -*-
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
(require 'oo-base-requirements)
(require 'oo-base-utils)
(require 'oo-base-macros-ing)
;;;; map!
(defun oo--map-let-binds (map body regexp &optional use-keywords)
  "Return a list of let-bindings for `with-map!'.
Collect symbols matching REGEXP in BODY into an alist."
  (let* ((mapsym (cl-gensym "map"))
         (let-binds `((,mapsym ,map)))
         (name nil)
         (key nil))
    (dolist (obj (flatten-tree body))
      (when (and obj
                 (symbolp obj)
                 (setq name (symbol-name obj))
                 (string-match regexp name)
                 (not (assoc obj let-binds)))
        (setq key (funcall (if use-keywords #'oo-into-keyword #'oo-into-symbol)
                           (match-string 1 name)))
        (push `(,obj (map-elt ,mapsym ',key)) let-binds)))
    (nreverse let-binds)))

(defmacro with-map! (map &rest body)
  "Let-bind bang symbols in BODY to corresponding keys in MAP."
  (declare (indent 1))
  `(let* ,(oo--map-let-binds map body "!\\([^[:space:]]+\\)" nil)
     ,@body))
;;; provide
(provide 'oo-base-macros-with-map-bang)
;;; oo-base-macros-with-map-bang.el ends here
