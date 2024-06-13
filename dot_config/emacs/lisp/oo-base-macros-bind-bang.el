;;; oo-base-macros-bind-bang.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Macro for binding keys.
;;
;; I have coded and re-coded this macro countless times.  The last time I wrote
;; it as a function-based, but for performance ideally it would be macro based.
;;
;;; Code:
(require 'oo-base-macro)
;; The main data structure will be a metadata that I can access.  I will go through
;; it via `use-package' style recursion.
(defvar oo--bind-steps '(oo--bind-step-defer-map
                         oo--bind-step-evil-keyword
                         oo--bind-step-let-bind
                         oo--bind-step-which-key
                         oo--bind-step-kbd
                         oo--bind-step-kbd
                         oo--bind-step-evil-bind
                         oo--bind-step-bind)
  "List of functions to be called one by one.")

(defun! oo--bind-step-defer-map (metadata steps)
  (set! map (map-elt metadata keymap))
  (if (equal map 'global-map)
      (oo--bind-generate-body metadata (cdr steps))
    (set! keymap (gensym "keymap"))
    (set! body (oo--bind-generate-body metadata (cdr steps)))
    `(let ((,keymap ,map))
       (if (not (boundp ,keymap))
           (oo-call-after-keymap ,keymap (lambda () ,@body))
         ,@body))))

(defun! oo--bind-step-evil-keyword (metadata steps)
  (set! evil-keyword (map-elt metadata :evil-keyword))
  (if (not evil-keyword)
      (oo--bind-generate-body metadata (cdr steps))
    (dolist (letter (oo--evil-state-letters evil-keyword))
      (let ((metadata metadata))
        (pushing! forms (oo--bind-generate-body metadata (cdr steps)))))
    `((progn ,@(apply #'append forms)))))

(defun oo--bind-let-bind (metadata steps)
  (for! ((keyword value) metadata)
    (set! symbol (gensym (seq-rest (symbol-name keyword))))
    (pushing! new-metadata (list keyword symbol))
    (pushing! bindings (list symbol value)))
  `((let ,@(nreverse bindings)
      ,@(oo--bind-generate-body new-metadata (cdr steps)))))

(defun! oo--bind-step-which-key (metadata step)
  (set! which-key (or which-key))
  (set! wk-fn #'which-key-add-keymap-based-replacements)
  `(lambda (keymap key def)
     (oo-call-after-load 'which-key ,wk-fn keymap key ,desc)
     (setq key (if (stringp key) (kbd key) key))
     (funcall this-fn keymap key def))
  (if (not which-key)
      (oo--bind-generate-body new-metadata (cdr steps))
    `((lef! ()
        ,@(oo--bind-generate-body new-metadata (cdr steps))))))

;; (defun oo--bind-step-kbd (metadata steps)
;;   (with-map! metadata
;;     (cons `(setq ,!key (if (stringp ,!key) (kbd ,!key) ,!key))
;;           (oo--bind-generate-body metadata (cdr steps)))))

(defun oo--bind-step-evil-bind (metadata steps)
  (with-map! metadata
    (-snoc (oo--bind-generate-body metadata (cdr steps))
           `(evil-define-key* ,!states ,!keymap ,!key ,!def))))

(defun oo--bind-step-define-key (metadata steps)
  (with-map! metadata
    (-snoc (oo--bind-generate-body metadata (cdr steps))
           `(define-key ,!map ,!key ,!def))))

(defun oo--bind-steps (metadata)
  "Return binding form."
  (funcall (car oo--bind-steps) metadata (cdr oo--bind-steps)))

(defmacro bind! (&rest args)
  (oo--bind-generate-form oo--bind-steps))
;;; provide
(provide 'oo-base-macros-bind-bang)
;;; oo-base-macros-bind-bang.el ends here
