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
(defvar oo--bind-steps nil
  "List of functions to be called one by one.")

(defun oo--bind-step-after-evil ()
  `((oo-call-after-load 'evil (lambda ()))))

(defun oo--bind-step-kbd (metadata steps)
  (with-map! metadata
    (cons `(setq ,!key (if (stringp ,!key) (kbd ,!key) ,!key))
          (oo--bind-steps metadata))))

(defun oo--bind-step-evil-bind (metadata steps)
  (with-map! metadata
    (-snoc (oo--bind-steps metadata)
           `(evil-define-key* ,!states ,!keymap ,!key ,!def))))

(defun! oo--bind-wk-fn (desc)
  (set! wk-fn #'which-key-add-keymap-based-replacements)
  `(lambda (keymap key def)
     (oo-call-after-load 'which-key ,wk-fn keymap key ,desc)
     (setq key (if (stringp key) (kbd key) key))
     (funcall this-fn keymap key def)))

(defun oo--bind-step-which-key (metadata steps)
  `((lef! ((define-key ,(oo--bind-wk-fn desc)))
      ,@(oo--bind-steps metadata))))

(defun oo--bind-step-define-key (metadata)
  (with-map! metadata
    (-snoc (oo--bind-steps metadata steps)
           `(define-key ,!map ,!key ,!def))))

(defun oo--bind-let-bind (forms)
  (dolist (item (flatten-list forms))
    (when (and (symbolp item))
      (push (list item (map-elt metadata item)) let-binds))))

(defun oo--bind-steps (metadata)
  "Return binding form."
  (funcall (car oo--bind-steps) metadata (cdr oo--bind-steps)))

(defun! oo--bind-let-fn (metadata form)
  (for! ((key symbol value) metadata)
    (collecting! let-binds (list symbol value)))
  `(let ,let-binds ,@form))

(defun oo--bind-normalize-args (args)
  "Return binding form.")

(defmacro bind! (&rest args)
  (oo--bind-generate-form oo--bind-steps))
;;; provide
(provide 'oo-base-macros-bind-bang)
;;; oo-base-macros-bind-bang.el ends here
