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
;; Since getting a keybinding correct is so precise I have designed this as a
;; set of functions that will be invoked recursively to generate a form.  The
;; design is very similar to that of `use-package'.  The idea is to maximize
;; flexibility while maintaining very modular and extensible code.  Unlike
;; `use-package' bind! does not expand into all built-in forms.  That being
;; said, this file is designed to be able to be compiled.
;;
;;; Code:
(require 'seq)
(require 'oo-base-macros)
(require 'oo-base-lib)

;; The main data structure will be a metadata that I can access.  I will go through
;; it via `use-package' style recursion.
(defvar oo--bind-steps '(oo--bind-step-evil-symbol
                         oo--bind-step-evil-state
                         oo--bind-step-defer-keymap
                         oo--bind-step-which-key
                         oo--bind-step-evil-bind
                         oo--bind-step-bind)
  "List of functions to be called one by one.")

(defun oo--bind-generate-body (metadata steps)
  "Return the body for `bind!'."
  (and steps (funcall (car steps) metadata (cdr steps))))

(defun! oo--bind-step-defer-keymap (metadata steps)
  "Defer the evaluation of body until keymap is loaded.
If METADATA has no keymap return."
  (with-map-keywords! metadata
    (if (or (not !!keymap) (equal !keymap 'global-map) (not (symbolp !keymap)))
        (oo--bind-generate-body metadata steps)
      (set! body (oo--bind-generate-body metadata steps))
      `((oo-call-after-keymap ',!keymap (lambda () ,@body))))))

(defun! oo--bind-step-evil-symbol (metadata steps)
  "Return"
  (set! evil-symbol (map-elt metadata :evil-symbol))
  (nif! evil-symbol
      (oo--bind-generate-body metadata steps)
    (dolist (char (append (symbol-name evil-symbol) nil))
      (if (char-equal char ?g)
          (collecting! forms (oo--bind-generate-body metadata steps))
        (set! state (gensym "state"))
        (set! body (oo--bind-generate-body (map-insert metadata :state state) steps))
        (set! form `(oo-eval-after-evil-state ,char (lambda (,state) ,@body)))
        (collecting! forms form)))))

(defun oo--bind-step-evil-state (metadata steps)
  "Wrap forms with"
  (with-map-keywords! metadata
    (nif! !!state
        (oo--bind-generate-body metadata steps)
      (set! state (gensym "state"))
      (set! metadata (map-insert metadata :state state))
      `((oo-call-after-evil-state ',!state (lambda (,state) ,@(oo--bind-generate-body metadata steps)))))))

;; The function `which-key-add-keymap-based-replacements' already applies kbd to
;; the binding passed in.  This makes it tricky for me to use kbd because I need
;; to make sure it is used after.
(defun! oo--bind-step-which-key (metadata steps)
  (with-map-keywords! metadata
    (set! which-key (or !which-key !wk !desc))
    (set! wk-fn #'which-key-add-keymap-based-replacements)
    (set! fn `(lambda (keymap key def)
                (oo-call-after-load 'which-key ,wk-fn keymap key ,which-key)
                ;; ,@(oo--bind-step-kbd metadata nil)
                (setq key (if (stringp key) (kbd key) key))
                (funcall this-fn keymap key def)))
    (nif! which-key
        (oo--bind-generate-body metadata steps)
      `((lef! ((define-key ,fn))
          ,@(oo--bind-generate-body metadata steps))))))

(defun oo--bind-step-evil-bind (metadata steps)
  "Append form for defining evil binding."
  (with-map-keywords! metadata
    (nif! (and !!state !!keymap !!key !!def)
        (oo--bind-generate-body metadata steps)
      (-snoc (oo--bind-generate-body metadata steps)
             `(evil-define-key* ,!state ,!keymap ,!key ,!def)))))

(defun oo--bind-step-bind (metadata steps)
  ""
  (with-map-keywords! metadata
    (nif! (and !!keymap !!key !!def (not !!state))
        (oo--bind-generate-body metadata steps)
      (-snoc (oo--bind-generate-body metadata steps)
             `(define-key ,!keymap ,!key ,!def)))))

;; (bind! :which-key "foo" :state normal :keymap global-map :key "d" :def #'foo)
;; (bind! :which-key "foo" :state normal :keymap org-mode-map :key "d" :def #'foo)
;; (bind! :which-key "foo" :evil-symbol nmi :keymap global-map :key "d" :def #'foo)
;; (bind! :which-key "foo" :evil-symbol g :keymap global-map :key "d" :def #'foo)

;; I had been considering looping through this, but for now this is fine.
(defun! oo--standardize-args (args)
  "Standardize arguments ARGS for `bind!'."
  (flet! esym (keyword) (intern (seq-rest (symbol-name keyword))))
  (flet! sharp-quoted-p (obj) (equal 'function (car-safe obj)))
  (pcase args
    (`(,(and (pred symbolp) (pred (not keywordp)) keymap) ,key ,(and (pred sharp-quoted-p) def) . ,rest)
     (cl-list* :keymap keymap :key key :def def rest))
    (`(,(and (pred keywordp) evil-keyword) ,key ,(and (pred sharp-quoted-p) def) . ,rest)
     (cl-list* :keymap 'global-map :evil-symbol (esym evil-keyword) :key key
               :def def rest))
    (`(,(and (pred symbolp) keymap) ,(and (pred keywordp) evil-keyword) ,key
       ,(and (pred sharp-quoted-p) def) . ,rest)
     (cl-list* :keymap keymap :evil-symbol (esym evil-keyword) :key key :def def rest))
    (`(,key ,(and (pred sharp-quoted-p) def) . ,rest)
     (cl-list* :key key :def def rest))
    (_ nil)))

(defmacro! bind! (&rest args)
  (macroexp-progn (oo--bind-generate-body (oo--standardize-args args) oo--bind-steps)))
;;; provide
(provide 'oo-base-macros-bind-bang)
;;; oo-base-macros-bind-bang.el ends here
