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
;;
;;
;;; Code:
(require 'seq)
(require 'oo-base-macros)
(require 'oo-base-lib)

(defvar oo--bind-steps '(oo--bind-step-evil
                         oo--bind-step-defer-keymap
                         oo--bind-step-which-key
                         oo--bind-step-let-bind
                         oo--bind-step-kbd
                         oo--bind-step-bind)
  "List of functions to be called in order to generate the main body.")

(defun oo--bind-generate-body (metadata steps)
  "Return the body for `bind!'.
Call the first step function with METADATA and the remaining step functions.  If
STEPS is nil, do nothing and return nil."
  (and steps (funcall (car steps) metadata (cdr steps))))

;; One of my biggest problems when writing this feature is
;; propogating variables across nested lambdas.  After thinking about it for a
;; while there were two was I could think of doing it.  The first was passing in
;; the arguments to a lambda.  The second is injecting the bindings into the
;; lambda itself.  I choose to use the latter because I could not see how to
;; pass in function arguments with symbols.
(defun oo--lambda-form (symbols args body)
  "Return a form that evaluates into a lambda with ARGS and BODY.
In the lambda SYMBOLS are let-bound to their values around BODY."
  (aif (ensure-list symbols)
      `(list 'lambda ,args (append (list 'let (cl-mapcar #'list ',it (list ,@it))) ',body))
    `(lambda ,args ,@body)))

(defun! oo--bind-lambda (args metadata steps)
  "Return a form that evaluates into a lambda.
The body of the lambda is the result of.  Any keys in `:env' in METADATA are
letbound to their values around the BODY of the lambda."
  (set! env (map-elt metadata :env))
  (dolist (key env)
    (collecting! symbols (map-elt metadata key)))
  (oo--lambda-form symbols args (oo--bind-generate-body metadata steps)))

(defun! oo--bind-step-defer-keymap (metadata steps)
  "Defer the evaluation of body until keymap is loaded.
If METADATA has no keymap return."
  (with-map-keywords! metadata
    (if (or (not !!keymap) (equal !keymap 'global-map) (not (symbolp !keymap)))
        (oo--bind-generate-body metadata steps)
      `((oo-call-after-keymap ',!keymap ,(oo--bind-lambda nil metadata steps))))))

(defun! oo--bind-step-evil (metadata steps)
  "Register keybinding as evil binding."
  (with-map-keywords! metadata
    (cond ((not (and !!keymap !!key !!def (or !evil-state !evil-symbol)))
           (oo--bind-generate-body metadata steps))
          ((prog1 nil
             (setf (map-elt metadata :bind-fn) #'evil-define-key*)
             (cl-pushnew :evil-state (map-elt metadata :bind-args))))
          (!!evil-state
           (set! evil-state (gensym "evil-state"))
           (setf (map-elt metadata :evil-state) evil-state)
           (cl-pushnew :evil-state (map-elt metadata :env))
           `((oo-call-after-evil-state ,!evil-state ,(oo--bind-lambda (list evil-state) metadata steps))))
          (!evil-symbol
           (set! evil-state (gensym "evil-state"))
           (dolist (char (append (symbol-name !evil-symbol) nil))
             (if (char-equal char ?g)
                 (prepending! forms (oo--bind-generate-body metadata steps))
               (let ((metadata metadata))
                 (set! evil-state (gensym "evil-state"))
                 (setf (map-elt metadata :evil-state) evil-state)
                 (cl-pushnew :evil-state (map-elt metadata :env))
                 (set! lambda (oo--bind-lambda (list evil-state) metadata steps))
                 (set! form `(oo-eval-after-evil-state ,char ,lambda))
                 (collecting! forms form))))
           forms))))

;; The function `which-key-add-keymap-based-replacements' already applies kbd to
;; the binding passed in.  This makes it tricky for me to use kbd because I need
;; to make sure it is used after.
(defun! oo--bind-step-which-key (metadata steps)
  (with-map-keywords! metadata
    (set! which-key (or !which-key !wk !desc))
    (set! wk-fn #'which-key-add-keymap-based-replacements)
    (set! fn `(lambda (keymap key def)
                (oo-call-after-load 'which-key ,wk-fn keymap key ,which-key)
                (setq key (if (stringp key) (kbd key) key))
                (funcall this-fn keymap key def)))
    (nif! which-key
        (oo--bind-generate-body metadata steps)
      `((lef! ((define-key ,fn))
          ,@(oo--bind-generate-body metadata steps))))))

;; This is so other functions not just the binding functions can use the
;; variables instead of.
(defun! oo--bind-step-let-bind (metadata steps)
  "Let bind symbols around body as specified by `:bind-args'."
  (for! (key (map-elt metadata :bind-args))
    (set! value (map-elt metadata key))
    (set! symbol (gensym (seq-rest (symbol-name key))))
    (collecting! let-binds (list symbol value))
    (set! new-metadata metadata)
    (setf (map-elt new-metadata key) symbol))
  `((let ,let-binds ,@(oo--bind-generate-body new-metadata steps))))

(defun oo--bind-step-kbd (metadata steps)
  "Apply kbd to binding if possible."
  (with-map-keywords! metadata
    (cons `(setq ,!key (if (stringp ,!key) (kbd ,!key) ,!key))
          (oo--bind-generate-body metadata steps))))

(defun oo--bind-step-bind (metadata steps)
  "Call the value of `:bind-fn' with value of `:bind-args'."
  (with-map-keywords! metadata
    (nif! (and !!bind-fn !!bind-args)
        (oo--bind-generate-body metadata steps)
      (set! args (mapcar (apply-partially #'map-elt metadata) !bind-args))
      (cons `(,!bind-fn ,@args)
            (oo--bind-generate-body metadata steps)))))

;; I had been considering looping through the bind arguments to tokenize them
;; which is probably less verbose than doing it statically like this, but it is
;; also a bit more involved to code.
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
    ;; (`(:alt ,key ,alt . ,rest)
    ;;  ())
    (_ nil)))

(defmacro! bind! (&rest args)
  "Bind keys as specified by ARGS."
  (set! metadata (oo--standardize-args args))
  (setf (map-elt metadata :bind-fn) #'define-key)
  (setf (map-elt metadata :bind-args) (list :keymap :key :def))
  (unless (map-elt metadata :keymap)
    (setf (map-elt metadata :keymap) 'global-map))
  (macroexp-progn (oo--bind-generate-body metadata oo--bind-steps)))
;;; provide
(provide 'oo-base-macros-bind-bang)
;;; oo-base-macros-bind-bang.el ends here
