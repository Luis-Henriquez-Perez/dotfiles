;;; oo-base-macros-bind-bang.el -*- lexical-binding: t; -*-
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
;; I took effort to allow bind to accept arbitrary forms that it would then eval
;; to produce its argument.  So for example you could use (concat "d" "e") as a
;; key and it would work.  I did not consider though that these forms actually
;; evaluate one time per binding instead of just one time in general which is
;; probably not good.  Fixing this is actually not immediately obvious because
;; the forms are not necessarily evaluated at the same time.  In fact it is
;; possible for every form to be evaluated at a different time.  I would need to
;; make sure that the arguments from evaluating one form.
;;
;; You need to make sure may conditions are met before binding keys in Emacs.
;; For one thing, if the keymap you specify does not exist you will get a void
;; symbol error.  For example, I need to make sure org mode is loaded before I
;; use (define-key org-mode-map "d" #'foo).  This is only augmented by using
;; evil-mode: if you use `evil-define-key*' before evil is loaded you will get a
;; void function error.  Instead, you need to do (eval-after-load 'evil
;; (evil-define-key* ...)).
;; Having to consideras well as is possible but tedious and resulting in verbose
;; forms.
;;
;; With this macro I hope to shorten and homogenize the syntax for keybindings
;; and to automate boilerplate used in tandem with keybindings.
;;
;; I do not know.  I was trying to abstract the actual binding of the key with
;; keys `:bind-fn' and `:bind-args' but now I do not think it is right.  I
;; should try to keep the abstraction as simple as possible.  Even as I write
;; this I look at it with disgust and I want to do it differently but I am
;; trying to resist that urge because I know.  Still, I do not feel like my
;; abstraction was the best.  I think that sometimes you have to separate
;; idealism and practicality.  Right now my idealistic inclination is to change
;; the abstraction or do something differently but the problem is my code
;; as bad as it is already works for the most part and changing the abstraction
;; would almost certainly compromise it is correctness.  At this point in time
;; rather, I need to be in just get it done mode where I do not focus on the
;; code making sense in terms of the overarching abstraction but just on getting
;; it to work.
;;
;;; Code:
(require 'seq)
(require 'oo-base-utils)
(require 'oo-base-macros-with-map-bang)
(require 'oo-base-macros-definers)

(defvar oo--bind-steps '(oo--bind-step-localleader
                         oo--bind-step-evil
                         oo--bind-step-defer-keymap
                         oo--bind-step-which-key
                         oo--bind-step-let-bind
                         oo--bind-step-alt
                         oo--bind-step-prefix
                         oo--bind-step-kbd
                         oo--bind-step-bind)
  "List of functions to be called in order to generate the main body.")

(defun oo--bind-generate-body (metadata steps)
  "Return the body for `bind!'.
Call the first step function with METADATA and the remaining step functions.  If
STEPS is nil, do nothing and return nil."
  (and steps (funcall (car steps) metadata (cdr steps))))

;; One of my biggest problems when writing this feature was
;; propogating variables across nested lambdas.  After thinking about it for a
;; while there were two was I could think of doing it.  The first was passing in
;; the arguments to a lambda.  The second is injecting the bindings into the
;; lambda itself.  I choose to use the latter because I could not see how to
;; pass in function arguments with symbols.
(defun! oo--lambda-form (symbols args body)
  "Return a form that evaluates into a lambda with ARGS and BODY.
In the lambda SYMBOLS are let-bound to their values around BODY."
  (set! symbols (ensure-list symbols))
  (nif! symbols
      `(lambda ,args ,@body)
    (set! let-binds `(cl-mapcar #'list ',symbols (mapcar #'macroexp-quote (list ,@symbols))))
    `(list 'lambda ',args (append (list 'let ,let-binds) ',body))))

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
      `((oo-call-after-bound ',!keymap ,(oo--bind-lambda nil metadata steps))))))

(defun! oo--bind-step-evil (metadata steps)
  "Register keybinding as evil binding."
  (with-map-keywords! metadata
    (if (not (and !!keymap !!key !!def (or !evil-state !evil-symbol)))
        (oo--bind-generate-body metadata steps)
      (prepending! steps '(oo--bind-step-evil-state oo--bind-step-evil-symbol))
      (oo--bind-generate-body metadata steps))))

(defun! oo--bind-step-evil-state (metadata steps)
  (set! state (map-elt metadata :evil-state))
  (if (or (not state) (equal 'global state))
      (oo--bind-generate-body metadata steps)
    (pushing! steps #'oo--bind-step-evil-signature)
    (setf (map-elt metadata :evil-state) (oo-ensure-quote state))
    `((oo-call-after-load 'evil (lambda () ,@(oo--bind-generate-body metadata steps))))))

(defun! oo--bind-step-evil-symbol (metadata steps)
  (set! evil-symbol (map-elt metadata :evil-symbol))
  (nif! evil-symbol
      (oo--bind-generate-body metadata steps)
    (dolist (char (append (symbol-name evil-symbol) nil))
      (setf (map-elt metadata :evil-char) char)
      (appending! forms (oo--bind-step-evil-char metadata steps)))
    forms))

(defun! oo--bind-step-evil-char (metadata steps)
  (set! char (map-elt metadata :evil-char))
  (if (or (not char) (= char ?g))
      (oo--bind-generate-body metadata steps)
    (pushing! steps #'oo--bind-step-evil-signature)
    (set! evil-state (gensym "state"))
                 (setq metadata (map-insert metadata :evil-state evil-state))
                 (set! lambda (oo--bind-lambda (list evil-state) metadata steps))
    `((oo-call-after-evil-state-char ,char ,lambda))))

(defun! oo--bind-step-evil-signature (metadata steps)
  (set! mode (map-elt metadata :mode))
  (set! state (map-elt metadata :evil-state))
  (cond (mode
         (setq metadata (map-insert metadata :bind-fn #'evil-define-minor-mode-key))
         (setq metadata (map-insert metadata :bind-args '(:evil-state :mode :key :def))))
        (state
         (setq metadata (map-insert metadata :bind-fn #'evil-define-key*))
         (setq metadata (map-insert metadata :bind-args '(:evil-state :keymap :key :def)))))
  (oo--bind-generate-body metadata steps))

;; The function `which-key-add-keymap-based-replacements' already applies kbd to
;; the binding passed in.  This makes it tricky for me to use kbd because I need
;; to make sure it is used after.
(defun! oo--bind-step-which-key (metadata steps)
  (with-map-keywords! metadata
    (set! which-key (or !which-key !wk !desc))
    (set! wk-fn #'which-key-add-keymap-based-replacements)
    (set! fn `(lambda (keymap key def)
                (oo-call-after-load 'which-key (apply-partially #',wk-fn keymap key ,which-key))
                (setq key (if (stringp key) (kbd key) key))
                (funcall this-fn keymap key def)))
    (nif! which-key
        (oo--bind-generate-body metadata steps)
      (setq steps (remove #'oo--bind-step-kbd steps))
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
    ;; Using `setf' instead of the `setq' and `map-insert' idiom here fails.
    (setq metadata (map-insert metadata key symbol)))
  `((let ,let-binds ,@(oo--bind-generate-body metadata steps))))

(defun oo--bind-step-kbd (metadata steps)
  "Apply kbd to binding if possible."
  (with-map-keywords! metadata
    (cons `(setq ,!key (if (stringp ,!key) (kbd ,!key) ,!key))
          (oo--bind-generate-body metadata steps))))

(defun! oo--bind-step-alt (metadata steps)
  (nif! (map-elt metadata :alt)
      (oo--bind-generate-body metadata steps)
    (set! orig (gensym "original"))
    (set! alt (gensym "alt"))
    (set! key (map-elt metadata :key))
    (set! def (map-elt metadata :def))
    (set! steps (remove #'oo--bind-step-kbd steps))
    (set! feature (map-elt metadata :feature))
    (set! condition (map-elt metadata :condition))
    (unless condition
      (if feature
          (set! condition `(or (featurep ',feature) (require ',feature nil t)))
        (set! condition t)))
    `((let ((,orig ,key)
            (,alt ,def))
        (setq ,key (vconcat (list 'remap ,orig)))
        (setq ,def (list 'menu-item "" ,orig :filter #'oo-alternate-command-choose-fn))
        (push ,(oo--lambda-form (list alt) '(&rest _) (list (list 'when condition alt))) (gethash ,orig oo-alternate-commands))
        ,@(oo--bind-generate-body metadata steps)))))

(defun! oo--bind-step-prefix (metadata steps)
  (with-map-keywords! metadata
    (nif! !prefix
        (oo--bind-generate-body metadata steps)
      (cons `(setq ,!key (if (stringp ,!key) (concat ,!prefix "\s" ,!key) ,!key))
            (oo--bind-generate-body metadata steps)))))

(defun! oo--bind-step-localleader (metadata steps)
  (set! alist '((oo-normal-localleader-short-key . normal)
                (oo-normal-localleader-key       . normal)
                (oo-insert-localleader-key       . insert)
                (oo-insert-localleader-short-key . insert)
                (oo-emacs-localleader-key        . emacs)
                (oo-emacs-localleader-key        . global)))
  (nif! (map-elt metadata :localleader)
      (oo--bind-generate-body metadata steps)
    (for! ((leader . state) alist)
      (alet (map-insert (map-insert metadata :evil-state state) :prefix leader)
        (appending! forms (oo--bind-generate-body it steps))))
    forms))

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
  (pcase args
    (`(,(and (pred symbolp) (pred (not keywordp)) keymap) ,key ,(and (pred oo-sharpquoted-p) def) . ,rest)
     (cl-list* :keymap keymap :key key :def def rest))
    (`(,(and (pred keywordp) evil-keyword) ,key ,(and (pred oo-sharpquoted-p) def) . ,rest)
     (cl-list* :keymap 'global-map :evil-symbol (esym evil-keyword) :key key
               :def def rest))
    (`(,(and (pred symbolp) keymap) ,(and (pred keywordp) evil-keyword) ,key
       ,(and (pred oo-sharpquoted-p) def) . ,rest)
     (cl-list* :keymap keymap :evil-symbol (esym evil-keyword) :key key :def def rest))
    (`(,(and (pred keywordp) evil-keyword) ,(and (pred symbolp) keymap) ,key
       ,(and (pred oo-sharpquoted-p) def) . ,rest)
     (cl-list* :keymap keymap :evil-symbol (esym evil-keyword) :key key :def def rest))
    (`(,key ,(and (pred oo-sharpquoted-p) def) . ,rest)
     (cl-list* :key key :def def rest))
    (`(:alt ,orig ,alt . ,rest)
     (cl-list* :alt t :key (macroexp-quote orig) :def (macroexp-quote alt) rest))
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
