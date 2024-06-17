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
(require 'seq)
(require 'oo-base-macros)
;; The main data structure will be a metadata that I can access.  I will go through
;; it via `use-package' style recursion.
(defvar oo--bind-steps '(oo--bind-step-defer-keymap
                         oo--bind-step-evil-symbol
                         oo--bind-step-which-key
                         oo--bind-step-evil-bind
                         oo--bind-step-bind)
  "List of functions to be called one by one.")

(defun! oo--bind-step-defer-keymap (metadata steps)
  (with-map-keywords! metadata
    (if (or (not !!keymap) (equal !keymap 'global-map) (not (symbolp !keymap)))
        (oo--bind-generate-body metadata steps)
      (set! body (oo--bind-generate-body metadata steps))
      `((oo-call-after-keymap ',!keymap (lambda () ,@body))))))

(defun! oo--bind-step-evil-symbol (metadata steps)
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
  (with-map-keywords! metadata
    (nif! !!state
        (oo--bind-generate-body metadata steps)
      `((oo-call-after-evil-state ',!state (lambda (_) ,@(oo--bind-generate-body metadata steps)))))))

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
  (with-map-keywords! metadata
    (nif! (and !!state !!keymap !!key !!def)
        (oo--bind-generate-body metadata steps)
      (-snoc (oo--bind-generate-body metadata steps)
             `(evil-define-key* ',!state ,!keymap ,!key ,!def)))))

(defun oo--bind-step-bind (metadata steps)
  (with-map-keywords! metadata
    (nif! (and !!keymap !!key !!def (not !!state))
        (oo--bind-generate-body metadata steps)
      (-snoc (oo--bind-generate-body metadata steps)
             `(define-key ,!keymap ,!key ,!def)))))

;; (defun oo--bind-step-do-binding (metadata steps)
;;   (with-map-keywords! metadata
;;     (nif! (and !!definer !!definer-args)
;;         (oo--bind-generate-body metadata steps)
;;       (-snoc (oo--bind-generate-body metadata steps)
;;              `(funcall #',!definer ,@!definer-args)))))

(defun oo--bind-generate-body (metadata steps)
  "Return binding form."
  (and steps (funcall (car steps) metadata (cdr steps))))

(bind! :which-key "foo" :evil-state normal :keymap global-map :key "d" :def #'foo)
(bind! :which-key "foo" :state normal :keymap org-mode-map :key "d" :def #'foo)
(bind! :which-key "foo" :evil-symbol nmi :keymap global-map :key "d" :def #'foo)
(bind! :which-key "foo" :evil-symbol g :keymap global-map :key "d" :def #'foo)
;; (oo--bind-generate-body '(:keymap global-map :key "d" :def #'foo) '(oo--bind-step-bind))
;; ((define-key global-map "d" #'foo))
;; (oo--bind-step-bind '(:keymap global-map :key "d" :def #'foo) nil)
(setq oo--bind-steps '(oo--bind-step-evil-symbol
                       oo--bind-step-evil-state
                       oo--bind-step-defer-keymap
                       oo--bind-step-which-key
                       oo--bind-step-evil-bind
                       oo--bind-step-bind))

(defmacro bind! (&rest args)
  (macroexp-progn (oo--bind-generate-body args oo--bind-steps)))

(defmacro! bind!! (&rest args)
  "Bind KEY in KEYMAP to DEFINITION.
This is the same as `define-key'.  If KEYMAP is not specified use `global-map'.
If STATES is specified use evil binding.  If KEYMAP is a symbol."
  (flet! plist-p (x) (or (null x) (keywordp (car x))))
  ;; States is a list of symbols
  (flet! states-p (x) (or (symbolp x) (and (listp x) (-all-p #'symbolp x))))
  (flet! map-symbol-p (x) (and (symbolp x)
                               (not (keywordp x))
                               (string-match-p "-map\\'" (symbol-name x))))
  (flet! keymap-p (-orfn #'keymapp #'map-symbol-p))
  (flet! key-p (-orfn #'stringp #'vectorp))
  (pcase args
    ;; Just passing in key and def, implies `global-map'.
    (`(:alt ,key ,def . ,(and (pred plist-p) metadata))
     (list :alt key def)
     (apply #'oo--bind :alt t :key key :def def metadata))
    (`(,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     `(bind! :keymap global-map :key )
     (apply #'oo--bind :keymap global-map :key key :def def metadata))
    (`(,(and (pred keymap-p) keymap) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :keymap keymap :key key :def def metadata))
    (`(,(and (pred keymap-p) keymap) ,(and (pred keywordp) state-key) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :state-key state-key :keymap keymap :key key :def def metadata))
    (`(,(and (pred keywordp) state-key) ,(and (pred keymap-p) keymap) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :state-key state-key :keymap keymap :key key :def def metadata))
    (`(,(and (pred keywordp) state-key) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :state-key state-key :keymap 'global-map :key key :def def metadata))
    (`(,(and (pred states-p) states) ,(and (pred keymap-p) keymap) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :states states :keymap keymap :key key :def def metadata))
    (`(,(and (pred states-p) states) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :states states :keymap 'global-map :key key :def def metadata))
    (_
     (error "unknown binding syntax"))))
;;; provide
(provide 'oo-base-macros-bind-bang)
;;; oo-base-macros-bind-bang.el ends here
