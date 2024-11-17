;;; base-bind.el --- keybinding utilities -*- lexical-binding: t; -*-
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
;; This file provides a keybinding function: `oo-bind'.
;;
;; This function provides a homogeneous syntax for binding keys uniformly that
;; abstracts away several specific keybinding functions I would normally use
;; such as `define-key', `evil-define-key*' and `evil-define-minor-mode-key'.
;; Additionally, it handles several details.  For example, it properly
;; defers any binding if the keymap symbol provided for said binding is not yet
;; bound or if said binding evil keybinding and evil is not loaded.  Ultimately,
;; the point is to facilitate writing code that is concise and declarative
;; as opposed to code that is nested in `with-eval-after-load' forms.
;;
;;; Code:
(require 'base)

(defun! oo--kbd-parse-args (args)
  (lef! ((letter-to-char (-compose #'string-to-char #'symbol-name))
         (letterp (obj) (and (symbolp obj) (= 1 (length (symbol-name obj)))))
         (keymap-symbol-p (obj) (and (symbolp obj) (string-match-p "[^[:space:]]+-map\\'" (symbol-name obj))))
         (state-p (state) (and (symbolp state) (not (keywordp state)) (not (keymap-symbol-p state)) (not (letterp state))))
         (letter-list-p (obj) (and (listp obj) (-all-p #'letterp obj)))
         (symbol-list-p (obj) (and (listp obj) (-all-p #'symbolp obj)))
         (non-keyword-symbol-p (obj) (and (symbolp obj) (not (keywordp obj))))
         (not-keyword-p (-not #'keywordp))
         (plist-p (map) (or (null map) (and (listp map) (symbolp (car map))))))
    (pcase args
      ;; 1 (bind! global-map "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
       `(:states '(global) :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 2 (bind! insert "d" #'foo)
      (`(,(and (pred state-p) state) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
       `(:states ',state :keymap global-map :key ,key :def ,def ,@plist))
      ;; 3 (bind! i org-mode-map "d" #'foo)
      (`(,(and (pred letterp) letter) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! char (string-to-char (symbol-name letter)))
       `(:states ,char :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 4 (bind! insert org-mode-map "d" #'foo)
      (`(,(and (pred state-p) state) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(:states ',state :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 5 (bind! i "d" #'foo)
      (`(,(and (pred letterp) letter) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
       (set! char (string-to-char (symbol-name letter)))
       `(:states ,char :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 6
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred state-p) state)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(:states ,state :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 7 (bind! org-mode-map i "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letterp) letter)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! char (string-to-char (symbol-name letter)))
       `(:states ,char :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 8 (bind! (n m v) "d" #'foo)
      (`(,(and (pred letter-list-p) letters) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! chars (--map (string-to-char (symbol-name it)) letters))
       `(:states ',chars :keymap global-map :key ,key :def ,def ,@plist))
      ;; 9 (bind! (normal insert visual) "d" #'foo)
      (`(,(and (pred symbol-list-p) states) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',states :key ,key :def ,def ,@plist))
      ;; 10 (bind! org-mode-map (n m v) "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letter-list-p) letters)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! chars (--map (string-to-char (symbol-name it)) letters))
       `(:states ',chars :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 11 (bind! org-mode-map (normal motion visual) "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred symbol-list-p) states)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(:states ',states :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 12 (bind! (n m v) org-mode-map "d" #'foo)
      (`(,(and (pred letter-list-p) letters) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! chars (--map (string-to-char (symbol-name it)) letters))
       `(:states ',chars :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 13 (bind! (normal motion visual) org-mode-map "d" #'foo)
      (`(,(and (pred symbol-list-p) states) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(:states ',states :keymap ,keymap :key ,key :def ,def))
      ;; 14 (bind! "d" #'foo)
      (`(,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(:states nil :keymap global-map :key ,key :def ,def))
      (_
       (error "cannot parse arguments...")))))

;; General things I need to do:
;; 1. Decide on which keybinding function to apply based on the given metadata.
;; How will this deciding take place?
;; I could have predicate function that looks at the arguments and determines if
;; it matches.
(defmacro nand! (&rest args)
  `(not (and ,@args)))

(defmacro nor! (&rest args)
  `(not (or ,@args)))

;; (defer-keymap defer-evil bind)

(defun! oo--kbd-key-binding-forms (meta)
  (set! states (map-elt meta :states))
  (set! keymap (map-elt meta :keymap))
  (set! key    (map-elt meta :key))
  (set! def    (map-elt meta :def))
  (set! mode   (map-elt meta :mode))
  (set! states (-list (or states 'global)))
  (dolist (state states)
    (cond ((member state '(nil global ?g))
           '(defer-keymap check-error which-key keymap-set)
           ;; (oo--defer-keymap-forms meta (oo-- (oo--keymap-set-forms meta _)))
           (appending! forms ()))
          ((characterp state)
           (set! fn (lambda (meta state) (oo--kbd-do-evil-kbd (map-insert meta :state state))))
           (appending! forms (oo--bind-defer-evil-state ( fn meta))))
          (mode
           (appending! forms (oo--kbd-form meta #'evil-define-minor-mode-key state mode key def)))
          (t
           (appending! forms (oo--kbd-form meta #'evil-define-key* state keymap key def)))))
  forms)

(defun oo--keymap-set-forms (meta _)
  (set! keymap (map-elt meta :keymap))
  (set! key    (map-elt meta :key))
  (set! def    (map-elt meta :def))
  `((keymap-set ,keymap ,key ,def)))

(defun oo--evil-define-key-forms (meta _)
  (set! states (map-elt meta :states))
  (set! keymap (map-elt meta :keymap))
  (set! key    (map-elt meta :key))
  (set! def    (map-elt meta :def))
  `((evil-define-key* ,states ,keymap ,key ,def)))

(defun oo--evil-define-minor-mode-key-forms (meta _)
  (set! states (map-elt meta :states))
  (set! keymap (map-elt meta :keymap))
  (set! key    (map-elt meta :key))
  (set! def    (map-elt meta :def))
  `((evil-define-minor-mode-key ,states ,mode ,key ,def)))

(defun oo--let-bind ()
  ""
  )

(defun! oo--kbd-defer-keymap-forms (meta forms)
  (set! keymap (map-elt meta :keymap))
  `((if (boundp ',keymap)
        (progn ,@forms)
      (defvar ,keymap)
      (oo-call-after-bound ',keymap (lambda () ,@forms)))))

(defun! oo-kbd-with-which-key (meta forms)
  (set! wk    (map-elt meta :wk))
  (set! lefbinds `((define-key ,fn) (keymap-set ,fn)))
  `((lef! ((define-key)
           (keymap-set))
      ,@forms)))

(defun! oo--check-error-forms (meta forms)
  `((condition-case err
        (progn ,@forms)
      (error (if oo-debug-p
                 (signal (car err) (cdr err))
               (error! "Error %S with binding because of %S." (car err) (cdr err)))))))

(defun! oo-kbd (&rest metadata)
  "Set keybinding as specified by METADATA."
  (setf (map-elt metadata :key) (kbd (map-elt metadata :key)))
  (setf (map-elt metadata :key) (or (map-contains-p metadata :keymap) global-map))
  (oo--kbd-perform-binding metadata))

(defmacro bind! (&rest args)
  (oo--kbd-keybinding-form (oo--kbd-parse-args args)))
;;; provide
(provide 'base-bind)
;;; base-bind.el ends here
