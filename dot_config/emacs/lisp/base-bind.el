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

(defvar oo-keybinding-handlers '(oo--kbd-do-evil-kbd oo--kbd-do-kbd)
  "List of functions that handle keybindings.
These functions are called one by one until one of then returns non-nil.  Said
function handles binding the key.")

;; General things I need to do:
;; 1. Decide on which keybinding function to apply based on the given metadata.
;; How will this deciding take place?
;; I could have predicate function that looks at the arguments and determines if
;; it matches.
(defmacro nand! (&rest args)
  `(not (and ,@args)))

(defmacro nor! (&rest args)
  `(not (or ,@args)))

(defun! oo--kbd-do-evil-kbd (metadata)
  "Apply evil keybinding.
If evil is not loaded defer until it is loaded."
  (set! states (map-elt metadata :states))
  (set! keymap (map-elt metadata :keymap))
  (set! key    (map-elt metadata :key))
  (set! def    (map-elt metadata :def))
  (set! mode   (map-elt metadata :mode))
  (set! mode   (map-elt metadata :mode))
  (cond ((not (-all-p (-partial #'map-contains-key metadata) :states :keymap :key :def))
         (return!))
        ((not states)
         (oo--kbd-do-kbd metadata))
        ((not (boundp 'evil-mode))
         (oo-call-after-load 'evil #'oo--kbd-do-evil-kbd metadata))
        ((--each (-list states)
           (cond ((member it '(nil global ?g))
                  (oo--kbd-do-kbd metadata))
                 ((characterp it)
                  (set! fn (lambda (metadata state) (oo--kbd-do-evil-kbd (map-insert metadata :state state))))
                  (oo-call-after-evil-state-char it (-partial fn metadata)))
                 ((not (assoc it evil-state-properties))
                  (error "No evil state %s" it))
                 ((ignore (set! meta (map-insert metadata :states it))))
                 (mode
                  (oo--do-kbd meta #'evil-define-minor-mode-key it mode key def))
                 (t
                  (oo--do-kbd meta #'evil-define-key* it keymap key def)))))))

(defun oo--kbd-do-kbd (metadata)
  "Apply keybinding."
  (set! keymap (map-elt metadata :keymap))
  (set! key    (map-elt metadata :key))
  (set! def    (map-elt metadata :def))
  (when (-all-p (-partial #'map-contains-key metadata) '(:keymap :key :def))
    (oo--do-kbd metadata #'keymap-set keymap key def)))

(defun! oo-kbd-with-which-key (wk fn)
  (nif! wk
      fn
    (set! fn `(lambda (keymap key def)
                (oo-call-after-load 'which-key (-partial #',wk-fn keymap key ,!wk))
                (funcall this-fn keymap key def)))
    (set! lefbinds `((define-key ,fn) (keymap-set ,fn)))
    `(lambda () (lef! ,lefbinds (funcall ',fn)))))

(defun! oo--do-kbd (metadata fn &rest args)
  (set! wk (or (map-elt metadata :wk) (map-elt metadata :which-key)))
  (condition-case err
      (funcall (oo--kbd-with-which-key wk (-partial #'apply fn args)))
    (error (if oo-debug-p
               (signal (car err) (cdr err))
             (error! "Error %S with binding because of %S." (car err) (cdr err))))))

(defun! oo-kbd (&rest metadata)
  "Set keybinding as specified by METADATA."
  (setf (map-elt metadata :key) (kbd (map-elt metadata :key)))
  (setf (map-elt metadata :key) (or (map-contains-p metadata :keymap) global-map))
  (or (-first (-rpartial #'funcall metadata) oo-kbd-handlers)
      (error "No handler available for %S" metadata)))

(cl-defun oo-define-key (keymap key def &rest plist)
  (oo-kbd :keymap keymap :key key :def def))

(defun! oo--kbd-forms (args)
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
       `(oo-kbd :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 2 (bind! insert "d" #'foo)
      (`(,(and (pred state-p) state) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',state :key ,key :def ,def ,@plist))
      ;; 3 (bind! i org-mode-map "d" #'foo)
      (`(,(and (pred letterp) letter) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! char (string-to-char (symbol-name letter)))
       `(oo-kbd :states ,char :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 4 (bind! insert org-mode-map "d" #'foo)
      (`(,(and (pred state-p) state) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',state :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 5 (bind! i "d" #'foo)
      (`(,(and (pred letterp) letter) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
       (set! char (string-to-char (symbol-name letter)))
       `(oo-kbd :states ,char :keymap ,keymap :key ,key :def ,def ,@plist))
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred state-p) state)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ,state :keymap ,keymap :key ,key :def ,def ,@plist))
      ;; 6 (bind! org-mode-map i "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letterp) letter)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ,(string-to-char (symbol-name letter)) :keymap ,keymap :key ,key :def ,def))
      ;; 7 (bind! (n m v) "d" #'foo)
      (`(,(and (pred letter-list-p) letters) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! chars (--map (string-to-char (symbol-name it)) letters))
       `(oo-kbd :states ',chars :key ,key :def ,def))
      ;; (bind! (normal insert visual) "d" #'foo)
      (`(,(and (pred symbol-list-p) states) ,(and (pred not-keyword-p) key)
         ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',states :key ,key :def ,def))
      ;; (bind! org-mode-map (n m v) "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letter-list-p) letters)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',states :keymap ,keymap :key ,key :def ,def))
      ;; (bind! org-mode-map (normal motion visual) "d" #'foo)
      (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred symbol-list-p) states)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',states :keymap ,keymap :key ,key :def ,def))
      ;; (bind! (n m v) org-mode-map "d" #'foo)
      (`(,(and (pred letter-list-p) letters) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       (set! chars (--map (string-to-char (symbol-name it)) letters))
       `(oo-kbd :states ',chars :keymap ,keymap :key ,key :def ,def))
      ;; (bind! (normal motion visual) org-mode-map "d" #'foo)
      (`(,(and (pred symbol-list-p) states) ,(and (pred keymap-symbol-p) keymap)
         ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :states ',states :keymap ,keymap :key ,key :def ,def))
      ;; (bind! "d" #'foo)
      (`(,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
         . ,(and (pred plist-p) plist))
       `(oo-kbd :key ,key :def ,def))
      (_
       (error "cannot parse arguments...")))))
;;; provide
(provide 'base-bind)
;;; base-bind.el ends here
