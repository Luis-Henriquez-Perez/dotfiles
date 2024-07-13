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
;; TODO: add commentary
;;
;;; Code:
(require 'oo-base)
;;;; build steps functions
;; These are functions that produce forms.
(defun oo--build-define-key (metadata forms)
  (with-map-keywords! metadata
    `((define-key ,!keymap ,!key ,!def)
      ,@forms)))

(defun oo--build-evil-define-key (metadata forms)
  (with-map-keywords! metadata
    `((evil-define-key* ,!state ,!keymap ,!key ,!def)
      ,@forms)))

(defun oo--build-evil-define-minor-mode-key (metadata forms)
  (with-map-keywords! metadata
    `((evil-define-key* ,!state ,!mode ,!key ,!def)
      ,@forms)))

(defun oo--build-kbd (metadata forms)
  "Apply kbd to binding if possible."
  (with-map-keywords! metadata
    `((setq ,!key (if (stringp ,!key) (kbd ,!key) ,!key))
      ,@forms)))

(defun! oo--build-which-key (metadata forms)
  (with-map-keywords! metadata
    (set! wk-fn #'which-key-add-keymap-based-replacements)
    (set! fn `(lambda (keymap key def)
                (oo-call-after-load 'which-key (apply-partially #',wk-fn keymap key ,!wk))
                (setq key (if (stringp key) (kbd key) key))
                (funcall this-fn keymap key def)))
    `((lef! ((define-key ,fn)) ,@forms))))
;;;; generate body
(defun! oo--bind-generate-forms (metadata)
  (--reduce (funcall it acc metadata) (oo--bind-build-steps metadata)))
;;;; process arguments
(defun! oo--build-metadata (args)
  "Return standardized metadata from arguments."
  (flet! letterp (obj)
    (and (symbolp obj) (= 1 (length (symbol-name obj)))))
  (flet! map-symbol-p (obj)
    (and (symbolp obj) (string-match-p "[^[:space:]]+-map\\'" (symbol-name obj))))
  (flet! symbol-list (obj)
    (and (listp obj) (-all-p #'symbolp obj)))
  (flet! letter-list (obj)
    (and (listp obj) (-all-p #'letterp obj)))
  (flet! notkeyp (-not #'keywordp))
  (pcase args
    ;; (bind! i "d" #'foo)
    (`(,(and (pred letterp) letter) ,(and (pred notkeyp) key)
       ,(and (pred notkeyp) def) . ,plist)
     `(:state ,letter :keymap global-map :key ,key :def ,def))
    ;; (bind! i org-mode-map "d" #'foo)
    (`(,(and (pred letterp) letter) ,(and (pred keymap-symbol-p) keymap)
       ,(and (pred notkeyp) key) ,(and (pred notkeyp) def) . ,plist)
     `(:state ,letter :keymap ,keymap :key ,key :def ,def))
    ;; (bind! org-mode-map i "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letterp) letter)
       ,(and (pred notkeyp) key) ,(and (pred notkeyp) def) . ,plist)
     `(:state ,letter :keymap ,keymap :key ,key :def ,def))
    ;; (bind! (n m v) "d" #'foo)
    (`(,(and (pred letter-list-p) letter-list) ,(and (pred notkeyp) key)
       ,(and (pred notkeyp) def) . ,plist)
     `(:evil-states ,letter-list :keymap global-map :key ,key :def ,def . ,plist))
    ;; (bind! org-mode-map (n m v) "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letter-list-p) letter-list)
       ,(and (pred notkeyp) key) ,(and (pred notkeyp) def) . ,plist)
     `(:evil-states ,letter-list :keymap global-map :key ,key :def ,def . ,plist))
    ;; (bind! (n m v) org-mode-map "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letter-list-p) letter-list)
       ,(and (pred notkeyp) key) ,(and (pred notkeyp) def) . ,plist)
     `(:evil-states ,letter-list :keymap global-map :key ,key :def ,def . ,plist))
    ;; (bind!)
    (`(,letter-list ,(and (pred notkeyp) key) ,(and (pred notkeyp) def) . ,plist)
     `(:evil-states ,letter-list :key ,key :def ,def . ,plist))
    ;; (bind! "d" #'foo)
    (`(,(and (pred notkeyp) key) ,(and (pred notkeyp) def) . ,plist)
     `(:keymap global-map :key ,key :def ,def . ,plist))
    (_
     (error "cannot parse arguments..."))))
;;;; bind steps
;; This contains the complex logic of which steps should happen when.  Before I
;; had this logic in the build functions themselves which each of them longer
;; and more confusing.
(defun! oo--build-steps (metadata)
  "Return the list of build steps for metadata."
  (flet! letterp (obj)
    (and (symbolp obj) (= 1 (length (symbol-name obj)))))
  (set! state (map-elt metadata :state))
  (cond ((member state '(g global))
         (pushing! steps 'oo--build-define-key))
        ((map-elt metadata :mode)
         (pushing! steps 'oo--build-evil-define-minor-mode-key))
        (t
         (pushing! steps 'oo--build-evil-define-key)))
  (if (map-elt metadata :wk)
      (pushing! steps 'oo--build-which-key)
    (pushing! steps 'oo--build-kbd))
  (pushing! steps 'oo--build-let-binds)
  (pushing! steps 'oo--build-defer-keymap)
  (unless (member state '(g global))
    (if (letterp state)
        (pushing! steps 'oo--build-defer-evil-state-char)
      (pushing! steps 'oo--build-defer-evil-state)))
  (nreverse steps))
;;;; bind! 
;; (defmacro bind! (&rest args)
;;   (oo--build-body (oo--build-steps args)))
;;; provide
(provide 'oo-base-macros-bind-bang)
;;; oo-base-macros-bind-bang.el ends here
