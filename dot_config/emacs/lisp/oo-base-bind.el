;;; oo-base-bind.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-base-lib)
;;;; binding keys
;; Inspired by [[https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding][this]] stackoverflow question, this macro lets me create conditional
;; bindings for commands giving me a flexible and robust experience with key
;; bindings.  By "condition bindings" I mean key bindings that can invoke a
;; particular command based on certain conditions.  For example, =SPC h f=  might
;; invoke [[file:snapshots/_helpful_command__helpful_callable_.png][helpful-callable]] if the package helpful is present (see [[][]]), otherwise it
;; would fallback to [[file:snapshots/_helpful_command__describe-function_.png][describe-function]] instead.

;; As opposed to [[file:snapshots/_helpful_special_form__cond_.png][cond]], for example, which requires multiple conditions I designed
;; this macro to add one condition at a time.  I do not want to be tied to naming
;; all the conditions at once in general I write my configuration in such a way
;; that I can augment it incrementally as opposed to building one big block of
;; code.
(defvar oo-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(defun oo-alternate-command-choose-fn (command)
  "Return command that should be called instead of COMMAND."
  (or (oo-first-success #'funcall (gethash command oo-alternate-commands))
      command))

(defun! oo-alt-bind (map orig alt &optional condition)
  "Remap keys bound to ORIG so ALT is called if CONDITION returns non-nil.
ORIG and ALT are command symbols.  CONDITION is a function that returns non-nil
when ALT should be invoked instead of ORIG."
  (flet! oo-when-fn (condition fn)
    `(lambda (&rest _) (when (funcall #',condition) #',alt)))
  (push (oo-when-fn (or condition #'always) alt) (gethash orig oo-alternate-commands))
  (define-key map `[remap ,orig] `(menu-item "" ,orig :filter oo-alternate-command-choose-fn)))

;; The functions here are functions to generate the body of the =bind!= macro.  I
;; divided form generation into two parts: the functions generate the deferment for
;; bindings and decide which binding function and arguments to use and the ones
;; that generate the actual binding forms.

;; This variable will contain the functions that will be called in turn to produce
;; the side-effect that results in the actual binding.  Order matters here.
(defvar oo-binding-fns '(oo--bind-alt
                         oo--bind-ensure-keybinding
                         ;; oo--bind-exwm-key
                         oo--bind-evil-state-keyword
                         oo--bind-evil-define-key
                         oo--bind-define-key)
  "List of functions that generate the body of `bind!'.")

;; This is the function that will be invoked by the functions in
;; [[id:20231026T133140.783912][oo-binding-functions]] to proceed to the next step.
(defun oo--resolve-binding (fns metadata)
  "Run the next function in `oo-bind-functions'.
If there are no more functions, do nothing."
  (funcall (or (car fns) #'ignore) (cdr fns) metadata))

(defun oo--bind-alt (fns metadata)
  (cond ((map-elt metadata :alt)
         (unless (map-elt metadata :keymap)
           (setf (map-elt metadata :keymap) global-map))
         (awhen (map-elt metadata :feature)
           (setf (map-elt metadata :condition)
                 `(lambda (&rest _) (or (featurep ',it) (require ',it nil t)))))
         (oo--do-binding metadata #'oo-alt-bind :keymap :key :def :condition))
        (t
         (oo--resolve-binding fns metadata))))

;; I think [[][general]] does the same kind of thing where you can specify a symbol
;; as the keymap in case you want to load the keybinding only when the keymap
;; symbol is defined.
(defun! oo--bind-ensure-keybinding (fns metadata)
  "Ensure metadata has essential components of a keybinding."
  (unless (and (map-elt metadata :key)
               (--any-p (map-contains-key metadata it) '(:keymap :localleader))
               (map-contains-key metadata :def))
    (error "Does not contain necessary components of keybinding."))
  (set! keymap (map-elt metadata :keymap))
  (cond ((symbolp keymap)
         (flet! insert-keymap-value (keymap fns metadata)
           (--> (map-insert metadata :keymap-symbol keymap)
             (map-insert it :keymap (symbol-value keymap))
             (oo--resolve-binding fns it)))
         (oo-call-after-bound keymap (-partial #'insert-keymap-value keymap fns metadata)))
        (t
         (oo--resolve-binding fns metadata))))

;; This function is to deal with the user passing in an abbreviation for a set of
;; states or as I call it an =evil-state-keyword= . The abbreviation.
;; ;; I need a separate function for this because I need to determine the state on
;; ;; the fly once I know it exists.  That is I can't just specify it in
;; ;; `oo--bind-state-key' as I would like to because I don't know what it is yet.
(defun! oo--bind-with-state-key (key metadata)
  "Replace state-key with state."
  (set! state (oo-evil-state key))
  (setf (map-elt metadata :state) state)
  (oo--bind-evil-define-key nil metadata))

(defun oo--bind-evil-state-keyword (fns metadata)
  (set! state-key (map-elt metadata :state-key))
  (if (not state-key)
      (oo--resolve-binding fns metadata)
    (set! keys
          (--> (symbol-name state-key)
            (seq-rest it)
            (string-split it "" t)
            (mapcar #'oo-into-keyword it)))
    (dolist (key keys)
      (if (equal key :g)
          (oo--resolve-binding fns metadata)
        (-p-> (oo--bind-with-state-key key metadata)
              (oo-call-after-evil-state key))))))

;; Part of the reason that =evil= was deferred separately in [[][]] is immediately
;; apparent: this function can be written with the assumption that =evil= is already
;; loaded.

;; The reason I use =oo-call-after-bound= here is to defer.  This is the frugal way
;; of.  I am considering [[][creating a hook]] specifically which is more involved.
;; I worry about the bindings not being active when I define a state myself and no
;; file is being loaded by =after-load-functions=.  But maybe in practice this will
;; always work?  I'm not sure so I will test it out first.
(defun! oo--bind-evil-define-key (fns metadata)
  "Define evil key based on metadata."
  (set! states (-list (map-elt metadata :state)))
  (if (not states)
      (oo--resolve-binding fns metadata)
    (dolist (state states)
      (cond ((equal state 'global)
             (oo--resolve-binding fns metadata))
            ((map-elt metadata :mode)
             (-p-> (oo--do-binding metadata #'evil-define-minor-mode-key :state :mode :key :def)
                   (oo-call-after-evil-state state)))
            (t
             (-p-> (oo--do-binding metadata #'evil-define-key* :state :keymap :key :def)
                   (oo-call-after-evil-state state)))))))

;; This is the default behavior for binding a key and what I generally will want to
;; use if I do not specify a state.
(defun oo--bind-define-key (_ metadata)
  (oo--do-binding metadata #'define-key :keymap :key :def))

;; This function is the indicator for when the function that actually does the
;; keybinding is actually called as opposed to forms that may defer the keybinding
;; until a later time.  With this I can access the precise time when a key is going
;; to be bound by advising or [[https://github.com/nicferrier/emacs-noflet][nofletting]] this function. From this function I can do
;; useful things such as override =define-key= to do something else (in fact this I
;; actually do this for using =which-key=) or even override =oo--do-binding= itself so
;; that it only either [[][logs]] or outputs to the messages buffer.  The latter is
;; useful if I want to see what keys would be bound by an invocation to =oo-bind=
;; instead of actually binding them.
(defun! oo--do-binding (metadata fn &rest keys)
  "Call FN with values of KEYS in METADATA."
  (flet! kbd-maybe (x) (if (vectorp x) x (kbd x)))
  (set! desc (map-elt metadata :wk))
  (set! args (mapcar (-partial #'map-elt metadata) keys))
  (nflet! define-key (keymap key def)
    (when desc
      (-p-> (which-key-add-keymap-based-replacements keymap key desc)
            (oo-call-after-load 'which-key)))
    (funcall this-fn keymap (kbd-maybe key) def))
  (apply fn args))

;; I need to combine the =noflet= form for =which-key= and using kbd because.

;; This is the internal function that I will use to bind keys.
(defun oo--bind (&rest metadata)
  (oo--resolve-binding oo-binding-fns metadata))

;; This is the "front-end" that I'll be using to bind keys.  Unlike the internal
;; function [[id:20231026T075710.285898][oo--bind]] whose goal is to be programmatically consistent (e.g. in
;; other functions and macros), the goal of =oo-bind= is to be easy and concise for
;; me to use for configuration.
(defun! oo-bind (&rest args)
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
     (apply #'oo--bind :alt t :key key :def def metadata))
    (`(,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
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
(provide 'oo-base-bind)
;;; oo-base-bind.el ends here
