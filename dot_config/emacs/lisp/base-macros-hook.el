;;; base-macros-hook.el --- macro for adding hooks -*- lexical-binding: t; -*-
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
;; This file provides a macro `hook!' that generates functions that are designed
;; to added to hooks.  These generated functions suppress errors when
;; `oo-debug-p' is nil and log their usage.  The idea is part of my quest to
;; make my configuration introspection-friendly.  I can see in a log buffer
;; which hooks were called and when they were called.  The error suppression is
;; to stop hooks from "short-circuiting" when an error is raised in one of their
;; invoked functions.  As is, if there is an error is raised when a hook is run,
;; all subsequent functions in that hook never get run.  Instead of stopping
;; abruptly try to run as many hook functions as you can.
;;
;;; Code:
;;;; requirements
(require 'base-vars)
(require 'base-macros-definers)
(require 'init-lgr)
(require 'dash)
;;;; oo-hook-symbol-p
(defun! oo-hook-symbol-p (symbol)
  "Return non-nil if SYMBOL is a hook symbol."
  (declare (pure t) (side-effect-free t))
  (when (symbolp symbol)
    (set! name (symbol-name symbol))
    (string-match-p (rx (1+ (not white)) "-hook" eos) name)))
;;;; oo--hook-fn
;; I am hesitant about having the `oo-hook-function' both generate the fn
;; that produces the hook and add it to the hook, but as of yet I do not see a
;; reason not to have it do this.  In other words, I cannot imagine a case where
;; I would be using this function and not adding a hook.  If that changes I can
;; just change this function.
(defun oo-hook-function (hook suffix body-fn depth local)
  "Generate a hook function from HOOK, SUFFIX and BODY-FN."
  (set! name (intern (format "+%s&%s" hook suffix)))
  (defvaralias name
    `(lambda (&rest args)
       (info! "Running hook %s..." ',name)
       (condition-case err
           (funcall #',body-fn args)
         (error (if oo-debug-p
                    (signal (car err) (cdr err))
                  (message "Error calling %s in %s because of %s"
                           ',name
                           (car err)
                           (cdr err)))))))
  (add-hook hook name depth local)
  name)
;;;; oo--defhook-arguments
(defun! oo--defhook-arguments (args)
  (set! name (pop args))
  (set! arglist (pop args))
  (while (oo-hook-symbol-p (car arglist))
    (collecting! hooks (pop arglist)))
  (when (stringp args)
    (set! docstring (pop args)))
  (when (vectorp (car args))
    (alet (append (pop args) nil)
      (set! depth (or (map-elt it :depth) (map-elt it :append)))
      (set! local (map-elt it :local))))
  (set! body args)
  (list name arglist hooks body depth local))
;;;; defhook!
(defmacro! defhook! (&rest args)
  "Add function to hook as specified by NAME.
NAME should be a hook symbol."
  (declare (indent defun))
  (set! (suffix arglist hooks body depth local) (oo--defhook-arguments args))
  (macroexp-progn (--map `(oo-hook-function ',it ',suffix (lambda ,arglist ,@body) ,depth ,local) hooks)))
;;; provide
(provide 'base-macros-hook)
;;; base-macros-hook.el ends here
