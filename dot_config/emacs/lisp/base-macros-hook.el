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
;; This file provides a macros that generate functions that are designed to
;; added to hooks.  These generated functions suppress errors when `oo-debug-p'
;; is nil and log their usage.  The idea is to make my configuration
;; introspection-friendly.  Logging lets me see which hook functions are called,
;; when they are called and in what order.  The error suppression stop hooks
;; from "short-circuiting" when an error is raised in a hook function.  As is,
;; if there is an error is raised in a hook function when a hook is run, all
;; subsequent functions in that hook never get run.  By suppressing (but
;; logging) errors I am saying: if something goes wrong with one hook do not
;; abort the hook, still run the rest.  Which is the behavior I want the vast
;; majority of the time.
;;; Code:
;;;; requirements
(require 'base-vars)
(require 'base-macros-autolet)
(require 'base-macros-definers)
;;;; hooks
;;;;; defhook!
;; This is a convenience macro for.
(defmacro hook! (hook function &rest args)
  ""
  `(progn (declare-function ,function)
          (oo-add-hook ',hook #',function ,@args)))

(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME."
  (declare (indent defun))
  (set! hook (pop args))
  (when (stringp (car body))
    (collecting! metadata (pop body)))
  (when (equal 'declare (car-safe (car body)))
    (collecting! metadata (pop body)))
  `(progn
     (defun! ,name nil ,@metadata ,@body)
     (hook! ,hook ,name ,@args)))
;;;;; after!
;; I made the decision to add a hook function to a hook regardless of whether
;; the hook has already has been run.  But if the hook has been run the hook
;; function is called individually.  The idea is that I do not want to just
;; evaluate the body and have no record of it being evaluated other than it is
;; side-effects.
(defmacro defafter! (name expr &rest body)
  "Evaluate BODY after EXPR is satisfied."
  (declare (indent defun))
  `(progn
     (defun! ,name nil ,@body)
     (oo-call-after-load ',expr #',name)))
;;;;; require!
(defmacro! require! (file &optional feature)
  (set! name (intern (format "oo--load-%s" file)))
  (string-match "\\`config-\\(.+\\)\\'" (symbol-name file))
  (set! feature (or feature (intern (match-string 1 (symbol-name file)))))
  `(progn (unless (fboundp ',name)
            (defun ,name ()
              (require ',file)))
          (oo-call-after-load ',feature #',name)))
;;;;; setq-hook
;;;; setq-hook!
(defmacro! setq-hook! (hooks symbol value)
  "Set the local value of hook."
  (set! name (intern (format "set-local-var-%s" symbol)))
  `(defhook! ,name ,(ensure-list hooks)
     ,(format "Set local variable `%S' locally." symbol)
     (setq-local ,symbol ,value)))
;;; provide
(provide 'base-macros-hook)
;;; base-macros-hook.el ends here
