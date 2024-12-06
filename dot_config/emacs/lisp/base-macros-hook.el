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
(require 'base-lib)
(require 'base-requirements)
(require 'base-macros-definers)
;;;; hooks
;;;;; oo-add-hook-symbol-p
(defun! oo-add-hook-symbol-p (symbol)
  "Return non-nil if SYMBOL is a hook symbol."
  (declare (pure t) (side-effect-free t))
  (when (symbolp symbol)
    (set! name (symbol-name symbol))
    (string-match-p "[^[:space:]]+-hook\\'" name)))
;;;;; defhook!
(defmacro! hook! (hook function &rest args)
  (set! fname (intern (format "%s&%s" hook function)))
  `(progn
     (declare-function ,function nil)
     (defun ,fname (&rest _)
       ,(string-join (list (format "Call `%s' from `%s'." function hook)
                           (format "If `oo-debug-p' is non-nil suppress and log any error raised by `%s'." function))
                     "\n")
       (info! "HOOK: %s -> %s" ',hook ',function)
       (condition-case err
           (apply #',function args)
         (error
          (cond (oo-debug-p
                 (signal (car err) (cdr err)))
                (t
                 (error! "Error calling %s in %s because of %s"
                         ',function
                         (car err)
                         (cdr err)))))))
     (add-hook ',hook #',fname ,args)))

(defmacro! defhook! (&rest args)
  "Add function to hook as specified by NAME."
  (declare (indent defun))
  (cl-flet ((hook-symbol-p (obj)
              (and (symbolp obj)
                   (string-match-p "[^[:space:]]+-hook\\'" (symbol-name obj)))))
    (set! name (pop args))
    (cl-assert (oo-true-list-p (car args)))
    (set! (hooks fargs) (-separate #'hook-symbol-p (pop args)))
    (when (stringp (car args))
      (collecting! metadata (pop args)))
    (when (equal 'declare (car-safe (car args)))
      (collecting! metadata (pop args)))
    (when (vectorp (car args))
      (set! hook-args (append (pop args) nil)))
    (set! body args)
    `(progn
       (defun! ,name ,fargs ,@metadata ,@body)
       ,@(--map `(oo-add-hook ',it #',name ,@hook-args) hooks))))
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
;;; provide
(provide 'base-macros-hook)
;;; base-macros-hook.el ends here
