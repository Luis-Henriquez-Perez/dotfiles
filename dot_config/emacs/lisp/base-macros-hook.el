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
;;
;; Notably, I am also experimenting with converting the `after-load' mechanism
;; to hook fashion.  The after-load mechanism I am referring to using
;; `eval-after-load' to add to the `after-load-alist', essentially a way to
;; evaluate code just after a feature is loaded.  The drawback is that it is not
;; introspectable.  With hooks if you want to know the functions that are run
;; you can just look at the value of the hook.  Moreover, if you want to change
;; what functions are being run you can simply add or remove from the hook.  In
;; contrast, `after-load-alist' is simply too large to examine and usually
;; contains predominately anonymous lambda.  Although it is easy to add to it,
;; examining it and removing from it are out of the question.
;;
;;; Code:
;;;; requirements
(require 'base-vars)
(require 'base-lib)
(require 'base-requirements)
(require 'base-macros-definers)
;;;; hooks
;;;;; oo-hook-symbol-p
(defun! oo-hook-symbol-p (symbol)
  "Return non-nil if SYMBOL is a hook symbol."
  (declare (pure t) (side-effect-free t))
  (when (symbolp symbol)
    (set! name (symbol-name symbol))
    (string-match-p "[^[:space:]]+-hook\\'" name)))
;;;;; oo--setup-after-load-hook-maybe
(defun! oo--setup-after-load-hook-maybe (hook)
  "Return a list of forms that generates an after-load hook."
  (set! name (symbol-name hook))
  (when (string-match "\\`oo-after-load-\\(.+\\)-hook\\'" name)
    (set! feature (intern (match-string 1 name)))
    (set! run-fn (intern (format "oo-run-after-load-%s-hook" feature)))
    `((unless (boundp ',hook)
        (defvar ,hook nil
          ,(format "Hook run after feature `%s' is loaded." feature))
        (defun ,run-fn (&rest _)
          (info! "Running `%s'..." ',hook)
          ,(format "Run `%s' after feature `%s' has been loaded." hook feature)
          (run-hooks ',hook))
        (oo-call-after-load ',feature #',run-fn)))))
;;;;; oo-generate-hook-forms
;; I am hesitant about having the `oo-generate-hook' both generate the fn
;; that produces the hook and add it to the hook, but as of yet I do not see a
;; reason not to have it do this.  In other words, I cannot imagine a case where
;; I would be using this function and not adding a hook.  If that changes I can
;; just change this function.
(defun! oo-generate-hook-forms (hook suffix forms depth local)
  "Produce a form that generates a hook function."
  (set! name (intern (format "%s&%s" hook suffix)))
  `(,@(oo--setup-after-load-hook-maybe hook)
    (defun ,name (&rest args)
      (info! "Running hook %s..." ',name)
      (condition-case err
          ,(macroexp-progn forms)
        (error (if oo-debug-p
                   (signal (car err) (cdr err))
                 (message "Error calling %s in %s because of %s"
                          ',name
                          (car err)
                          (cdr err))))))
    (add-hook ',hook #',name ,depth ,local)
    ',name))
;;;;; hook!
(cl-defmacro hook! (hook fn &key append depth local)
  "Generate a hook that calls function."
  (macroexp-progn (oo-generate-hook-forms hook fn `((funcall ',fn)) (or append depth) local)))
;;;;; oo--defhook-arguments
(defun! oo--defhook-arguments (args)
  ""
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
;;;;; defhook!
(defmacro! defhook! (&rest args)
  "Add function to hook as specified by NAME."
  (declare (indent defun))
  (set! (fn-symbol arglist hooks body depth local) (oo--defhook-arguments args))
  (dolist (hook hooks)
    (appending! forms (oo-generate-hook-forms hook fn-symbol body depth local)))
  `(progn ,@forms))
;;;;; after!
;; I made the decision to add a hook function to a hook regardless of whether
;; the hook has already has been run.  But if the hook has been run the hook
;; function is called individually.  The idea is that I do not want to just
;; evaluate the body and have no record of it being evaluated other than it is
;; side-effects.
(defmacro after! (suffix expr &rest body)
  "Evaluate BODY after EXPR is satisfied."
  (declare (indent defun))
  (pcase expr
    ((pred null) nil)
    ((and feature (pred symbolp))
     (set! hook (intern (format "oo-after-load-%s-hook" feature)))
     `(aprog1 ,(macroexp-progn (oo-generate-hook-forms hook suffix body nil nil))
        (when (featurep ',feature) (funcall it))))
    (`(:or . ,exprs)
     `(progn ,@(--map `(after! ,suffix ,it ,@body) exprs)))
    (`(:and . ,exprs)
     `(after! ,suffix ,exprs ,@body))
    (`(,expr . nil)
     `(after! ,suffix ,expr ,@body))
    (`(,expr . ,exprs)
     `(after! ,suffix ,expr (after! ,suffix ,exprs ,@body)))
    (_
     (error "invalid expression `%S'" expr))))
;;;;; require!
(defmacro! require! (file &optional feature)
  (set! name (intern (format "load-%s" file)))
  (if feature
      `(after! ,name (,feature) (require ',file))
    (string-match "\\`config-\\(.+\\)\\'" (symbol-name file))
    (set! feature (intern (match-string 1 (symbol-name file))))
    `(after! ,name (,feature) (require ',file))))
;;; provide
(provide 'base-macros-hook)
;;; base-macros-hook.el ends here
