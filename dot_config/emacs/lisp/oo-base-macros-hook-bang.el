;;; oo-base-macros-hook-bang.el --- macro for adding hooks -*- lexical-binding: t; -*-
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
(require 'oo-base-vars)
(require 'oo-base-lib)

(defmacro! hook! (name &rest plist)
  "Define a function named NAME and add it to hook.
NAME is a symbol of the form HOOK&FUNCTION.  HOOK is the hook to which the
symbol NAME will be added.  FUNCTION is the function NAME should call when
invoked.  The defined function will log its usage and suppress errors whenever
`oo-debug-p' is nil, logging them instead."
  (set! append (or (plist-get plist :depth) (plist-get plist :append)))
  (set! local (plist-get plist :local))
  (alet (symbol-name name)
    (string-match "\\([^[:space:]]+\\)&\\([^[:space:]]+\\)" it)
    (set! hook (intern (match-string 1 it)))
    (set! fn (intern (match-string 2 it))))
  `(progn (defun ,name (&rest args)
            (info! "Running hook %s -> %s..." ',hook ',fn)
            (condition-case err
                (apply #',fn args)
              (error (if oo-debug-p
                         (signal (car err) (cdr err))
                       (error! "Error %s calling %s in %s because of %s"
                               ',hook
                               ',fn
                               (car err)
                               (cdr err))))))
          (add-hook ',hook #',name ,append ,local)))
;;; provide
(provide 'oo-base-macros-hook-bang)
;;; oo-base-macros-hook-bang.el ends here
