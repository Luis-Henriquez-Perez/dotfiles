;;; config-tempel.el --- Configure tempel -*- lexical-binding: t; -*-
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
;; Configure tempel.
;;
;;; Code:
;;;; make tempel-map an overriding map
(defhook! make-tempel-map-overriding (oo-after-load-evil-hook)
  (evil-make-overriding-map tempel-map))
;;;; Specify rules for abbrev expansion
(defun +abbrev-emacs-lisp-mode-enable-p ()
  "Do not enable programming mode abbrevs in docstrings or comments."
  (and (derived-mode-p 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))))
;;;; Abbrev snippets
;; TODO: make a function or (most likely) macro for this.
;; These are the rules that must be strictly followed for a snippet abbrev to
;; work with tempel.
;; 1. It needs to be a function symbol that is passed in as the hook.  No
;;    lambdas.
;; 2. The symbol should have a `no-self-insert' property of t.  Otherwise, the
;;    abbrev will be inserted as well.
;; 3. When using evil, the snippet should call `evil-normalize-keymaps' after
;;    the snippet has been inserted otherwise the bindings for `tempel-map' will
;;    not be active until a state change.
;; 4. The definition of the abbrev must be an empty string.
;; 5. The hook function must return t.
(put '+abbrev-insert-defun 'no-self-insert t)
(defun +abbrev-insert-defun ()
  (tempel-insert 'fn)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(define-abbrev +abbrev-table-main "fun" "" '+abbrev-insert-defun :enable-function #'+abbrev-emacs-lisp-mode-enable-p)

(put '+abbrev-insert-alias 'no-self-insert t)
(defun +abbrev-insert-alias ()
  (tempel-insert 'als)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)
(define-abbrev +abbrev-table-main "als" "" '+abbrev-insert-alias :enable-function #'+abbrev-emacs-lisp-mode-enable-p)

(put '+abbrev-insert-defvar 'no-self-insert t)
(defun +abbrev-insert-defvar ()
  (tempel-insert 'vr)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)
;; I found a surprising interaction with abbrev tables and enable functions.
;; Prior to this I had thought that if I added an enable property to a table I
;; could override this property for an individual abbrev by specifying its
;; enable function explicitly.  However, it seems that the enable function of
;; the table is still considered.  Adding an enable function makes it so that
;; both the table's enable function and the abbrev's enable function have to
;; return true.
(define-abbrev +abbrev-table-main "dv" "" '+abbrev-insert-defvar :enable-function #'+abbrev-emacs-lisp-mode-enable-p)
;;; provide
(provide 'config-tempel)
;;; config-tempel.el ends here
