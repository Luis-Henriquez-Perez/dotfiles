;;; oo-after-load-aas.el --- aas configuration -*- lexical-binding: t; -*-
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
;; At first glance I thought that aas was obviously better than abbrev because
;; it expands.  However, looking at the issue more closely makes it more and
;; more unclear.
;;
;; A discussion of the subtle differences between abbrev and `aas':
;;
;; 1. overriding snippets
;; A consequence of expanding snippets immediately is that they can override
;; another snippet.  For instance, using abbrev you could have BLANK1 and BLANK2
;; be non-overlapping snippets, but with `aas' BLANK1 would shadow BLANK2.  You
;; would type in ";" and it would immediately expand into ";; ".  Whereas with
;; abbrev, the expansion is triggered by a space, thereby giving you a way to
;; differentiate the two.
;;
;; 2. residue space
;; When using abbrev you end up with a residue space when
;; expanding snippets.  I am sure it is not hard to fix this but it does suggest
;; that using `aas' can be more convenient in at least some cases.  Since the
;; snippet would expand immediately.
;;
;; 3. not using regexps
;; One gotcha is that `aas' does *not* use regexps to match the previous thing,
;; it builds.  In practice this is a trade-off.
;;
;; 4.
;; You cannot write the same kind of abbrevs.
;;
;; TODO: find a use-case for aas in combination with abbrev
;; One thing you can do is use aas in combination with abbrev.  You can use
;; abbrev to handle most of the abbrevs.  One idea is to use it in cases where a
;; space might interfere with the abbrev itself.
;;
;;; Code:
(require 'oo-base)
;;;; replace two spaces with a period
(defun oo--aas-text-enable-p ()
  (and (or (derived-mode-p 'text-mode)
           (oo-in-string-or-comment-p))
       (not (minibuffer-window-active-p (minibuffer-window)))))
;;; provide
(provide 'oo-after-load-aas)
;;; oo-after-load-aas.el ends here
