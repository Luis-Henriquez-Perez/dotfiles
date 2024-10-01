;;; +abbrev-immediate-abbrevs.el --- immediately expanding abbrevs -*- lexical-binding: t; -*-
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
;; This abbrev table is for text I want expanded immediately.
;;
;;; Code:
;;;; requirements
(require 'abbrev)
(require 'base)
;;;; code for defining immediate abbrevs
(defvar +abbrev-immediate-abbrevs (make-abbrev-table)
  "A abbrev table containing abbrevs that should be expanded immediately.")
;;;; abbrevs that expand immediately
(defhook! expand-immediate-abbrevs (post-self-insert-hook)
  "Expand abbrevs used for immediate expansion."
  (require 'oo-immediate-abbrevs)
  (let ((local-abbrev-table oo-immediate-abbrev-table))
    (funcall abbrev-expand-function)))
;;; provide
(provide '+abbrev-immediate-abbrevs)
;;; +abbrev-immediate-abbrevs.el ends here
