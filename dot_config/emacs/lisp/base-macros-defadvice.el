;;; base-macros-defadvice.el --- advising macros -*- lexical-binding: t; -*-
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
;; Just like hooks, I want my advices to be named functions specifically
;; designed to be advices.  The point of these advice
;; macros is to help me seemlessly write named advices.
;;
;;; Code:
(require 'base-macros-definers)
;;;; defadvice!
(defmacro! defadvice! (&rest args)
  "Define an advice."
  (declare (indent defun))
  (set! suffix (pop args))
  (set! arglist (pop args))
  (set! how (oo-into-keyword (pop arglist)))
  (set! symbol (pop arglist))
  (set! body args)
  (set! body-fn `(lambda ,arglist (block! ,@body)))
  `(oo-generate-advice ,how ',symbol ',suffix ,body-fn))
;;; provide
(provide 'base-macros-defadvice)
;;; base-macros-defadvice.el ends here
