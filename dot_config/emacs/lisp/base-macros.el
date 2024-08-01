;;; base-macros.el -*- lexical-binding: t; -*-
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
;; The purpose of this file is simply to require all of the macros I am using.
;; This file is meant to be compiled away--as in `(eval-when-compile (require
;; 'oo-macros))'.  I am putting the macros in this file like this so that it is
;; easy to compile them away as opposed to intermingling them with functions.
;;
;;; Code:
(require 'base-macros-ing)
;; (require 'base-macros-advice)
(require 'base-macros-hook)
(require 'base-macros-bind)
(require 'base-macros-loop)
(require 'base-macros-let)
(require 'base-macros-lef)
(require 'base-macros-progn)
(require 'base-macros-with-map)
(require 'base-macros-opt)
;;; provide
(provide 'base-macros)
;;; base-macros.el ends here
