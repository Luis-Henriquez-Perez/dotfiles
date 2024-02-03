;;; oo-base-custom.el --- Tools to config features -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 02 Jan 2024
;;
;; URL: https://github.com/Luis-Henriquez-Perez/dotfiles
;;
;; License: GPLv3
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;
;;; Code:
(require 'oo-base-library)
;;;; custom
(defvar oo-old-values-alist nil
  "An alist that contains symbols I want to \"reset\" after startup.
Each element is of the form (SYMBOL OLD-VALUE SETTER).  SYMBOL's value should be
reset to OLD-VALUE by calling SETTER with SYMBOL and OLD-VALUE.")

(defmacro startup-set! (symbol value &optional setter)
  "Set VAR to VALUE using SETTER.
At the end of `emacs-statup-hook' set VAR back to its original VALUE."
  `(progn (setf (alist-get ',symbol oo-old-values-alist)
                (list ,symbol #',setter))
          (funcall (or #',setter #'set) ',symbol ,value)))

(provide 'oo-base-custom)
;;; oo-base-custom.el ends here
