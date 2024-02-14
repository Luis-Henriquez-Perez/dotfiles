;;; 12-init-benchmark-init.el --- initialize `benchark-init' -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 08 Jan 2021
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
;; These are my personal Emacs configurations.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
(require 'benchmark-init)

(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(oo-add-hook 'after-init-hook 'benchmark-init/deactivate)
;;; provide
(provide '12-init-benchmark-init)
;;; 12-init-benchmark-init ends here
