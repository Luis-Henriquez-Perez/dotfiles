;;; oo-package-lib.el --- Tools for pacman -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 7 Feb 2024
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
;; This file provides me tools for installing packages.
;;
;;; Code:
(require 'package)

;; I want a clearer name.  When I saw the name "elpa" I had no idea what was in that folder.
(setq package-user-dir (locate-user-emacs-file "packages/"))

;; Add MELPA repository if it's not already included
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Needed for `compat' library maybe.
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
;; Initialize the package system
(package-initialize)

;; Refresh package contents if the package list is empty
(unless package-archive-contents
  (package-refresh-contents))
;; So many problems. One problem is I compiling errors.

(push 'which-key package-selected-packages)
(push 'no-littering package-selected-packages)
(push 'grugru package-selected-packages)
(push 'rainbow-delimiters package-selected-packages)
(push 'goto-chg package-selected-packages)
(push 'evil-goggles package-selected-packages)
(push 'macrostep package-selected-packages)
(push 'evil package-selected-packages)
(push 'evil-goggles package-selected-packages)
(push 'smartparens package-selected-packages)
(push 'lispy package-selected-packages)
(push 'lispyville package-selected-packages)
(push 'corfu package-selected-packages)
(push 'consult package-selected-packages)
(push 'burly package-selected-packages)
(push 'evil-surround package-selected-packages)
(push 'expand-region package-selected-packages)
(push 'helm package-selected-packages)
(push 'gcmh package-selected-packages)
;; (push 'modus-themes package-selected-packages)
(push 'meow package-selected-packages)
(push 'tempel package-selected-packages)
(push 'redacted package-selected-packages)
(push 'orderless package-selected-packages)
(push 'eros package-selected-packages)
(push 'eshell-up package-selected-packages)
(push 'eshell-z package-selected-packages)
(push 'emms package-selected-packages)
(push 'xr package-selected-packages)
(push 'super-save package-selected-packages)
(push 'helm-system-packages package-selected-packages)
(push 'fennel-mode package-selected-packages)
(push 'dirvish package-selected-packages)
(push 'captain package-selected-packages)
(push 'org-bookmark-heading package-selected-packages)
(push 'outshine package-selected-packages)
(push 'cape package-selected-packages)
(push 'aggressive-indent package-selected-packages)
(push 'ws-butler package-selected-packages)
(push 'magit package-selected-packages)
(push 'evil-magit-init package-selected-packages)

(package-install-selected-packages t)

(provide 'oo-package-lib)
