;;; init-evil-collection.el --- Initialize `evil-collection' -*- lexical-binding: t; -*-
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
;; Initialize `evil-collection'.
;;
;;; Code:
;; Load dired bindings.
(autoload #'evil-collection-dired-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-dired-hook evil-collection-dired-setup)

(autoload #'evil-collection-calc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-calc-hook evil-collection-calc-setup)

(autoload #'evil-collection-info-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-info-hook evil-collection-info-setup)

(autoload #'evil-collection-ibuffer-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ibuffer-hook evil-collection-ibuffer-setup)

(autoload #'evil-collection-eww-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eww-hook evil-collection-eww-setup)
;;; provide
(provide 'init-evil-collection)
;;; init-evil-collection.el ends here
