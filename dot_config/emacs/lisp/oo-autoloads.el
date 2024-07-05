;;; oo-autoloads.el --- autoloads for lisp directory -*- lexical-binding: t; -*-
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
;; This file contains the autoloads for lisp directory.  I specifically write
;; them out by hand because I am not a fan of having to generate a file.  With
;; this out of the way, it is one less thing I have todo.
;;
;;; Code:
(autoload #'chezmoi-find                       "chezmoi"                           nil t 'function)
(autoload #'chezmoi-write                      "chezmoi"                           nil t 'function)
(autoload #'chezmoi-open-other                 "chezmoi"                           nil t 'function)
(autoload #'evil-operator-eval                 "evil-extra-operator"               nil t 'function)
(autoload #'evil-operator-eval-replace         "evil-extra-operator"               nil t 'function)
(autoload #'evil-inner-line                    "evil-textobj-line"                 nil t 'function)
(autoload #'evil-a-line                        "evil-textobj-line"                 nil t 'function)
(autoload #'evil-i-syntax                      "evil-textobj-syntax"               nil t 'function)
(autoload #'evil-a-syntax                      "evil-textobj-syntax"               nil t 'function)
(autoload #'oo-blog-new-post                   "extensions/oo-blog-utils"          nil t 'function)
(autoload #'oo/ensure-provide                  "extensions/oo-package-boilerplate" nil t 'function)
(autoload #'oo/ensure-file-header              "extensions/oo-package-boilerplate" nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-word "oo-after-load-evil-easymotion"     nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-WORD "oo-after-load-evil-easymotion"     nil t 'function)
(autoload #'oo-evilem-motion-end-of-word       "oo-after-load-evil-easymotion"     nil t 'function)
(autoload #'oo-evilem-motion-end-of-WORD       "oo-after-load-evil-easymotion"     nil t 'function)
(autoload #'oo-evilem-motion-char              "oo-after-load-evil-easymotion"     nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-line "oo-after-load-evil-easymotion"     nil t 'function)
(autoload #'oo-set-font-face                   "oo-commands"                       nil t 'function)
(autoload #'oo-sort-elpaca-forms               "oo-commands"                       nil t 'function)
(autoload #'oo-split-window-below-and-focus    "oo-commands"                       nil t 'function)
(autoload #'oo-split-window-right-and-focus    "oo-commands"                       nil t 'function)
(autoload #'oo-open-emacs-init-file            "oo-commands"                       nil t 'function)
(autoload #'oo-add-new-abbrev                  "oo-commands"                       nil t 'function)
(autoload #'oo-pop-to-buffer                   "oo-commands"                       nil t 'function)
;;; provide
(provide 'oo-autoloads)
;;; oo-autoloads.el ends here
