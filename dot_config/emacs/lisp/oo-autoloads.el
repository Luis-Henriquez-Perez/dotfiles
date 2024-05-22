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
(autoload #'oo/ensure-provide "xx-package-boilerplate" nil t 'function)
(autoload #'oo/ensure-file-header "xx-package-boilerplate" nil t 'function)

;; The problem with these keys is that they interfere with keyboard macros.  Let
;; me explain--when you use avy, it is not necessarily the case that the
;; following keys have the same letter.  For keyboard macros to work you need
;; keys to exhibit predictable behaviors.  I do not want to get rid of these
;; keys entirely, but I have to consider more carfully on how I will re-add them
;; to my configuration.  TODO: Maybe make more direct or add support for
;; autoloading in the lisp directory.  Right now this loads easymotion which
;; causes these functons to be define by me with `eval-after-load'.
(autoload #'oo-evilem-motion-beginning-of-word "evil-easymotion")
(autoload #'oo-evilem-motion-beginning-of-WORD "evil-easymotion")
(autoload #'oo-evilem-motion-end-of-word "evil-easymotion")
(autoload #'oo-evilem-motion-end-of-WORD "evil-easymotion")
(autoload #'oo-evilem-motion-char "evil-easymotion")
(autoload #'oo-evilem-motion-beginning-of-line "evil-easymotion")
(autoload #'oo-set-font-face "oo-ext-commands")
(autoload #'oo-split-window-below-and-focus "oo-ext-commands")
(autoload #'oo-split-window-right-and-focus "oo-ext-commands")
;;; provide
(provide 'oo-autoloads)
;;; oo-autoloads.el ends here
