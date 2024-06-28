;;; oo-after-load-dired.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(require 'dired)

(opt! wdired-confirm-overwrite nil)
(setq dired-clean-confirm-killing-deleted-buffers nil)
;;;; dired
(bind! oo-app-map "d" #'dired)
;;;;; map =h= to =dired-up-directory=
;; TODO: investigate whether `bind!' is working properly.
;; When I was evaluating this immediately upon startup the dired binding did not
;; take effect.  However, when I put this in an after-load file, the binding
;; successfully took effect.

;; I do not want to keep pressing =^= for the common action of going up the
;; directory.
(bind! dired-mode-map :nm "h" #'dired-up-directory)

;; Additionally, =l= is faster than =Enter= on a QWERTY keyboard.
(bind! dired-mode-map :nm "l" #'dired-find-file)
;;;; dirvish
(bind! :alt dired dirvish)

(opt! dirvish-use-mode-line nil)

(opt! dirvish-attributes '(file-size subtree-state))

(opt! dirvish-default-layout nil)

(oo-add-hook 'dired-mode-hook #'dired-omit-mode)
;; By default hide details.
(oo-add-hook 'dired-mode-hook #'dired-hide-details-mode)

(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)
;;; provide
(provide 'oo-after-load-dired)
;;; oo-after-load-dired.el ends here
