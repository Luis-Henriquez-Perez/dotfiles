;;; oo-after-load-tempel.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; tempel
(bind! tempel-map :ieg "C-j" #'tempel-next)
(bind! tempel-map :ieg "C-k" #'tempel-previous)
(bind! tempel-map :ieg "TAB" #'tempel-next)
(bind! tempel-map :ieg [backtab] #'tempel-previous)
;;;; make tempel an overriding map
(defafter! make-tempel-map-an-overriding-map (evil tempel)
  "Make `tempel-map' into an overriding map."
  (evil-make-overriding-map tempel-map))
;;; provide
(provide 'oo-after-load-tempel)
;;; oo-after-load-tempel.el ends here
