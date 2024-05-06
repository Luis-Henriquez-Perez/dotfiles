;;; 99-after-load-grugru.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; For helpful information see [[][]]
;; (grugru-define-on-major-mode)

;;; grugru

;;;; define opposites
;; Define grugru globally.  This is applied in all buffers.
(grugru-define-global 'symbol '("yes" "no"))

(grugru-define-global 'symbol '("up" "down"))

(grugru-define-global 'symbol '("left" "right"))

(grugru-define-global 'symbol '("wrong" "right"))

(grugru-define-global 'symbol '("red" "orange" "yellow" "green" "blue" "indigo" "violet"))

(grugru-define-global 'symbol '("front" "back"))

(grugru-define-global 'symbol '("inner" "outer"))

;;;; emacs-lisp
(grugru-define-on-major-mode '(emacs-lisp) 'word '("let" "let*" "let!" "-let"))

(grugru-define-on-major-mode '(emacs-lisp) 'word '(abbrev-table-put abbrev-table-get))

(grugru-define-on-major-mode '(emacs-lisp) 'word '(gethash puthash))
;;; provide
(provide '99-after-load-grugru)
;;; 99-after-load-grugru.el ends here
