;;; oo-after-load.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-base)
;;;; lazy-load after-load files
;; The "after-load" files contain customizations that should be enabled after a
;; package has been loaded.  These files are usually more meaty than the init
;; files.
;; Tell the `load-path'.
(progn!
  (set! regexp "\\`oo-after-load-\\(.+\\)")
  (flet! feature-name (-compose #'file-name-sans-extension #'file-name-nondirectory))
  (flet! feature (-compose #'intern #'feature-name))
  (flet! package (file) (alet (feature-name file) (string-match regexp it) (intern (match-string 1 it))))
  (for! (file (directory-files (locate-user-emacs-file "lisp") t regexp))
    (set! package (package file))
    (set! feature (feature file))
    (cond ((featurep package))
          (t
           (oo-call-after-load package #'require feature)))))
;;; provide
(provide 'oo-after-load)
;;; oo-after-load.el ends here
