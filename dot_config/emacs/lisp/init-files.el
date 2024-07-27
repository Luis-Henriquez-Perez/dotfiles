;;; init-files.el --- initialize files -*- lexical-binding: t; -*-
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
;; Initialize files.
;;
;;; Code:
;;;; don't pass case-insensitive to =auto-mode-alist=
;; This is taken from =centaur-emacs=.  By default [[file:snapshots/*helpful variable: auto-mode-case-fold*.png][auto-mode-case-fold]] is
;; non-nil; when enabled the auto-mode-alist is traversed twice.  This double
;; traversal can be expensive and it seems unnecessary.
(setq auto-mode-case-fold nil)
;;;; stop asking me whether I want to enable file local variables
;; When installing packages with =quelpa=, I was prompted whether I wanted to apply
;; file local variables.  I'm guessing =straight.el= and =elpaca= disable this.
;; The value safe tells Emacs to only apply the "safe" local variables.  I'm
;; assuming this means ones like "mode" which tell Emacs to open the buffer at a
;; certain major mode.  At first I had this set to nil, but I wanted to open
;; [[][]] in =common-lisp-mode= and I realized Emacs wasn't doing it because I
;; told it not to with this variable.
(setq enable-local-variables :safe)
;;; provide
(provide 'init-files)
;;; init-files.el ends here
