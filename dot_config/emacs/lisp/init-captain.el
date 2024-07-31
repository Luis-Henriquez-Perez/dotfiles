;;; init-captain-mode.el --- initialize captain-mode -*- lexical-binding: t; -*-
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
;; Initialize captain-mode.
;;
;;; Code:
(require 'base)

(hook! prog-mode-hook captain-mode)
(hook! text-mode-hook captain-mode)

(defhook! text-mode-hook&set-captain-local-vars ()
  (setq-local captain-predicate #'always)
  (setq-local captain-sentence-start-function #'captain--default-sentence-start))

(oo-call-after-load 'captain #'require 'config-captain)
;;; provide
(provide 'init-captain-mode)
;;; init-captain-mode.el ends here
