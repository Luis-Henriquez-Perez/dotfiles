;;; base-extra.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; Prevent =devaralias= from generating a warning
;; The built-in package `woman' overwrites the existing variable
;; `woman-topic-history' by aliasing it to `Man-topic-history' and emacs tells
;; you this by popping up a *Warnings* buffer.  This whole thing is probably
;; some bug.  So I do this to stop this whole thing from happening.
(defun! oo--suppress-woman-warning (orig-fn &rest args)
  (pcase args
    (`(woman-topic-history Man-topic-history . ,_)
     (advice-remove 'defvaralias #'oo--suppress-woman-warning))
    (_
     (apply orig-fn args))))

(advice-add 'defvaralias :around #'oo--suppress-woman-warning)
;;; provide
(provide 'base-extra)
;;; base-extra.el ends here
