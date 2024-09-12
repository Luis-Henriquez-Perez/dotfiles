;;; config-org.el --- Configure nil -*- lexical-binding: t; -*-
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
;; Configure nil.
;;
;;; Code:
;; https://emacs.stackexchange.com/questions/57677/how-to-set-effort-all-globally
(add-to-list 'org-modules 'org-habit :append)
;; (string-join (--map (format "0:%.2d" it) (number-sequence 5 55 5)) "\s")

(defun! +org-choose-tags ()
  "Choose tags to add to current headline.
If you choose a tag that is already in the current headline, remove it.  Any
tags that are not in the current headline are added to it.  The Resulting tags are
in alphabetical order."
  (interactive)
  (set! all (save-restriction (widen) (org-get-buffer-tags)))
  (set! current (-map #'substring-no-properties (org-get-tags (point) t)))
  (set! selected (completing-read-multiple "Choose tag: " all))
  (set! new (append (-difference selected current) (-difference current selected)))
  (org-set-tags (-sort #'string< new)))
;;; provide
(provide 'config-org)
;;; config-org.el ends here
