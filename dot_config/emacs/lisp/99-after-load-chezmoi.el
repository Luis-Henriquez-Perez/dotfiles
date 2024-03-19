;;; 99-after-load-chezmoi.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: integrate with =defhook= once I figure out how that is going to work.
;; TODO: I need to figure out how to lazy require chezmoi and f.  I get the
;; error that the features have not been loaded.
(defhook! after-save-hook&chezmoi-maybe-write-file ()
  (set! file (expand-file-name (buffer-file-name)))
  (when (and file (member file (mapcar #'expand-file-name (chezmoi-managed))))
    (chezmoi-write file)))
;;; provide
(provide '99-after-load-chezmoi)
;;; 99-after-load-chezmoi.el ends here
