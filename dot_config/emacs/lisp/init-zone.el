;;; init-zone.el --- initialization for zone -*- lexical-binding: t; -*-
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
;; Initialize `zone'.
;;
;;; Code:
;; (setq)
(require 'zone)

(require 'zone-sl)
;; https://www.emacswiki.org/emacs/ZoneMode
(defun +zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive (list (completing-read "Program: " (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (vector (intern pgm)))) (zone)))

(zone-select-add-program 'zone-pgm-sl)

(unless (memq 'zone-pgm-sl (append zone-programs nil))
  (setq zone-programs (vconcat zone-programs [zone-pgm-sl])))

(unless (memq 'zone-rainbow (append zone-programs nil))
  (setq zone-programs (vconcat zone-programs [zone-rainbow])))

(unless (memq 'zone-tmux-clock (append zone-programs nil))
  (setq zone-programs (vconcat zone-programs [zone-tmux-clock])))
;;; provide
(provide 'init-zone)
;;; init-zone.el ends here
