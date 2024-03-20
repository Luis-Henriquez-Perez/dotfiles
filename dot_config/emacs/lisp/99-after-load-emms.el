;;; 99-after-load-emms.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; emms
(opt! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(opt! emms-directory (expand-file-name "emms/" oo-var-dir))

;; TODO: Make this dependent on whether mpv is installed.  And also figure out
;; how to do this lazily.  Ideally when I would intercept a "player-list is
;; empty" error and if I have mpv installed, add it and play the file.  I can do
;; this for `emms-play-file' but I need to check if to.
(opt! emms-player-list '(emms-player-mpv))
(require 'emms-player-mpv)

;; TODO: Add a way of setting the volume without having to repeat the lower
;; volume command, `emms-volume-lower'.
;;; provide
(provide '99-after-load-emms)
;;; 99-after-load-emms.el ends here
