;;; init-lgr.el --- initialization for lgr -*- lexical-binding: t; -*-
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
;; Initialize lgr.
;;
;;; Code:
(require 'lgr)

;; TODO: figure out how to change the log format
;; I do not really utilize the logging enough yet because I need to understand
;; `lgr' more.  I considered removing the package, but I still got it to work.
;; And logging a little is better than nothing.
(defvar oo-lgr (block! (set! logger (lgr-get-logger "oo"))
                       (set! log-buffer (get-buffer-create "*lgr*"))
                       (lgr-add-appender logger (lgr-appender-buffer :buffer log-buffer)))
  "Object used for logging.")

(defmacro info! (msg &rest meta)
  `(lgr-info oo-lgr ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-lgr ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-lgr ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-lgr ,msg ,@meta))
;;; provide
(provide 'init-lgr)
;;; init-lgr.el ends here
