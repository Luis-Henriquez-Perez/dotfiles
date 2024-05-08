;;; oo-after-load-chezmoi.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; chezmoi
;; First thing to do is trigger chezmoi commands via bindings.  One of the key
;; concepts needed with chezmoi is the concept of source state and target state.
;; Source state is the version-controlled file that chezmoi.  Target state is
;; the file that is written to the users filesystem to reflect.
;;;;; TODO maybe have a way to sync all files
;; I will be honest.  Sometimes I forget which target files I have edited and I
;; want to sync them to make sure to.
;;;;;;; TODO automatically use =chezmoi= to write files
;; I need the command to write the source from the target.  The command
;; =chezmoi-apply= does this but I would like it to do it automatically if I am
;; already editing a target-file.
(defhook! after-save-hook&chezmoi-write-maybe (&rest _)
  (when (aand (require 'chezmoi nil t)
              (buffer-file-name)
              (chezmoi-target-file it))
    (with-demoted-errors "error:%S" (chezmoi-write))))
;;; provide
(provide 'oo-after-load-chezmoi)
;;; oo-after-load-chezmoi.el ends here
