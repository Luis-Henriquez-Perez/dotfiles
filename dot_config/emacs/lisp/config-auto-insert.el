;;; config-auto-insert.el --- Configure auto-insert -*- lexical-binding: t; -*-
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
;; Configure auto-insert.
;;
;;; Code:
(require 'base)

(defun! oo-auto-insert-elisp-template ()
  "Insert emacs-lisp template in file."
  (set! filename (buffer-file-name))
  (set! name (f-base filename))
  (when (f-child-of-p filename "~/.local/share/chezmoi/dot_config/emacs/lisp/")
    (cond ((string-match-p "\\`init-\\(.+\\)\\'" name)
           (set! feature (match-string 1 name))
           (set! comment1 (format "Initialize %s" feature))
           (set! comment2 (format "Initialize %s." feature)))
          ((string-match-p "\\`config-\\(.+\\)\\'" name)
           (set! feature (match-string 1 name))
           (set! comment1 (format "Configure %s" feature))
           (set! comment2 (format "Configure %s." feature))))
    (oo--ensure-file-header)
    (goto-char (point-min))
    ;; This is a kind of roundabout way of doing it.  Not sure if it is the
    ;; "best" way whatever that means, but it works.
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment1)
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment2)
    (save-excursion (oo--ensure-provide filename))))

(defun! oo-auto-insert-html-template ()
  "Insert html template in file."
  (interactive)
  (require 'tempel)
  (tempel-insert '("<!doctype html>" n
                   "<html lang=\"en\">" > n
                   "<head>" > n
                   "<meta charset=\"UTF-8\"/>" > n
                   "<title>" p "</title>" > n
                   "</head>" > n
                   "<body>" > n
                   "</body>" > n
                   "</html>"))
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))

(defun! oo-auto-insert-python-file-header ()
  "Insert python file header."
  (require 'tempel)
  (tempel-insert '("# Filename: " (f-filename (buffer-file-name)) > n
                   "# Author: " user-full-name " <" user-mail-address ">" > n
                   "# Created: " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                   "# Description: " p > n))
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))
;;; provide
(provide 'config-auto-insert)
;;; config-auto-insert.el ends here
