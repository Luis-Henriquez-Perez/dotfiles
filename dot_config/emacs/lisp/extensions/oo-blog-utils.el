;;; oo-blog-utils.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This library is for adding new blog posts.
;;
;;; Code:
(defun! oo--blog-post-filename (title)
  "Return a valid blog post name from TITLE.
TITLE is a string describing the topic of a post."
  (set! timestamp (format-time-string "%Y-%m-%d--%H-%M-%S--%N"))
  (set! name (downcase (string-replace "\s" "-" title)))
  (set! post-dir "~/Documents/blog/org/posts/")
  (expand-file-name (format "%s--%s.org" timestamp name) post-dir))

(defun oo-blog-new-post (title)
  "Create a new blog post."
  (interactive "sBlog post title: ")
  (find-file (oo--blog-post-filename title))
  (insert (format "#+TITLE: %s\n" (capitalize title)))
  (insert (format "#+AUTHOR: Luis Henriquez-Perez\n" (capitalize title))))
;;; provide
(provide 'oo-blog-utils)
;;; oo-blog-utils.el ends here
