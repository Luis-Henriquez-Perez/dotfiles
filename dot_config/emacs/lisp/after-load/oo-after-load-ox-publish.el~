;;; 99-after-load-ox-publish.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This stores the configuration for my blog.  As I am writing this I am
;; thinking that maybe this should not go here but I do not know where else to
;; put it.  I guess it should be an extension that requires `ox-publish'.  Yeah
;; but this should not be an "after load" file.
;;
;;; Code:
(setq org-publish-project-alist
      (progn!
        (set! blog-dir (expand-file-name "blog" "~/Documents/"))
        (set! base-dir (expand-file-name "posts" blog-dir))
        (set! publish-dir (expand-file-name "html" blog-dir))
        `(("pages"
           ;; This is where the source files are.
           :base-directory ,base-dir
           :base-extension "org"
           ;; This is where the exported html files will go.
           :publishing-directory ,publish-dir
           :recursive t
           :publishing-function org-html-publish-to-html
           :section-numbers nil)
          ("static"
           :base-directory ,base-dir
           :base-extension "css\\|txt\\|jpg\\|gif\\|png"
           :recursive t
           :publishing-directory ,publish-dir
           :publishing-function org-publish-attachment)
          ("blog"
           :components ("pages" "static")))))
;;; provide
(provide '99-after-load-ox-publish)
;;; 99-after-load-ox-publish.el ends here
