;;; init-org-refile.el --- initialize org-refile -*- lexical-binding: t; -*-
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
;; Initialize org-refile.
;;
;;; Code:
(require 'base)

(opt! org-refile-allow-creating-parent-nodes t)
;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(opt! org-refile-targets '((oo-directory-files :maxlevel . 10)))
(opt! org-outline-path-complete-in-steps nil)
(opt! org-refile-use-cache nil)
;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(opt! org-refile-use-outline-path 'file)
;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
;; TODO: Fix `oo-has-source-block-p' is not defined.
;; (opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;;; provide
(provide 'init-org-refile)
;;; init-org-refile.el ends here
