;;; oo-after-load-org.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; org
;;;;; org-superstar
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(opt! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))

(opt! org-superstar-leading-bullet ?\s)

(opt! org-superstar-special-todo-items t)
;;;;; org-appear
(oo-add-hook 'org-mode-hook #'org-appear-mode)

(opt! org-appear-autolinks t)
;;;;; org-refile
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
;;;;; org-id
(opt! org-id-track-globally t)
(opt! org-id-locations-file (expand-file-name "org-id-locations" oo-data-dir))

;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(opt! org-id-method 'ts)

(opt! org-id-link-to-org-use-id t)
;;;;; org-src
(oo-popup-at-bottom "\\*Org Src")

(opt! org-edit-src-persistent-message nil)

;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))

;; (adjoin! org-src-lang-modes '("lua" . lua))

(opt! org-src-ask-before-returning-to-edit-buffer nil)

(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)

(opt! org-src-window-setup 'plain)

(bind! :alt org-edit-src-code oo-dwim-edit-src-code)
;; (bind! :h "," #'org-edit-src-code :localleader t)
;; (bind! org-mode-map "e" #'org-edit-src-code)
;; (bind! :h "es" #'org-edit-src-code :wk "source block" :localleader t)

;; (bind! org-src-mode-map "," #'org-edit-src-exit :localleader t :mode 'org-src-mode)
;; (bind! org-src-mode-map "a" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
;; (bind! org-src-mode-map "c" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
;;;;; org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")

(bind! oo-quick-map "j" #'org-capture :wk "capture")
(bind! oo-app-map "a" #'org-capture :wk "capture")
(bind! oo-app-map "j" #'org-capture :wk "capture")

(opt! org-archive-save-context-info nil)

(opt! org-archive-location (concat org-directory "archive.org::"))

(oo-add-hook 'org-insert-heading-hook #'org-id-get-create)
;;;;; miscellaneous
(opt! org-src-fontify-natively t)
(opt! org-hide-emphasis-markers t)
;;; provide
(provide 'oo-after-load-org)
;;; oo-after-load-org.el ends here
