;;; init-org.el --- initialize org -*- lexical-binding: t; -*-
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
;; Initialize org.
;;
;;; Code:
;;;; requirements
(require 'base)
;;;; general
(opt! org-default-notes-file (make-temp-file "emacs-org-notes-"))
(opt! org-directory (f-full "~/Documents/org/"))
(opt! org-agenda-files (directory-files org-directory t "\\.org\\'"))
(opt! org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "ON-HOLD(h@)" "|"
                                    "DONE(d)" "CANCELLED(c)")
                          (sequence "BUG" "FIXED")
                          (sequence "BUY" "BOUGHT")))
(opt! org-src-fontify-natively t)
(opt! org-hide-emphasis-markers t)
(opt! org-log-done 'note)
(opt! org-priority-lowest ?F)
(opt! org-priority-highest ?A)
(opt! org-default-priority ?C)
(opt! org-enforce-todo-dependencies t)
(opt! org-tags-column 0)
(opt! org-archive-location (alet (f-expand "archive.org" org-directory)
                             (format "%s::" it)))
(opt! org-archive-mark-done t)
(opt! org-global-properties `(("Effort_ALL"
                               ,(string-join (-map (-partial #'format "0:%.2d")
                                                   (number-sequence 5 55 5))
                                             "\s"))))

(require! config-org)
;;;; keybindings
(bind! n org-mode-map "T" #'org-todo)
(bind! n org-mode-map "t" #'+org-choose-tags)
;;;; org-agenda
(require! config-org-agenda)
(autoload #'+org-agenda-day-view "config-org-agenda" nil t nil)
(bind! oo-leader-map ";" #'+org-agenda-day-view)
;;;; org-capture
(autoload #'+org-capture-plain "config-org-capture" nil t 'function)
(autoload #'+org-capture-todo "config-org-capture" nil t 'function)
(autoload #'+org-capture-open "config-org-capture" nil t 'function)
(autoload #'+org-capture-question "config-org-capture" nil t 'function)
(autoload #'+org-capture-bug "config-org-capture" nil t 'function)
(autoload #'+org-capture-choose-template "config-org-capture" nil t 'function)

(bind! oo-app-map "c" #'org-capture)
(bind! oo-app-map "a c" #'org-capture)
(bind! oo-app-map "a a" #'+org-capture-plain)
(bind! oo-app-map "a p" #'+org-capture-plain)
(bind! oo-app-map "a s" #'+org-capture-todo)
(bind! oo-app-map "a j" #'+org-capture-todo)
(bind! oo-app-map "a t" #'+org-capture-todo)
(bind! oo-app-map "a k" #'+org-capture-bug)
(bind! oo-app-map "a l" #'+org-capture-open)
(bind! oo-app-map "a o" #'+org-capture-open)
(bind! oo-app-map "a ;" #'+org-capture-question)
(bind! oo-app-map "a q" #'+org-capture-question)

(alt! org-capture +org-capture-choose-template org-capture)
(require! config-org-capture)
;;;; org-refile
(opt! org-refile-allow-creating-parent-nodes t)
;; The variable =org-refile-targets= specifies the places from which information
;; is taken to create the list of possible refile targets.  So, for example,
(opt! org-refile-targets '((oo-directory-files :maxlevel . 10)))
(opt! org-outline-path-complete-in-steps nil)
(opt! org-refile-use-cache nil)
;; Without this setting, you can't actually refile to a generic file with
;; refiling; you can only refile to existing headings within that file.  The way
;; I use refiling, I'm refiling to files most of the time.
(opt! org-refile-use-outline-path 'file)
;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
;; TODO: Fix `oo-has-source-block-p' is not defined.
;; (opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;;;; org-src
(oo-popup-at-bottom "\\*Org Src")
(opt! org-edit-src-persistent-message nil)
;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))
;; (adjoin! org-src-lang-modes '("lua" . lua))
(opt! org-src-ask-before-returning-to-edit-buffer nil)
(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)
(opt! org-src-window-setup 'plain)
;;;; org-superstar
(hook! org-mode-hook org-superstar-mode)

(opt! org-superstar-leading-bullet ?\s)
(opt! org-superstar-special-todo-items t)
;;; provide
(provide 'init-org)
;;; init-org.el ends here
