;;; +workspace.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is my own workspace management system built on top of `burly' and
;; `tab-bar'.  As it stands, the aforementioned have the potential for workspace
;; management but they are not integrated together--or at least have a very
;; shallow integration in the form of `burly-tabs-mode'.  In this file I will
;; try to provide that integration.
;;
;; - Allow non-visible buffers to be a part of burly bookmarks.
;; Right now, burly only records the visible window buffers.
;;
;; - Create a default buffer that a new workspace with no buffers will start with.
;; This buffer will be an "untitled" buffer.
;;
;; - Use a `switch-to-buffer' variant that only considers the buffers in the workspace.
;; - Save workspaces at the end of the session.
;;
;;; Code:
;;;; reqs
(require 'tab-bar)
(require 'burly)
;;;; internal
(defun +workspace--make-bookmark ())
;; I might need a new data structure for the workspace that holds which buffers
;; belong to it.
;;;; commands
;; On a side-note I am thinking that I should associate tabs to their buffers
;; somehow and that I wished that burly could associate more buffers than what
;; are just visible in the frame.
(defun +workspace-new (name)
  "Create a new workspace."
  (interactive "sWorkspace name: ")
  (message "New workspace `%s'!" name)
  ;; 1. Clear current window configuration.
  ;; 2. Switch to untitled buffer.
  ;; 3. Create a burly bookmark for this.
  ;; This should be the default.
  ;; Prompt for workspace name.
  ;; (oo--make-untitled-buffer)
  ;; (burly-bookmark-windows)
  ;; (tab-bar-new-tab)
  ;; (tab-bar)
  )

(defun +workspace-show-tabs ()
  "Show workspaces as tabs.")

(defun +workspace-switch ()
  )

(defun +workspace-rename ()
  )

;; TODO: potentially kill the buffers corresponding to the workspace, maybe that
;; should be an option.
(defun +workspace-close ()
  ""
  (interactive)
  (burly-book)
  (tab-bar-close)
  (dolist (buffer (workspace-buffers))
	(kill-buffer buffer)))
;;; provide
(provide '+workspace)
;;; +workspace.el ends here
