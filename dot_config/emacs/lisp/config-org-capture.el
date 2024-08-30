;;; config-org-capture.el --- Configure org-capture -*- lexical-binding: t; -*-
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
;; Configure org-capture.
;;
;;; Code:
;;;; Requirements
(require 'doct)
(require 'org-ml)
;;;; disable header-line
(setq-hook! org-capture-mode-hook header-line-format nil)
;;;; enable evil-insert-state
(defhook! enter-evil-state (org-capture-mode-hook)
  "Do not show header line."
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))
;;;; use completing-read for org-capture
(defun! +org-capture-choose-template ()
  "Choose capture template to open."
  (interactive)
  (set! templates (--map (cons (-second-item it) (-first-item it)) org-capture-templates))
  (set! choosen (completing-read "Choose a Templates: " templates))
  (set! key (cdr (assoc choosen templates)))
  (if key
      (org-capture nil key)
    (error "No capture template corresponding to %S" choosen)))
;;;; org-capture-file
(defun! +org-capture-file ()
  "Return the file to be used for capturing.
Progressively try to see if a notes file exists if the current one is too big
make a new one."
  (f-full (f-expand "notes.org" org-directory)))
;;;; main capture template
(defun +org-capture-plain-template ()
  "Return capture template as a string."
  (require 'org-ml)
  (->> (org-ml-build-headline! :level 1 :title-text "%?")
       (org-ml-headline-set-node-property "ID" (org-id-new))
       (org-ml-to-string)))

(defun +org-capture-todo-template ()
  "Return the todo capture template as a string."
  (require 'org-ml)
  (->> (org-ml-build-headline! :level 1 :todo-keyword "TODO" :title-text "%?")
       (org-ml-headline-set-node-property "ID" (org-id-new))
       (org-ml-to-string)))

(defun +org-capture-bug-template ()
  "Return the bug capture template as a string."
  (require 'org-ml)
  (->> (org-ml-build-headline! :level 1 :todo-keyword "BUG" :title-text "%?")
       (org-ml-headline-set-node-property "ID" (org-id-new))
       (org-ml-to-string)))

(defun +org-capture-question-template ()
  "Return the bug capture template as a string."
  (require 'org-ml)
  (->> (org-ml-build-headline! :level 1 :todo-keyword "QUESTION" :title-text "%?")
       (org-ml-headline-set-node-property "ID" (org-id-new))
       (org-ml-to-string)))

(setq org-capture-templates
      (append (doct (list (list "todo"
                                :prepend t
                                :keys "t"
                                :file #'+org-capture-file
                                :template #'+org-capture-todo-template)))
              (doct (list (list "bug"
                                :prepend t
                                :keys "p"
                                :file #'+org-capture-file
                                :template #'+org-capture-plain-template)))
              (doct (list (list "question"
                                :prepend t
                                :keys "q"
                                :file #'+org-capture-file
                                :template #'+org-capture-question-template)))
              (doct (list (list "plain"
                                :prepend t
                                :keys "p"
                                :file #'+org-capture-file
                                :template #'+org-capture-plain-template)))))
;;; provide
(provide 'config-org-capture)
;;; config-org-capture.el ends here
