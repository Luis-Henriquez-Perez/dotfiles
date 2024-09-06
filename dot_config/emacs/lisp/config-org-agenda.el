;;; config-org-agenda.el --- Configure nil -*- lexical-binding: t; -*-
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
;; Configure nil.
;;
;;; Code:
;;;; requirements
(require 'org-agenda)
;;;; settings
(setq org-agenda-files `(,org-directory))
(setq org-agenda-span 'week)
(setq org-agenda-start-on-weekday 1)  ; Monday
(setq org-agenda-confirm-kill t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-show-outline-path nil)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        ;; Remove the category displayed on the left of entries.
        (todo   . " ")
        (tags   . " ")
        (search . " %i %-12:c")))
;;;; views
(defun +org-agenda-main-view ()
  "Main agenda."
  (interactive)
  (let ((org-agenda-custom-commands
         `(("_" "Daily agenda and top priority tasks"
            ,(+org-agenda-main-commands)))))
    (org-agenda nil "_")))

(defun +org-agenda-emacs-view ()
  "Emacs agenda."
  (interactive)
  (let ((org-agenda-custom-commands
         `(("_" "Daily agenda and top priority tasks"
            ,(+org-agenda-emacs-commands)))))
    (org-agenda nil "_")))
;;;; sorting entries
;; I do not use timestamps, instead I have time-based IDs.
(setq org-agenda-cmp-user-defined #'+org-agenda-sort-by-id)

(defun! +org-agenda-entry-id (entry)
  "Return ID corresponding to entry."
  (set! marker (get-text-property 0 'org-marker entry))
  (unless marker (error "Entry: %S" entry))
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (set! id (org-id-get)))
  id)

;; The sort function accepts two entries and by entries the manual means
;; propertized strings.  These strings have references to the headline it refers to.
(defun! +org-agenda-sort-by-id (a b)
  "Compare two entries A and B based on their ID property to sort by oldest first."
  (set! id-a (+org-agenda-entry-id a))
  (set! id-b (+org-agenda-entry-id b))
  (if (string< id-a id-b) -1 1))
;;;; uncategorized
(defun! +org-agenda-main-commands ()
  "Custom agenda for use in `org-agenda-custom-commands'."
  (set! sorting-strategy '(deadline-up user-defined-up))
  `((todo "TODO" ((org-agenda-overriding-header "\nTasks")
                  (org-agenda-sorting-strategy ',sorting-strategy)
                  (org-agenda-max-entries 5)))
    (agenda "" ((org-agenda-overriding-header "\nSchedule")
                (org-agenda-start-on-weekday nil)
                (org-scheduled-past-days 0)
                (org-deadline-warning-days 0)
                (org-agenda-span 1)))))

(defun! +org-agenda-emacs-commands ()
  "Custom agenda for use in `org-agenda-custom-commands'."
  (set! sorting-strategy '(deadline-up user-defined-up))
  `((tags-todo "emacs" ((org-agenda-overriding-header "\nEmacs Tasks")
                        (org-agenda-sorting-strategy ',sorting-strategy)
                        (org-agenda-max-entries 5)))))
;;; provide
(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
