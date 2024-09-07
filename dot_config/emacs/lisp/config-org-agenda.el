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
      '((agenda . " %i %?-12t")
        ;; Remove the category displayed on the left of entries.
        (todo   . " ")
        (tags   . " ")
        (search . " %i %-12:c")))

;; Always show the time grid even if I do not have anything scheduled or any
;; deadlines.
;; Also, increase the sequence of numbers shown on the grid.
(setq org-agenda-time-grid `((daily today)
                             ,(number-sequence 300 2300 100)
                             "-------------- "
                             "---------------"))

;; Do not use a special character for the block separator.
(setq org-agenda-block-separator ?-)
;;;; views
(defun +org-agenda-day-view ()
  "Day agenda."
  (interactive)
  (let ((org-agenda-custom-commands
         `(("_" "Daily Agenda"
            ((todo "TODO" ((org-agenda-overriding-header "\nTasks")
                           (org-agenda-sorting-strategy '(priority-down user-defined-up))
                           (org-agenda-max-entries 5)))
             (agenda "" ((org-agenda-overriding-header "\nSchedule")
                         (org-agenda-start-on-weekday nil)
                         (org-scheduled-past-days 0)
                         (org-deadline-warning-days 0)
                         (org-agenda-span 1))))))))
    (org-agenda nil "_")))

(defun +org-agenda-week-view ()
  "Week view."
  (interactive)
  (let ((org-agenda-custom-commands
         `(("_" "Week Agenda"
            ((agenda "" ((org-agenda-overriding-header "\nSchedule")
                         (org-agenda-start-on-weekday nil)
                         (org-scheduled-past-days 0)
                         (org-deadline-warning-days 0)
                         (org-agenda-span 3))))))))
    (org-agenda nil "_")))
;;;; sorting entries
;; I am ignoring the microseconds.  I should not be making capture templates
;; within microseconds of each other.  I got this function from chatgpt.
(defun +org-id-to-time (org-id)
  "Convert an Org ID string into an Emacs time object.
ORG-ID should be in the format 'YYYYMMDDTHHMMSS.SSSSSS'."
  (let* ((date-str (substring org-id 0 8))
         (time-str (substring org-id 9 15))
         (microseconds-str (substring org-id 16))
         (year (string-to-number (substring date-str 0 4)))
         (month (string-to-number (substring date-str 4 6)))
         (day (string-to-number (substring date-str 6 8)))
         (hour (string-to-number (substring time-str 0 2)))
         (minute (string-to-number (substring time-str 2 4)))
         (second (string-to-number (substring time-str 4 6))))
    (encode-time second minute hour day month year)))

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
  (set! id-a (+org-id-to-time (+org-agenda-entry-id a)))
  (set! id-b (+org-id-to-time (+org-agenda-entry-id b)))
  (if (time-less-p id-a id-b) -1 1))
;;; provide
(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
