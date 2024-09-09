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
        (todo   . " %(+org-agenda--overdue-string)")
        (tags   . " ")
        (search . " %i %-12:c")))

;; Always show the time grid even if I do not have anything scheduled or any
;; deadlines.
;; Also, increase the sequence of numbers shown on the grid.
(setq org-agenda-time-grid `((daily today)
                             ,(number-sequence 300 2300 200)
                             "-------------- "
                             "---------------"))

;; Do not use a special character for the block separator.  I like it to look
;; old-fashionish.
(setq org-agenda-block-separator ?-)
(setq org-agenda-skip-deadline-if-done t)
;;;; sorting entries
;;;;; helpers
(defun +org-agenda-call-at-entry (entry fn)
  "Call function from entry."
  (set! marker (get-text-property 0 'org-marker entry))
  (unless marker (error "Entry: %S" entry))
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (funcall fn)))
;;;;; setup sorting
;; I do not use timestamps, instead I have time-based IDs.
(setq org-agenda-cmp-user-defined #'+org-agenda-main-comparator)

(setq +org-agenda-comparators '(+org-agenda-overdue-comparator +org-agenda-tsid-comparator))

(defvar +org-agenda-comparators nil
  "Comparators used for sorting org agenda.")

(defun! +org-agenda-main-comparator (a b)
  "Return whether A should be ordered before B."
  (set! user-sorters +org-agenda-comparators)
  (while user-sorters
    (set! sorter (pop user-sorters))
    (set! result (funcall sorter a b))
    (unless (zerop result)
      (return! result)))
  0)
;;;;; tag comparator
(defun! +org-agenda-tag-comparator (a b)
  "Compare two entries A and B based on their tags."
  (set! tag-weights '(("blog" . 2) ("emacs" . 1)))
  (funcall (-on #'> #'+org-agenda--tag-weights) a b)
  (if (> (+org-agenda--tag-weights a)) -1 1)
  (cond ((> weights-a weights-b) 1)
        ((< weights-a weights-b) -1)
        (t 0)))
;;;;; overdue deadline comparator
(defmacro with-entry! (entry &rest body)
  (declare (indent 1))
  (cl-with-gensyms (e marker)
    `(let* ((,e ,entry)
            (,marker (get-text-property 0 'org-marker ,e)))
       (unless ,marker (error "Entry: %S" ,e))
       (with-current-buffer (marker-buffer ,marker)
         (goto-char (marker-position ,marker))
         (progn ,@body)))))

;; A task is overdue if the deadline of the task is past the current time.
(defun +org-overdue-entry-p (entry)
  "Return non-nil if entry is overdue."
  (with-entry! entry
    (aand (org-get-deadline-time (point))
          (< (float-time (time-subtract it (current-time))) 0))))

(defsubst oo-negative-p (number)
  "Return non-nil if NUMBER is negative."
  (< number 0))

(defsubst oo-positive-p (number)
  "Return non-nil if NUMBER is negative."
  (> number 0))

(defun +org-agenda--agenda-filter (entry)
  (if (and (get-text-property 0 'org-marker entry)
           (+org-overdue-entry-p entry))
      nil
    entry))

(defun! +org-agenda--overdue-string ()
  "Return string indicating deadline status."
  (set! now (current-time))
  (or (aand (org-get-deadline-time (point))
            (< (float-time (time-subtract it now)) 0)
            "OVERDUE\s")
      ""))

(defun! +org-agenda-overdue-comparator (a b)
  "Return 1 if A is more overdue than B.
Return -1 if B is more overdue than A."
  (set! now (current-time))
  (flet! deadline-time (entry)
    (+org-agenda-call-at-entry entry (lambda () (org-get-deadline-time (point)))))
  ;; This should be a positive number of seconds if we have not passed the deadline.
  (flet! overdue-time (entry)
    (awhen (deadline-time entry) (float-time (time-subtract it now))))
  ;; A negative number.
  ;; (make-timestamp)
  (set! diff-a (overdue-time a))
  (set! diff-b (overdue-time b))
  (cond ((not (or diff-a diff-b))
         0)
        ((and diff-a (not diff-b))
         1)
        ((and diff-b (not diff-a))
         -1)
        ;; When the deadline has not passed prefer the element closest to the
        ;; deadline--which is the lesser one.
        ((and (> diff-a 0) (> diff-b 0) (< diff-a diff-b))
         1)
        ((and (> diff-a 0) (> diff-b 0) (> diff-a diff-b))
         -1)
        ;; When either one of the deadlines has passed.
        ((and (< diff-a 0) (> diff-b 0))
         1)
        ((and (> diff-a 0) (< diff-b 0))
         -1)
        ;; Both of the deadlines have passed.
        ((and (< diff-a 0) (< diff-b 0) (< (abs a) (abs b)))
         1)
        ((and (< diff-a 0) (< diff-b 0) (> (abs a) (abs b)))
         -1)
        (t
         0)))
;;;;; ID comparator
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

(defun! +org-agenda-entry-id (entry)
  "Return ID corresponding to entry."
  (+org-agenda-call-at-entry entry #'org-id-get))

;; The sort function accepts two entries and by entries the manual means
;; propertized strings.  These strings have references to the headline it refers to.
(defun! +org-agenda-tsid-comparator (a b)
  "Compare two entries A and B based on their ID property to sort by oldest first."
  (if-let* ((time-a (with-entry! a (org-id-get)))
            (time-b (with-entry! b (org-id-get)))
            (id-a (+org-id-to-time time-a))
            (id-b (+org-id-to-time time-b)))
      (if (time-less-p id-a id-b) -1 1)
    0))
;;;;; dealing with composite tasks
(defun +org-agenda--filter-parents-with-undone-children (entry)
  (when (not (+org-agenda-call-at-entry entry #'+org-has-tasks-to-be-done))
    entry))

;; For now hard-code whether done.
(defun! +org-has-tasks-to-be-done ()
  "Return non-nil if current headline has any subtasks that need to be done."
  (interactive)
  (flet! not-done-p ()
    (aand (substring-no-properties (org-get-todo-state))
          (not (member it '("DONE" "CANCELLED")))))
  (save-excursion
    (when (org-goto-first-child)
      (when (not-done-p)
        (return! t))
      (while (org-goto-sibling)
        (when (not-done-p)
          (return! t))))))
;;;; views
;; The main view is the day view.
;; Overdue items are first, then items with high priority, then items with low
;; effort, and then items that were created first.
(defun +org-agenda-day-view ()
  "Day agenda."
  (interactive)
  (let ((org-agenda-custom-commands
         `(("_" "Daily Agenda"
            ((todo "TODO" ((org-agenda-overriding-header "\nTODO")
                           (org-agenda-sorting-strategy '(user-defined-down))
                           ;; (org-agenda-before-sorting-filter-function #'+org-agenda--filter-parents-with-undone-children)
                           (org-agenda-max-entries 5)))
             (agenda "" ((org-agenda-overriding-header "\nSchedule")
                         (org-agenda-start-on-weekday nil)
                         ;; Do not show overdue items in agenda.  Overdue items
                         ;; are displaed akwardly at the end of the agenda which
                         ;; is confusing because I am thinking they are at the
                         ;; end of the day.  ID rather have them in the TODO
                         ;; section or their own dedicated section.
                         (org-agenda-before-sorting-filter-function #'+org-agenda--agenda-filter)
                         (org-scheduled-past-days 0)
                         (org-deadline-warning-days 0)
                         (org-agenda-span 1)))
             (todo "DONE" ((org-agenda-overriding-header "\nDone")
                           (org-agenda-max-entries 5))))))))
    (org-agenda nil "_")))
;;; provide
(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
