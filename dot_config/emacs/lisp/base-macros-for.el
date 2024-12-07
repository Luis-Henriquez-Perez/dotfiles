;;; base-macros-for.el -*- lexical-binding: t; -*-
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
;; Looping macros.
;;
;;; Code:
(require 'base-macros-let)
(require 'cl-lib)
(require 'seq)
;;;; for!
;; There is a huge question of whether to automatically wrap loops with
;; =autolet!=, but I decided to.
(defalias 'dolist! 'for!)
(defalias 'for! 'loop!)
(defmacro loop! (loop-struct &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
BODY is the body of the loop.  LOOP-STRUCT determines how `for!' loops and can
take the following forms:

- (repeat n)
Evaluate BODY N times where n is an integer equal to or greater than zero.

- (VAR NUMBER)
Same as `dotimes'.

- (MATCH-FORM SEQUENCE)
Evaluate BODY for every element in sequence.  MATCH-FORM is the same as in
`let!'."
  (declare (indent 1))
  (pcase loop-struct
    ((or (and (pred integerp) n) `(repeat ,n))
     `(dotimes (_ ,n) ,@body))
    (`(,(and match-form (or (pred listp) (pred vectorp))) ,list)
     (cl-with-gensyms (elt)
       `(for! (,elt ,list)
          (pcase-let* ,(oo--to-pcase-let match-form elt)
            ,@body))))
    (`(,(and elt (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
               (dolist (,elt ,list) ,@body))
              ((sequencep ,list)
               (seq-doseq (,elt ,list) ,@body))
              ((integerp ,list)
               (dotimes (,elt ,list) ,@body))
              (t
               (error "Unknown list predicate: %S" ',loop-struct)))))))
;;; provide
(provide 'base-macros-for)
;;; base-macros-for.el ends here
