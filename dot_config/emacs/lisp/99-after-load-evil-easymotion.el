;;; 99-after-load-evil-easymotion.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is my configuration for `evil-easymotion'.  Here I define commands for
;; jumping to points.  replace the default evil motions =w=, =e=, =E=, =W=
;; with more useful counterparts.
;;
;; The README of `evil-easymotion' suggests that by default motions should
;; ignore overlays but that did not seem to be the case for me.  But I need to
;; figure out how to tell.  Thus far I have only been able to tell by the lag in
;; large org files.
;;
;; one important consideration is keyboard macros.  I do not think that choosing
;; with easymotion goes well with keyboard macros because the characters might
;; change positions..
;; Some thing I should keep track of is overlays.  If point is in an overlay,
;; get out of it.  Also I do not know if this is as useful in keyboard macros.
;;
;; Note that you will see the idiom (save-excursion (goto-char (1+ (point))))
;; often.  that is because.
;; TODO: I want to maybe make a macro for defining these functions but I do not
;; know if its worth.  The `evil-easymotion' could be simplified into one
;; "defun-like"
;; TODO: if the there is a word at the very beginning of the buffer this does
;; not match it because then it would not be able to advance to the next points.
;; as in, I would need to start at point -1 but that position does not exist.
;; It is not a massive deal because it is not often your at the top of the
;; window and there is a word right at the beginning of the buffer but I would
;; like to fix it.
;; I might need to provide another function to use for point collection.
;; TODO: captain does not work well in comments.  Captain uses the
;; `sentence-at-point' function I think so I just think that function does not
;; work in comments.
;;
;; TODO: Exclude the current point from the set because obviously I do not want
;; to jump to the place I am already at.
;;
;; TODO: find a function that returns the point at the top of the window (not
;; the top of the buffer).  I should be using that instead of `point-min'.
;;
;; Gotcha.
;; TODO: The reason that the letters are inconsistent is because the default
;; sorting function assigns values based on the /character distance/ not the
;; distance in words.  So if there is a long word.  What I need to do is sort by
;; word distance.
;;
;; 0. Remove the current point from points
;; 1. sort the points by distance from the main point
;; 2. separate the points into points less than origin and points greater than it
;; 3. interleave the points
;;
;; On another note, the `:point-collection' key for `evilem-make-motion' will
;; let me insert the point at the beginning of the buffer.  Alternatively, I
;; could specify another function that just gets that one point if it's at the
;; top of the window.  That function can also choose to sort the points so I can
;; sort them based on.
;; (defun oo--evilem-include-first-point (points)
;;   "Return"
;;   (if ())
;;   (cons ()))
;;
;;; Code:
(require 'rx)
(require 'evil-easymotion)
;;;; ignore overlay
;; TODO: abbrev that specifies only words isolated by spaces.
;; I got this idea from writing some function name suffixed with "-fn" and
;; getting the unwanted abbrev expansion.  Essentially, I want to tell abbrev if
;; there is a non-blank, do not expand it.  I only intended this abbrev to work
;; in isolation.  For things like abnormal hooks I can use text-completion.

;; (defun oo--left-bouned-by-space-p ())
;; (defun oo--skip-ov-fn (motion-fn)
;;   "Return MOTION-FUNCTION."
;;   (oo-before-fn motion-fn (-partial #'goto-char (end-of-overlay)))
;;   (oo-if-fn #'in-overlay-p it motion-function))
;;;; better sort function
(defun! oo--evilem-sort-by-match (points)
  "Return points sorted by match occurance.
This is as opposed to character length."
  (set! before (length points))
  (set! point (point))
  (set! points (-remove (-compose (-partial #'= point) #'car) points))
  (set! points (-sort (-on (-partial #'< point) #'car) points))
  (set! (less-than greater-than) (-separate (-on (-partial #'< point) #'car) points))
  ;; Interleave the points.  I really wish that I could use dash's `-interleave'
  ;; but if the interleaved lists are not the same size the extra values are
  ;; thrown away.
  (while (or less-than greater-than)
    (when less-than
      (pushing! interleaved (pop less-than)))
    (when greater-than
      (pushing! interleaved (pop greater-than))))
  (nreverse interleaved))
;;;; improve scope
;; Something similar is used in doom.
(put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
;;;; macro to simplify defining motions
;; It is a peeve of mine seeing excessive wordiness in defining these motions.
;; Instead of defining a helper function and then using it in the
;; `evil-make-motion' invocation, I would like to do it all at once.
(defmacro! defemotion! (name args &rest body)
  "Convenience macro for defining evil motions.
This is a wrapper around `evilem-make-motion'."
  (declare (indent defun))
  ;; TODO: use what I defined on base-lib for this.
  ;; Remove the keywords passed in after docstring.
  (when (stringp (car body))
    (set! docstring (pop body)))
  (while (keywordp (car body))
    (appending! map (list (pop body) (pop body))))
  `(evilem-make-motion ,name (lambda () ,docstring (interactive) ,@body) ,@map))
;;;; beginning of word
(defemotion! evilem-motion-beginning-of-word ()
  "Jump to the beginning of a word in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (set! regexp "\\(?:\\`\\|[^[:word:]]+\\)\\([[:word:]]\\)[[:word:]]*")
  (and (save-excursion (forward-char) (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 1))))

;; There is a bug with `evilem-make-motion' where the arguments are not in the
;; proper order.
;;;; beginning of WORD
(defemotion! evilem-motion-beginning-of-WORD ()
  "Jump to the beginning of a WORD in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (set! regexp "\\(?:\\`\\|[^[:word:]]*\\)\\([[:word:]]\\)[^[:space:]]*")
  (and (save-excursion (forward-char)
                       (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 1))))
;;;; end of word
(defemotion! evilem-motion-end-of-word ()
  "Jump to the beginning of a word in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (set! regexp "[[:alnum:]]+")
  ;; I need to ensure that the regexp does not match the word on top of the
  ;; current point.
  ;; TODO: Fix capitalization rules, current at the top of the sentence above
  ;; was capitalized but should not be.
  (and (save-excursion (forward-char)
                       (re-search-forward regexp nil t nil))
       (goto-char (1- (match-end 0)))))
;;;; end of WORD
(defemotion! evilem-motion-end-of-WORD ()
  "Jump to the end of a WORD in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (set! regexp "\\(?:\\`\\|[^[:blank:]]+\\)\\([[:word:]]\\)")
  (and (re-search-forward regexp nil t nil)
       (goto-char (match-beginning 1))))
;;;; go to a parentheses
;; TODO: how to find current form?
(defemotion! oo-evilem-open-paren ()
  "Jump to opening parenthesis in current form."
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (set! regexp "(")
  (and (save-excursion (forward-char) (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 0))))
;;;; beginning-of-line
;; TODO: screenshot the difference between this with and without the
;; postprocess.
(defemotion! evilem-motion-beginning-of-line ()
  "Jump to the beginning of line in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (interactive)
  (set! regexp "^[[:space:]]*\\(.\\)")
  (and (save-excursion (forward-char) (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 1))))
;;;; a character
;; TODO: I might be able to use `evil-find-char' for this one, but I might not
;; do it because this already works well.
(defemotion! evilem-motion-char (char)
  "Jump to a character in current visible buffer."
  :bind ((char (read-char "Char: ")))
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'oo--evilem-sort-by-match
  (and (save-excursion (forward-char)
                       (re-search-forward (rx-to-string (char-to-string char)) nil t nil))
       (goto-char (match-beginning 0))))
;;; provide
(provide '99-after-load-evil-easymotion)
;;; 99-after-load-evil-easymotion.el ends here
