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
;; jumping to points.  I replace the default evil motions =w=, =e=, =E=, =W=
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
;;
;;; Code:
(require 'rx)
(require 'evil-easymotion)
;;;; evilem function
;; Apply.
;; (defun oo--evilem-fn (motion-fn)
;;   "Return"
;;   `(lambda () (interactive)))
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
(defemotion! oo-evilem-bow ()
  "Jump to the beginning of a word in the current visible buffer."
  :initial-point #'point-min
  :scope 'page
  (set! regexp "\\(?:\\`\\|[^[:word:]]+\\)\\([[:word:]]\\)[[:word:]]*")
  (save-match-data
    (when (re-search-forward regexp nil t nil)
      (goto-char (match-beginning 1)))))
;;;; beginning of WORD
(defemotion! oo-evilem-boW ()
  "Jump to the beginning of a WORD in the current visible buffer."
  :initial-point #'point-min
  :scope 'page
  (set! regexp "\\(?:\\`\\|[^[:word:]]+\\)\\([[:word:]]\\)[^[:blank:]]*")
  (save-match-data
    (when (re-search-forward regexp nil t nil)
      (goto-char (match-beginning 1)))))
;;;; end of word
(defemotion! oo-evilem-eow ()
  "Jump to the beginning of a word in the current visible buffer."
  :initial-point #'point-min
  :scope 'page
  ;; (set! regexp "[[:word:]]*\\([[:word:]]\\)")
  (set! regexp "[[:alnum:]]+")
  (save-match-data
    ;; I need to ensure that the regexp does not match the word on top of the
    ;; current point.
    ;; TODO: Fix capitalization rules, current at the top of the sentence above
    ;; was capitalized but should not be.
    (when (save-excursion (goto-char (1+ (point))) (re-search-forward regexp nil t nil))
      (goto-char (1- (match-end 0))))))
;;;; end of WORD
(defemotion! oo-evilem-eoW ()
  "Jump to the end of a WORD in the current visible buffer."
  :initial-point #'point-min
  :scope 'page
  (set! regexp "\\(?:\\`\\|[^[:blank:]]+\\)\\([[:word:]]\\)")
  (save-match-data
    (when (re-search-forward regexp nil t nil)
      (goto-char (match-beginning 1)))))
;;;; go to a parentheses
;; TODO: how to find current form?
(defemotion! oo-evilem-open-paren ()
  "Jump to opening parenthesis in current form."
  :initial-point #'point-min
  :scope 'page
  (set! regexp "(")
  (save-match-data
    (when (save-excursion (goto-char (1+ (point))) (re-search-forward regexp nil t nil))
      (goto-char (match-beginning 0)))))
;;;; beginning-of-line
(defemotion! oo-evilem-bol ()
  "Jump to the beginning of line in the current visible buffer."
  :initial-point #'point-min
  :scope 'page
  (interactive)
  (set! regexp "^[[:space:]]*\\(.\\)")
  (save-match-data
    (when (save-excursion (goto-char (1+ (point))) (re-search-forward regexp nil t nil))
      (goto-char (match-beginning 1)))))
;;;; a character
(defemotion! oo-evilem-char (char)
  "Jump to a character in current visible buffer."
  :bind ((char (read-char "Char: ")))
  :scope 'page
  :initial-point #'point-min
  (with! (save-match-data))
  (when (save-excursion (goto-char (1+ (point)))
                        (re-search-forward (rx-to-string (char-to-string char)) nil t nil))
    (goto-char (1- (match-end 0)))))
;;; provide
(provide '99-after-load-evil-easymotion)
;;; 99-after-load-evil-easymotion.el ends here
