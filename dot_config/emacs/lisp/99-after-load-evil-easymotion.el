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
;; TODO: add commentary
;;
;;; Code:
(require 'rx)

(evilem-make-motion oo-goto-beginning-of-word
                    (lambda nil
                      (interactive)
                      "Jump to beginning of word at current buffer." nil
                      (block!
                        (set! regexp
                              (rx
                               (or
                                (seq bol
                                     (1+ white))
                                (seq
                                 (1+ white))
                                (seq bol
                                     (1+ punct))
                                (seq
                                 (1+ white)
                                 (1+ punct))
                                (seq
                                 (1+ punct)))
                               word))
                        (save-match-data
                          (when
                              (re-search-forward regexp nil t nil)
                            (backward-char)))))
                    :initial-point
                    (function point-min)
                    :scope 'page)

(evilem-make-motion oo-goto-end-of-word
                    (lambda nil
                      (interactive)
                      "Jump to the end of word in the current buffer." nil
                      (block!
                        (set! regexp
                              (rx
                               (1+ alnum)))
                        (save-match-data
                          (awhen
                              (save-excursion
                                (goto-char
                                 (1+
                                  (point)))
                                (re-search-forward regexp nil t nil))
                            (goto-char
                             (1-
                              (match-end 0)))))))
                    :initial-point
                    (function point-min)
                    :scope 'page)

(evilem-make-motion oo-goto-char
                    (lambda nil
                      (interactive)
                      "Jump to the character in current buffer." nil
                      (block!
                        (with! (save-match-data))
                        (when
                            (save-excursion
                              (goto-char
                               (1+
                                (point)))
                              (re-search-forward
                               (rx-to-string
                                (char-to-string char))
                               nil t nil))
                          (goto-char
                           (1-
                            (match-end 0))))))
                    :bind
                    ((char
                      (read-char "Char: ")))
                    :scope 'page :initial-point
                    (function point-min))

;;; provide
(provide '99-after-load-evil-easymotion)
;;; 99-after-load-evil-easymotion.el ends here
