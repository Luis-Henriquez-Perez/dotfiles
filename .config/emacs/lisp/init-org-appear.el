;;; init-org-appear.el --- Initialize org-appear -*- lexical-binding: t; -*-
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
;; Initialize org-appear.
;;
;;; Code:
(set! org-appear-autoemphasis t)
(set! org-appear-autolink t)

(hook! org-mode-hook org-appear-mode)

(defhook! ensure-proper (org-appear-mode-hook)
  "Ensure proper symbols are set."
  (when (and org-appear-autoemphasis (not org-hide-emphasis-markers))
    (warn! "`%s' is non-nil when `%s' nil.")
    (setq org-hide-emphasis-markers t)
    (info! "Set %s to t" 'org-hide-emphasis-markers))
  (when (and org-appear-autolink (not org-link-descriptive))
    (warn! "`%s' is non-nil when `%s' is nil")
    (setq org-link-descriptive t)
    (info! "Set %s to t" 'org-hide-emphasis-markers)))
;;; provide
(provide 'init-org-appear)
;;; init-org-appear.el ends here
