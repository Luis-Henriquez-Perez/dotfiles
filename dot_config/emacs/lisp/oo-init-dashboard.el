;;; oo-init-dashboard.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'dashboard)

;; If I put dashboard configuration in its own.
(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(opt! dashboard-init-info #'oo-dashboard-init-info)

(opt! dashboard-banner-logo-title "Welcome!")

(opt! dashboard-set-footer nil)

(opt! dashboard-items nil)

(opt! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))

(opt! dashboard-center-content t)
;;; provide
(provide 'oo-init-dashboard)
;;; oo-init-dashboard.el ends here
