;;; oo-init-dashboard.el --- dashboard configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for dashboard.
;;
;;; Code:
(require 'dashboard)

;; If I put dashboard configuration in its own.
(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(setq dashboard-init-info #'oo-dashboard-init-info)

(setq dashboard-banner-logo-title "Welcome!")

(setq dashboard-startupify-list (-difference dashboard-startupify-list '(dashboard-insert-items dashboard-insert-footer)))

(setq dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))

(setq dashboard-center-content t)
;;; provide
(provide 'oo-init-dashboard)
;;; oo-init-dashboard.el ends here
