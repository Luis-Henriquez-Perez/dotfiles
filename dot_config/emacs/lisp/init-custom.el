;;; init-custom.el --- initialize custom -*- lexical-binding: t; -*-
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
;; Initialize custom.
;;
;;; Code:
(require 'base)
;;;; don't ask me for permission to enable a theme
;; By default Emacs will ask you whether you are sure you want to enable a theme
;; as a precaution because a theme could contain malicious code.  Downloading
;; themes with elpaca is safe.  I don't make a habit of grabbing random themes
;; from wierd places online and evaluating them.  So I don't need.
(setq custom-safe-themes t)
;;;; don't create a custom file
;; I don't need it.  I'll be honest; to me it seems like the emacs's custom
;; interface is intended for people that don't know elisp.  For me it's completely
;; unnecessary.  Every variable I customize is in my emacs configuration.
(setq custom-file null-device)
;;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defun! oo--disable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(advice-add 'load-theme :around #'oo--disable-old-themes)
;;;; bindings
(bind! oo-toggle-map "r" #'oo-load-random-theme)
(bind! oo-toggle-map "t" #'load-theme)
;;;; Enable faces
;; By default, the function `custom-theme-set-faces' and `custom-set-faces' do
;; effect the variable `custom--inhibit-theme-enable' needs to be nil.  And you
;; will notice that even after customizing a themes faces the customization does
;; not persist.  This function addresses both of these issues ensuring that as
;; expected the faces are set immediately and that these changes persist even
;; after theme change.

(defvar oo-custom-faces-alist nil
  "An alist of faces to be applied.
Each element is of the form (theme . faces).")

;; The original function made advices but honestly I think it is better practice
;; to minimize the number of advices added for each function.
(defun oo-custom-set-faces (theme &rest faces)
  "Customize THEME with FACES.
Advise `enable-theme' with a function that customizes FACES when
THEME is enabled.  If THEME is already enabled, also applies
faces immediately."
  (declare (indent defun))
  (when (or (equal theme 'user) (member theme custom-enabled-themes))
    (let ((custom--inhibit-theme-enable nil))
      (apply #'custom-theme-set-faces theme faces)))
  (setf (alist-get theme oo-custom-faces-alist)
        (cl-union faces (alist-get theme oo-custom-faces-alist) :key #'car)))

(defun! oo-apply-custom-faces-a (orig-fn &rest args)
  "Apply any faces that need to be applied."
  (prog1 (apply orig-fn args)
    (for! ((theme . faces) oo-custom-faces-alist)
      (when (or (equal theme 'user) (member theme custom-enabled-themes))
        (info! "Applying faces for %s..." theme)
        (let ((custom--inhibit-theme-enable nil))
          (apply #'custom-theme-set-faces theme faces))))))

(advice-add 'enable-theme :around #'oo-apply-custom-faces-a)
;;; provide
(provide 'init-custom)
;;; init-custom.el ends here
