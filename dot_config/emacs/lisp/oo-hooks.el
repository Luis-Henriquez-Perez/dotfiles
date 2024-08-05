;;; oo-init-hooks.el --- initial hooks -*- lexical-binding: t; -*-
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
;; This file contains all the "base" hooks that trigger all other hooks.  Or in
;; other words, the hooks that cannot go into "after-load" files.
;;
;;; Code:
(require 'base)
;;;; hooks
;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
;;;;; after-init-hook
(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)
;;;;; minibuffer
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defhook! increase-garbage-collection (minibuffer-setup-hook)
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  [:depth 10]
  (oo-record-value 'gc-cons-threshold)
  (oo-record-value 'gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(defhook! decrease-garbage-collection (minibuffer-exit-hook)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  [:depth 90]
  (oo-restore-value 'gc-cons-threshold)
  (oo-restore-value 'gc-cons-percentage))
;;;;; on-first-input-hook
(oo-add-hook 'on-first-input-hook #'minibuffer-depth-indicate-mode)
;;;;; emacs-lisp-mode-hook
;; The macros in my configuration are expanded during compilation thereby saving
;; time because they do not need to be expanded during startup.  The one caviat
;; is that since they are already expanded at runtime my emacs configuration
;; will have no knowledge of them.  The `oo-macros' file will not be loaded at
;; all.  And again this is great for reducing startup time but I still want the
;; macros to be defined when I am actually editing emacs-lisp.  Therefore, I
;; load the `oo-macros' file.
(defhook! require-macros (emacs-lisp-mode-hook)
  (require 'base-macros))
(defhook! enable-font-lock (emacs-lisp-mode-hook)
  "Add font lock keywords for definer macros."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
;;;;; prog-mode-hook
(oo-add-hook 'prog-mode-hook #'hs-minor-mode)
;; This outputs the message and causes a slight delay when opening a file in
;; prog-mode for the first time.
;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
(oo-add-hook 'prog-mode-hook #'auto-fill-mode)
;; TODO: figure out the best way to add these things.
(defhook! set-captain-local-vars (prog-mode-hook)
  (setq-local captain-predicate #'oo-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'oo--prog-mode-should-capitalize-p))
;;;;; text-mode-hook
(oo-add-hook 'text-mode-hook #'auto-fill-mode)
;;;;; garbage collection
(defun! oo-lower-garbage-collection ()
  "Lower garbage collection until it reaches default values."
  (cl-assert (zerop (% gc-cons-threshold (* 4 1024 1024))))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo-lower-garbage-collection)
    (cl-decf gc-cons-threshold (* 4 1024 1024))
    (cl-decf gc-cons-percentage 0.1)
    (cond ((= gc-cons-threshold (* 8 1024 1024))
           (setq gc-cons-percentage 0.4))
          (t
           (run-with-timer 5 nil #'oo-lower-garbage-collection)))))
;;; provide
(provide 'oo-init-hooks)
;;; oo-init-hooks.el ends here
