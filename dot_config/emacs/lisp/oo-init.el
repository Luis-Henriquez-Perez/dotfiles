;;; oo-init.el -*- lexical-binding: t; -*-
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
;; This file loads everything that needs to be evaluated immediately on startup.
;;
;;; Code:
;;;; requirements
(require 'base)
(require 'oo-init-keybindings)
;;;; hooks
;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
;;;;; on-first-input-hook
(hook! on-first-input-hook minibuffer-depth-indicate-mode)
;;;;; emacs-lisp-mode-hook
(defhook! oo-enable-elisp-font-lock-h (emacs-lisp-mode-hook)
  "Add font lock keywords for definer macros."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
;;;;; oo-override-map
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defhook! oo-make-intercept-map-h (evil-mode-hook)
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map override-global-map 'all t))
;;;;; emacs-startup-hook
(oo-call-after-load 'evil #'oo-call-after-load-functions)

(defhook! oo-init-after-load-functions-h (on-first-input-hook :depth 99)
  "Call `oo-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (oo-call-after-load-functions)
  (hook! after-load-functions oo-call-after-load-functions))
;;;;; load macros for init file
;; The macros in my configuration are expanded during compilation thereby saving
;; time because they do not need to be expanded during startup.  The one caviat
;; is that since they are already expanded at runtime my emacs configuration
;; will have no knowledge of them.  The `oo-macros' file will not be loaded at
;; all.  And again this is great for reducing startup time but I still want the
;; macros to be defined when I am actually editing emacs-lisp.  Therefore, I
;; load the `oo-macros' file.
(defhook! oo-require-macros-h (emacs-lisp-mode-hook)
  (require 'base-macros))
;;;;; minibuffer
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defhook! oo-increase-garbage-collection-h (minibuffer-setup-hook :depth 10)
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  (set-register :gc-cons-threshold gc-cons-threshold)
  (set-register :gc-cons-percentage gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(defhook! oo-decrease-garbage-collection-h (minibuffer-exit-hook :depth 90)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold (get-register :gc-cons-threshold))
  (setq gc-cons-percentage (get-register :gc-cons-percentage)))
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
;;;; set initial font
;; This is very basic font setting based on available faces.  I have seen much
;; more complex font setups like in minemacs (which probably got its from doom)
;; but for now this will do.
(defvar oo-default-fonts (list (font-spec :family "RecMonoDuotone Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "CaskaydiaCove Nerd Font Mono"
                                          :weight 'light
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "Mononoki Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "JetBrainsMono Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18))
  "List of fonts to check.")

(defun! oo-set-default-font-h ()
  "Set the default font based on available fonts."
  (dolist (font oo-default-fonts)
    (trace! "Checking whether %s font is available..." font)
    (awhen (find-font font)
      (info! "Setting font to...%s" it)
      (set-face-attribute 'default nil :font font)
      (done!)))
  (set! default-font (face-attribute 'default :family))
  (info! "Unable to set font to any in `oo-default-font-list', defaulting to `%s'." default-font))

(add-hook 'after-init-hook #'oo-set-default-font-h 80)
;;;; sort lines
(defun! oo-sort-elpaca-forms-h ()
  "Sort elpaca package forms in current buffer."
  (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (set! beg (point-min))
  (set! end (point-max))
  (save-excursion
    (goto-char beg)
    (when (re-search-forward rx end t nil)
      (sort-regexp-fields nil rx "\\1" (match-beginning 0) end))))

(defhook! oo-setup-auto-line-sorting-maybe-h (find-file-hook)
  "Setup auto line sorting for `init-elpaca'."
  (set! path "~/.local/share/chezmoi/dot_config/emacs/lisp/init-elpaca.el")
  (when (f-same-p (buffer-file-name) (f-full path))
    (info! "Setup auto-sorting for %s..." (f-base path))
    (hook! before-save-hook oo-sort-elpaca-forms-h :local t)))

(defun! oo-align-abbrev-forms-h ()
  "Align all abbrev forms in current buffer."
  (set! regexp "(define-abbrev\\(?1:\\s-+\\)\\S-+\\(?2:\\s-+\\)\".*?\"\\(?3:\\s-+\\)\".*?\"\\(?4:\\s-+\\)\\S-+\\(?5:\\s-+\\):enable-function\\(?6:\\s-+\\).+)")
  (set! rules `((rule1 . ((regexp . ,regexp) (group . (1 2 3 4 5 6))))))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward rx nil t nil)
      (shut-up (align (match-beginning 0) (point-max) nil rules)))))

(defhook! oo-setup-auto-alignment-maybe-h (find-file-hook)
  "Set up auto alignment for certain buffers."
  (set! path "~/.local/share/chezmoi/dot_config/emacs/lisp/+abbrev-plain-text-abbrevs.el")
  (when (f-same-p (buffer-file-name) (f-full path))
    (info! "Setup auto-aligning for %S..." (f-base path))
    (add-hook 'before-save-hook #'oo-align-abbrev-forms-h nil t)))
;;;; enable initial theme
(defhook! oo-load-initial-theme-h (after-init-hook)
  "Load `modus-operandi' theme."
  (load-theme 'modus-operandi :no-confirm nil))
;;;; start emacs server
;; This is so that if I need to use some sort of program to open a file, it will
;; use he running emacs daemon.
;; (unless (server-running-p) (server-start))
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
