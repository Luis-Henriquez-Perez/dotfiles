;;; 98-init-features.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is initialization code all of the packages I use.  I have vacillated
;; between having them as separate files.  But to be honest I do not think
;; there's a point for doing so, particularly because many of these
;; configurations do not affect.
;;
;;; Code:
(require 'on)
;;;; core
;; These are fundamental, essential.  They should be moved into a more core
;; file.
;;;;; no-littering
(setq no-littering-etc-directory oo-etc-dir)
(setq no-littering-var-directory oo-var-dir)

(require 'no-littering)
;;;;; initial buffer choice
(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
              (get-buffer-create "*scratch*"))
    (lgr-info oo-lgr "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)
;;;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defadvice! load-theme@ARdisable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))
;;;;; enable default theme - modus-operandi
(defhook! after-init-hook&load-theme ()
  "Load `modus-operandi' theme."
  (when (display-graphic-p)
    (load-theme 'modus-operandi :no-confirm nil)))
;;;; hooks
;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
;;;;; on-first-file-hook
(oo-add-hook 'on-first-file-hook #'super-save-mode)
;;;;; on-first-input-hook
(oo-add-hook 'on-first-input-hook #'minibuffer-depth-indicate-mode)
(oo-add-hook 'on-first-input-hook #'vertico-mode)
(oo-add-hook 'on-first-input-hook #'savehist-mode)
;;;;; emacs-lisp-mode-hook
(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
;;;;; reb-mode-hook
(oo-add-hook 'reb-mode-hook #'rainbow-delimiters-mode)
;;;;; prog-mode-hook
(oo-add-hook 'prog-mode-hook #'hs-minor-mode)
;; This outputs the message and causes a slight delay when opening a file in
;; prog-mode for the first time.
;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)
(oo-add-hook 'prog-mode-hook 'auto-fill-mode)
(oo-add-hook 'prog-mode-hook #'abbrev-mode)
(oo-add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; (oo-add-hook 'prog-mode-hook #'corfu-mode)
(oo-add-hook 'prog-mode-hook #'outli-mode)
(oo-add-hook 'prog-mode-hook #'smartparens-mode)
(oo-add-hook 'prog-mode-hook #'turn-on-show-smartparens-mode)
(oo-add-hook 'prog-mode-hook #'lispyville-mode)
(oo-add-hook 'prog-mode-hook #'ws-butler-mode)
(oo-add-hook 'prog-mode-hook #'captain-mode)
(oo-add-hook 'prog-mode-hook #'orglink-mode)
;;;;; text-mode-hook
(oo-add-hook 'text-mode-hook #'visual-line-mode)
(oo-add-hook 'text-mode-hook #'auto-fill-mode)
(oo-add-hook 'text-mode-hook #'abbrev-mode)
(oo-add-hook 'text-mode-hook #'captain-mode)
(oo-add-hook 'text-mode-hook #'turn-on-show-smartparens-mode)
(oo-add-hook 'text-mode-hook #'smartparens-mode)
;;;;; after-init-hook
;; Don't load everything at once.
;; (oo-require-hook 'after-init-hook 'evil)
(defhook! after-init-hook&load-evil ()
  [:depth 10]
  (require 'evil nil t))
;; TODO: The display flickers when setting the initial theme.  Maybe this is
;; inevitable.  But maybe this has to do with me either disabling the previous
;; theme first or the order of setting the window-divider, or maybe I can
;; specify the default theme to load beforehand.  I need to play around with
;; settings and see if this flickering can be avoided.
(oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)
;;;;; emacs-startup-hook
(oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)
(oo-add-hook 'emacs-startup-hook #'evil-mode)
(oo-add-hook 'emacs-startup-hook #'which-key-mode)
(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)
(oo-add-hook 'emacs-startup-hook #'recentf-mode)
;;;;; html-mode
(oo-add-hook 'html-mode-hook #'emmet-mode)
;;;; feature-specific customization
;;;;; re-builder
;; By default, use `rx' syntax.  It is my preferred syntax.
(opt! reb-re-syntax 'rx)
;;;;; unknown
;; I like an indentation of 4 spaces; maybe I have gotten used to it with Python.
(opt! sgml-basic-offset 4)
;;;;; dabbrev
(opt! dabbrev-check-all-buffers nil)
;;;;; dashboard
(defhook! oo-initial-buffer-choice-hook&make-dashboard ()
  (when (require 'dashboard nil t)
    (aprog1 (get-buffer-create dashboard-buffer-name)
      (with-current-buffer it
        (dashboard-insert-startupify-lists)))))
;; If I put dashboard configuration in its own.
(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(opt! dashboard-init-info #'oo-dashboard-init-info)

(opt! dashboard-banner-logo-title "Welcome!")

(opt! dashboard-set-footer nil)

(opt! dashboard-items nil)

(opt! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))

(opt! dashboard-center-content t)
;;;;; frame
;; TODO: Figure out how to set this based on the color of the theme.
(defhook! after-init-hook&set-window-divider-face ()
  "Set the window divider face."
  [:depth 11]
  (set-face-foreground 'window-divider "black"))

(defadvice! load-theme@AFset-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

;; (add-hook 'emacs-startup-hook
;;           (lambda (&rest _) (oo-add-advice #'load-theme :after #'oo-set-window-divider-face)))

;; You can either use =right-only= to place window dividers on the right of each
;; window.  Or =bottom-only= to place them just on the bottom.
(opt! window-divider-default-places t)

;; The default value is 6.
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-bottom-width 7)
;;;; set initial font
(progn!
  (aset! (or (font-spec :name "Martian Mono Nerd Font"
                        :weight 'normal
                        :slant 'normal
                        :size 14)
             (font-spec :name "Nimbus Mono PS"
                        :weight 'normal
                        :slant 'normal
                        :size 15)
             (font-spec :name "Iosevka Comfy Wide"
                        :weight 'normal
                        :slant 'normal
                        :size 15)
             (font-spec :name "SpaceMono Nerd Font"
                        :weight 'normal
                        :slant 'normal
                        :size 15)
             (font-spec :name "iMWritingMono Nerd Font Mono"
                        :weight 'normal
                        :slant 'normal
                        :size 15)))
  (set-face-attribute 'default nil :font it))
;;;; load macros for init file
;; The macros in my configuration are expanded during compilation thereby saving
;; time because they do not need to be expanded during startup.  The one caviat
;; is that since they are already expanded at runtime my emacs configuration
;; will have no knowledge of them.  The `oo-macros' file will not be loaded at
;; all.  And again this is great for reducing startup time but I still want the
;; macros to be defined when I am actually editing emacs-lisp.  Therefore, I
;; load the `oo-macros' file.
(defhook! emacs-lisp-mode-hook&require-macros ()
  (require 'oo-macros))
;;;; custom functions
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (set! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;; provide
(provide '98-init-features)
;;; 98-init-features.el ends here
