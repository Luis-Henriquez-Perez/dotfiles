;;; oo-init-hooks.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'oo-base)
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
(oo-add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
;;;;; reb-mode-hook
(oo-add-hook 'reb-mode-hook #'rainbow-delimiters-mode)
;;;;; prog-mode-hook
(oo-add-hook 'prog-mode-hook #'hs-minor-mode)
;; This outputs the message and causes a slight delay when opening a file in
;; prog-mode for the first time.
;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)
(oo-add-hook 'prog-mode-hook #'auto-fill-mode)
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
;; (defhook! emacs-startup-hook&enable-modeline ()
;;   [:depth 90]
;;   (require 'oo-init-modeline))
;;;;; html-mode
(oo-add-hook 'html-mode-hook #'emmet-mode)
;;;;; enable default theme - modus-operandi
(defhook! after-init-hook&load-theme ()
  "Load `modus-operandi' theme."
  (when (display-graphic-p)
    (load-theme 'modus-operandi :no-confirm nil)))
;;;;; load macros for init file
;; The macros in my configuration are expanded during compilation thereby saving
;; time because they do not need to be expanded during startup.  The one caviat
;; is that since they are already expanded at runtime my emacs configuration
;; will have no knowledge of them.  The `oo-macros' file will not be loaded at
;; all.  And again this is great for reducing startup time but I still want the
;; macros to be defined when I am actually editing emacs-lisp.  Therefore, I
;; load the `oo-macros' file.
(defhook! emacs-lisp-mode-hook&require-macros ()
  (require 'oo-base-macros))
;;;;; enable special font-lock
(defhook! emacs-lisp-mode-hook&enable-font-lock ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
;;;;; enable smartparens in the minibuffer
(defhook! minibuffer-setup-hook&enable-smartparens-maybe ()
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))
;;; provide
(provide 'oo-init-hooks)
;;; oo-init-hooks.el ends here
