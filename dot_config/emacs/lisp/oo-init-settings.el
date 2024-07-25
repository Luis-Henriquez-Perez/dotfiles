;;; oo-init-settings.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This file contains settings for third-party packages.  Many emacsers and I in
;; the past divided package configuration into separate files, but the truth is
;; for most packages I just have to set some variables and I was beginning to
;; feel like it was not worth it.  I am learning the hard way that is do not
;; over-abstract; only make an abstraction when the situation calls for it.
;; Otherwise, you risk making maintaining code much more difficult than it has
;; to be.  Also it made less sense for me to do so considering many package
;; configurations involve only the setting of a few variables.
;;
;;; Code:
;;;; abbrev-mode
(hook! text-mode-hook&abbrev-mode)
;;;; helm
(oo-popup-at-bottom "\\*Helm")
(set! helm-candidate-number-limit 50)
;;;; captain-mode
(hook! prog-mode-hook&captain-mode)
(hook! text-mode-hook&captain-mode)
;;;; recentf
(hook! emacs-startup-hook&recentf-mode)
(hook! kill-emacs-hook&recentf-save-list)
(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
(oo-add-advice #'recentf-cleanup :around #'oo-funcall-silently)
(oo-add-advice #'recentf-save-list :around #'oo-funcall-silently)
(oo-add-advice #'recentf-mode :around #'oo-funcall-silently)
(setq recentf-filename-handlers '(file-truename))
;;;; eshell
(hook! eshell-mode-hook&abbrev-mode)
(hook! eshell-mode-hook&smartparens-mode)
(hook! eshell-mode-hook&eat-eshell-mode)
(oo-popup-at-bottom "\\*eshell")
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(opt! eshell-banner-message "")
(opt! eshell-highlight-prompt nil)
(opt! eshell-prompt-function 'epe-theme-lambda)
(opt! eshell-hist-ignoredups t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;; Eshell prints various messages about loading modules.  These messages
;; originate from the function [[][eshell-unload-all-modules]].  I would rather
;; not see these messages.
(oo-add-advice #'eshell-unload-all-modules :around #'oo-funcall-silently)
;; At first I thought the culprit was this function, but I was wrong.  The
;; printing comes from =eshell-mode=.  In any case, however, I silence it as
;; well.
(oo-add-advice #'eshell-mode :around #'oo-funcall-silently)
;; (require 'eshell-z)
;; (require 'eshell-up)
;;;; gcmh
(hook! emacs-startup-hook&gcmh-mode :depth 91)
(opt! gcmh-idle-delay 'auto)
(opt! gcmh-high-cons-threshold (* 8 1024 1024))
(opt! gcmh-low-cons-threshold (* 4 1024 1024))
;;;; ace-window
(opt! aw-swap-invert t)
(opt! aw-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxb")))
;;;; avy
(opt! avy-style 'pre)
(opt! avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))
(opt! avy-background nil)
(opt! avy-timeout-seconds 0.3)
;;;; evil
(hook! emacs-startup-hook&evil-mode)
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)
(opt! evil-move-cursor-back nil)
(opt! evil-move-beyond-eol nil)
(opt! evil-search-wrap nil)
(opt! evil-insert-state-cursor '((bar . 3) "chartreuse3"))
(opt! evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
(opt! evil-normal-state-cursor '(box "DarkGoldenrod2"))
(opt! evil-visual-state-cursor '((hollow) "dark gray"))
(opt! evil-operator-state-cursor '((hbar . 10) "hot pink"))
(opt! evil-replace-state-cursor '(box "chocolate"))
(opt! evil-motion-state-cursor '(box "plum3"))
;;;; denote
(opt! denote-directory "~/Documents/notes/")
(opt! denote-file-type 'text)
;;;; emms
(opt! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(opt! emms-directory (expand-file-name "emms/" oo-var-dir))

;; TODO: Make this dependent on whether mpv is installed.  And also figure out
;; how to do this lazily.  Ideally when I would intercept a "player-list is
;; empty" error and if I have mpv installed, add it and play the file.  I can do
;; this for `emms-play-file' but I need to check if to.
(opt! emms-player-list '(emms-player-mpv))
(oo-call-after-load 'emms #'require emms-player-mpv nil t)
;;;; vertico
(hook! on-first-input-hook&vertico-mode)
(opt! vertico-quick1 "asdfgh")
(opt! vertico-quick2 "jkluionm")
(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)
;;;; orderless
(opt! orderless-matching-styles '(orderless-initialism orderless-regexp))
;;;; consult
(opt! consult-preview-key nil)
(opt! consult-fontify-preserve nil)
;;;; corfu
(opt! corfu-quick1 "ajskdlghty")
(opt! corfu-quick2 "ajskdlghty")
;; TODO: make it so moving on a candidate if I press espace insert that candidate.
(opt! corfu-preview-current t)
(opt! corfu-preselect-first t)
(opt! corfu-quit-at-boundary nil)
(opt! corfu-auto t)
(opt! corfu-auto-delay 0.1)
(opt! corfu-auto-prefix 1)
(opt! corfu-bar-width 0)
;;;; dired
(hook! dired-mode-hook&dired-omit-mode)
;; By default hide details.
(hook! dired-mode-hook&dired-hide-details-mode)
(opt! dired-clean-confirm-killing-deleted-buffers nil)
(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)
;;;; dirvish
(opt! dirvish-use-mode-line nil)
(opt! dirvish-attributes '(file-size subtree-state))
(opt! dirvish-default-layout nil)
;;;; super-save
(hook! on-first-file-hook&super-save-mode)
;; The default auto-saving feature in emacs saves after a certain number of
;; characters are typed (see [[helpvar:auto-save-interval][auto-save-interval]]).  The problem is that if you're in
;; the middle of typing and you've just hit the number of characters that trigger a
;; save, you could experience a lag, particularly if you are dealing with a large
;; file being saved.  Instead of doing this, [[https://github.com/bbatsov/super-save][super-save]] saves buffers during idle
;; time and after certain commands like [[helpfn:switch-to-buffer][switch-to-buffer]] (see [[helpvar:super-save-triggers][super-save-triggers]]).
;; Note that this is the same strategy employed by [[id:c550f82a-9608-47e6-972b-eca460015e3c][idle-require]] to load packages.
;; Saving files like this reduces the likelihood of user delays.
(opt! super-save-auto-save-when-idle t)
;; Save after 5 seconds of idle time.
(opt! super-save-idle-duration 5)
;;;; lispyville
;; Do not bind any keys by default.
(hook! prog-mode-hook&lispyville-mode)
(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)
(opt! lispyville-key-theme nil)
;;;; savehist
(hook! on-first-input-hook&savehist-mode)
(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))
(opt! savehist-additional-variables (cl-adjoin 'register-alist savehist-additional-variables))
(defadvice! savehist-save@BFremove-kill-ring-properties (&rest _)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))
;;;; smartparens
(hook! text-mode-hook&turn-on-show-smartparens-mode)
(hook! text-mode-hook&smartparens-mode)
(hook! prog-mode-hook&smartparens-mode)
(hook! prog-mode-hook&turn-on-show-smartparens-mode)
(opt! sp-highlight-wrap-tag-overlay nil)
(opt! sp-highlight-pair-overlay nil)
(opt! sp-highlight-wrap-overlay nil)
(opt! sp-show-pair-delay 0.2)
(oo-call-after-load 'smartparens #'require 'smartparens-config)
;;;; which-key
(hook! emacs-startup-hook&which-key-mode)
(opt! which-key-sort-uppercase-first nil)
(opt! which-key-max-display-columns nil)
(opt! which-key-add-column-padding 1)
(opt! which-key-min-display-lines 1)
(opt! which-key-side-window-slot -10)
(opt! which-key-sort-order #'which-key-prefix-then-key-order)
(opt! which-key-popup-type 'side-window)
(opt! which-key-idle-delay 0.8)
;; (opt! line-spacing 3 :hook which-key-init-buffer-hook :local t)

(opt! which-key-show-transient-maps t)
(opt! which-key-show-operator-state-maps t)

(opt! which-key-show-prefix 'top)
;;;; window divider
(opt! window-divider-default-bottom-width 7)
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-places t)
;;;; hungry-delete
;; Leave one space in between instead of deleting everything.
(opt! hungry-delete-join-reluctantly t)
;;;; notmuch
(opt! notmuch-sort-oldest-first nil)
;;;; rx
;; (oo-call-after-load #'require 'oo-rx-definitions)
;;;; highlight-quoted
(hook! emacs-lisp-mode-hook&highlight-quoted-mode)
;;;; rainbow delimiters
(hook! prog-mode-hook&rainbow-delimiters-mode)
(hook! reb-mode-hook&rainbow-delimiters-mode)
;;;; aggressive-indent
(hook! emacs-lisp-mode-hook&aggressive-indent-mode)
;;;; org
;;;;; org-superstar
(hook! org-mode-hook&org-superstar-mode)
(opt! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))
(opt! org-superstar-leading-bullet ?\s)
(opt! org-superstar-special-todo-items t)
;;;;; org-appear
(hook! org-mode-hook&org-appear-mode)
(opt! org-appear-autolinks t)
;;;;; org-refile
(opt! org-refile-allow-creating-parent-nodes t)
;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(opt! org-refile-targets '((oo-directory-files :maxlevel . 10)))
(opt! org-outline-path-complete-in-steps nil)
(opt! org-refile-use-cache nil)
;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(opt! org-refile-use-outline-path 'file)
;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
;; TODO: Fix `oo-has-source-block-p' is not defined.
;; (opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;;;;; org-id
(opt! org-id-track-globally t)
(opt! org-id-locations-file (expand-file-name "org-id-locations" oo-data-dir))
;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(opt! org-id-method 'ts)
(opt! org-id-link-to-org-use-id t)
;;;;; org-src
(oo-popup-at-bottom "\\*Org Src")
(opt! org-edit-src-persistent-message nil)
;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))
;; (adjoin! org-src-lang-modes '("lua" . lua))
(opt! org-src-ask-before-returning-to-edit-buffer nil)
(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)
(opt! org-src-window-setup 'plain)
;;;;; org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")
(opt! org-archive-save-context-info nil)
(opt! org-archive-location (concat org-directory "archive.org::"))
(hook! org-insert-heading-hook&org-id-get-create)
;;;;; miscellaneous
(opt! org-src-fontify-natively t)
(opt! org-hide-emphasis-markers t)
;;;; outli
(hook! prog-mode-hook&outli-mode)
;; TODO: figure out how to make this a named advice.
(advice-add 'load-theme :after (lambda (&rest _) (outli-reset-all-faces)))
;;;; grugru
;; (oo-call-after-load #'require 'oo-grugru-definitions)
;;;; magit
(oo-call-after-load 'evil #'evil-magit-init)
(oo-popup-at-bottom "\\`magit")
;;;; idle-require
(oo-add-advice #'idle-require-load-next :around #'oo-funcall-silently)
;;; provide
(provide 'oo-init-settings)
;;; oo-init-settings.el ends here
