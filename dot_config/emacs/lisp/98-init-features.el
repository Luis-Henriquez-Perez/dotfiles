;;; 98-init-features.el --- initialize features  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 14 Feb 2024
;;
;; License: GPLv3
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
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
;;; feature-specific customization
;;;; unknown
;; I like an indentation of 4 spaces; maybe I have gotten used to it with Python.
(opt! sgml-basic-offset 4)
;;;; abbrev
;; Do not write/read abbrevs from a file.  I would rather just evaluate them
;; every time with `20-config-abbrev.el'.  In other words, do not make abbrevs
;; stateful.
(advice-add #'read-abbrev-file :around #'ignore)
(advice-add #'write-abbrev-file :around #'ignore)
(advice-add #'abbrev--possibly-save :around #'ignore)
;;;; ace-window
;;;;; swap
(opt! aw-swap-invert t)
;;;;; set the keys used by ace-window
;; The character z conflicts.
(opt! aw-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxb")))
;;;;; select a window with =w=, =j= or =o=
;; There are commands such as.  I do not need these commands.  After moving left,
;; right, up or down some direction once, the effort needed to traverse a window
;; using directional window movement commands greatly increases.  The command
;; [[file:snapshots/_helpful_command__ace-window][ace-window]] in contrast scales really well with a greater number of
;; windows.  And it only loses slightly to directional window commands when moving
;; one time.

;; The command [[file:snapshots/_helpful_command__ace-window_.png][ace-window]] leverages [[https://github.com/abo-abo/avy][avy]] to select a window.  It assigns each window
;; a character (I'm using [[][letters]] close to the homerow) which it displays on
;; the upper right-hand corner of windows. I've found that
;; ace-window is the quickest way possible to switch focus form one live window to
;; another.

;; The mnemonic bind is =w= and the quick bindings--which I will likely use most
;; often--are =o= and =j=.
(oo-bind 'oo-window-map "w" #'ace-window :wk "select")
(oo-bind 'oo-window-map "j" #'ace-window :wk "select")
(oo-bind 'oo-window-map "o" #'ace-window :wk "select")
;;;;; swap two windows with =s=
;; Often when I want to switch focus from my main window to one of its
;; subsidiaries; I will want to swap buffers from the two windows.
;; Actually, =edwina= does provide functions to do this: namely
;; [[_helpful_command__edwina-swap-next-window_.png][edwina-swap-next-window]] and [[file:snapshots/_helpful_command__edwina-swap-previous-window_.png][edwina-swap-previous-window]].
;; But I can do something similar, but much faster with.  This is a case where =s= is
;; mnemonic and easy to press.
(oo-bind 'oo-window-map "s" #'ace-swap-window :wk "swap")
;;;; avy
(opt! avy-style 'pre)

(opt! avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

(opt! avy-background nil)

(opt! avy-timeout-seconds 0.3)
;;;; benchmark-init
(require 'benchmark-init)

(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(oo-add-hook 'after-init-hook 'benchmark-init/deactivate)
;;;; burly
;;;;; leader bindings
(oo-bind 'oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-find-map "j" #'burly-open-bookmark)
;;;;; save window configuration with =b= or =S=
;; The command [[file:snapshots/_helpful_command__burly-bookmark-windows_.png][burly-bookmark-windows]] creates a bookmark with the information
;; necessary to reproduce the current window configuration.  I can then restore the
;; window information I've bookmarked with [[file:snapshots/_helpful_command__burly-open-bookmark_.png][burly]].
(oo-bind 'oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind 'oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
;;;; captain
(oo-add-hook 'prog-mode-hook #'captain-mode)
(oo-add-hook 'text-mode-hook #'captain-mode)
;;;; chezmoi
;; First thing to do is trigger chezmoi commands via bindings.  One of the key
;; concepts needed with chezmoi is the concept of source state and target state.
;; Source state is the version-controlled file that chezmoi.  Target state is
;; the file that is written to the users filesystem to reflect.
;;;;; create a leader map for dotfile actions
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(oo-bind 'oo-leader-map "d" #'oo-dotfile-prefix-command :wk "dotfiles")

;; TODO: oo-bind should already do this for me.
(autoload #'chezmoi-find "chezmoi")
(autoload #'chezmoi-write "chezmoi")
(autoload #'chezmoi-open-other "chezmoi")
(oo-bind 'oo-dotfile-map "f" #'chezmoi-find)
;; I use the command =chezmoi-write= the most so far.  It syncs the current file
;; with its corresponding chezmoi file.  If called while in the target file, it
;; applies the changes in the target file to the source file and vice versa.
;; Only caveat is that if there is a more recent change in the "other" file,
;; then you have to use a prefix command to make sure you want to override those
;; changes.
(oo-bind 'oo-dotfile-map "w" #'chezmoi-write)
;; Binding to the "w" key is the more BLANK choice but "d" is closer to the
;; homerow for QWERTY keyboards.
(oo-bind 'oo-dotfile-map "d" #'chezmoi-write)
;; The command =chezmoi-open-other= is also useful.  Similar to =chezmoi-find=
;; it does something different depending on whether your in the source file or
;; the target file.  If you are in the source file, you open the target file and
;; vice versa.
(oo-bind 'oo-dotfile-map "o" #'chezmoi-open-other)
;;;;; TODO maybe have a way to sync all files
;; I will be honest.  Sometimes I forget which target files I have edited and I
;; want to sync them to make sure to.
;;;;;;; TODO automatically use =chezmoi= to write files
;; I need the command to write the source from the target.  The command
;; =chezmoi-apply= does this but I would like it to do it automatically if I am
;; already editing a target-file.

;; (add-hook 'after-save-hook #'oo-chezmoi-maybe-write-file)
;; Additionally I may do some auto-commit stuff.  Who knows?
;;;; consult
(opt! consult-preview-key nil)

(opt! consult-fontify-preserve nil)

(defun! oo-display-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'display-buffer)
  (call-interactively #'consult-buffer))

(oo-bind :alt #'display-buffer #'oo-display-buffer)

(oo-bind 'oo-find-map "k" #'consult-bookmark :wk "bookmark")
(oo-bind 'oo-find-map "b" #'consult-bookmark :wk "bookmark")

(oo-bind 'oo-find-map "s" #'consult-line :wk "line")
(oo-bind 'oo-find-map "l" #'consult-line :wk "line")

(oo-bind 'oo-find-map "h" #'consult-outline :wk "outline")
(oo-bind 'oo-find-map "h" #'consult-org-heading :wk "heading")

(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)

(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)

(oo-bind 'oo-miscellany-map "l" #'consult-bookmark)
;;;; corfu
(oo-bind 'corfu-map "<tab>" #'corfu-next)
(oo-bind 'corfu-map [backtab] #'corfu-previous)
(oo-bind 'corfu-map "C-;" #'corfu-quick-complete)
(oo-bind 'corfu-map "C-j" #'corfu-next)
(oo-bind 'corfu-map "M-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-p" #'corfu-previous)

(opt! corfu-quick1 "ajskdlghty")
(opt! corfu-quick2 "ajskdlghty")

(oo-add-hook 'prog-mode-hook #'corfu-mode)
(oo-add-hook 'corfu-mode-hook #'corfu-history-mode)

(opt! corfu-quit-at-boundary nil)

(opt! corfu-auto t)

(opt! corfu-auto-delay 0.1)

(opt! corfu-auto-prefix 1)

(opt! corfu-bar-width 0)
;;;; dashboard
(defhook! oo-initial-buffer-choice-hook&make-dashboard ()
  (when (require 'dashboard nil t)
    (aprog1 (get-buffer-create dashboard-buffer-name)
      (with-current-buffer it
        (dashboard-insert-startupify-lists)))))

(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(opt! dashboard-init-info #'oo-dashboard-init-info)

(opt! dashboard-banner-logo-title "Welcome!")

(opt! dashboard-set-footer nil)

(opt! dashboard-items nil)

(opt! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))

(opt! dashboard-center-content t)
;;;; denote
(opt! denote-file-type 'text)
;;;; dired
(oo-bind 'oo-app-map "d" #'dired)
;;;;; map =h= to =dired-up-directory=
;; I do not want to keep pressing =^= for the common action of going up the
;; directory.
(oo-bind 'dired-mode-map :nm "h" #'dired-up-directory)
;; Additionally, =l= is faster than =Enter= on a QWERTY keyboard.
(oo-bind 'dired-mode-map :nm "l" #'dired-find-file)
;;;; dirvish
(oo-bind :alt #'dired #'dirvish)

(opt! dirvish-use-mode-line nil)

(opt! dirvish-attributes '(file-size subtree-state))

(opt! dirvish-default-layout nil)

(oo-bind 'dired-mode-map :nm "h" #'dired-up-directory)

(oo-add-hook 'dired-mode-hook #'dired-omit-mode)
;; By default hide details.
(oo-add-hook 'dired-mode-hook #'dired-hide-details-mode)

(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)
;;;; emms
(opt! emms-player-list '(emms-player-mpv))
(opt! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(opt! emms-directory (expand-file-name "emms/" oo-var-dir))
;; (require 'emms-player-mpv)
;;;; eshell
;;;;; miscellaneous
(oo-bind 'oo-app-map "e" #'eshell)

(oo-popup-at-bottom "\\*eshell")
;;;;; specify locations for files
;; (setq eshell-directory-name (concat oo-cache-dir "eshell/"))

;; (set! eshell-history-file-name (concat eshell-directory-name "history"))
;;;;; don't display a banner message
(opt! eshell-banner-message "")
;;;;; eshell history
(opt! eshell-hist-ignoredups t)
;;;;; prevent eshell from printing out messages on load
;; Eshell prints various messages about loading modules.  These messages
;; originate from the function [[][eshell-unload-all-modules]].  I would rather
;; not see these messages.
(oo-add-advice #'eshell-mode :around #'oo-funcall-silently)

;; At first I thought the culprit was this function, but I was wrong.  The
;; printing comes from =eshell-mode=.  In any case, however, I silence it as
;; well.
(oo-add-advice #'eshell-unload-all-modules :around #'oo-funcall-silently)

;;;; evil
(oo-add-hook 'emacs-startup-hook #'evil-mode)

;; Don't load everything at once.
;; (oo-require-hook 'after-init-hook 'evil)
(defhook! after-init-hook&load-evil ()
  [:depth 10]
  (require 'evil nil t))

;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)
;;;; evil-cleverparens
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)

(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;;; evil-easymotion
(autoload #'oo-goto-beginning-of-word "evil-easymotion")
(autoload #'oo-goto-end-of-word "evil-easymotion")
(autoload #'oo-goto-char "evil-easymotion")

(oo-bind :nv "w" #'oo-goto-beginning-of-word)
(oo-bind :nv "e" #'oo-goto-end-of-word)
(oo-bind :nv "f" #'oo-goto-char)
;;;; evil-operator
(oo-bind :n "gr" #'evil-operator-eval)
;;;; evil-surround
(oo-add-hook 'prog-mode-hook #'evil-surround-mode)

(oo-add-hook 'text-mode-hook #'evil-surround-mode)
;;;; expand-region
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
;;;; frame
;; TODO: Figure out how to set this based on the color of the theme.
(defhook! after-init-hook&set-window-divider-face ()
  "Set the window divider face."
  [:depth 11]
  (set-face-foreground 'window-divider "black"))

(defadvice! load-theme@AFset-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

;; TODO: The display flickers when setting the initial theme.  Maybe this is
;; inevitable.  But maybe this has to do with me either disabling the previous
;; theme first or the order of setting the window-divider, or maybe I can
;; specify the default theme to load beforehand.  I need to play around with
;; settings and see if this flickering can be avoided.
(oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)

;; (add-hook 'emacs-startup-hook
;;           (lambda (&rest _) (oo-add-advice #'load-theme :after #'oo-set-window-divider-face)))

;; You can either use =right-only= to place window dividers on the right of each
;; window.  Or =bottom-only= to place them just on the bottom.
(opt! window-divider-default-places t)

;; The default value is 6.
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-bottom-width 7)
;;;; helpful
(oo-bind :alt #'describe-function #'helpful-callable :feature 'helpful)
(oo-bind :alt #'describe-command  #'helpful-command  :feature 'helpful)
(oo-bind :alt #'describe-variable #'helpful-variable :feature 'helpful)
(oo-bind :alt #'describe-key      #'helpful-key      :feature 'helpful)
;;;; highlight-quoted
(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
;;;; idle-require
(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)

(oo-add-advice #'idle-require-load-next :around #'oo-funcall-silently)

(opt! idle-require-load-break 5)

(opt! idle-require-idle-delay 10)
;;;; lispy
(oo-bind :v "E" #'lispy-eval-and-replace)

(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)
;;;; lispyville
(oo-bind :n "g," #'lispyville-comment-or-uncomment)
(oo-bind :n "gc" #'lispyville-comment-and-clone-dwim)
(oo-bind :n "gl" #'lispyville-comment-and-clone-dwim)

(oo-add-hook 'prog-mode-hook #'lispyville-mode)

(oo-bind 'lispyville-mode-map :i "SPC" #'lispy-space)
(oo-bind 'lispyville-mode-map :i ";" #'lispy-comment)

(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)
;;;; macrostep
(oo-bind 'emacs-lisp-mode-map "me" #'macrostep-expand :localleader t :wk "expand")
(oo-bind 'emacs-lisp-mode-map "mc" #'macrostep-collapse :localleader t :wk "collapse")
(oo-bind 'emacs-lisp-mode-map "mC" #'macrostep-collapse-all :localleader t :wk "collapse all")
;; ;;;; gcmh
;; (oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)

;; (opt! gcmh-idle-delay 'auto)

;; (opt! gcmh-high-cons-threshold (* 8 1024 1024))

;; (opt! gcmh-low-cons-threshold (* 4 1024 1024))

;; ;; [[helpvar:minibuffer-setup-hook][minibuffer-setup-hook]] and [[helpvar:minibuffer-exit-hook][minibuffer-exit-hook]] are the hooks run just before
;; ;; entering and exiting the minibuffer (respectively).  In the minibuffer I'll be
;; ;; primarily doing searches for variables and functions.  There are alot of
;; ;; variables and functions so this can certainly get computationally expensive.  To
;; ;; keep things snappy I increase boost the [[helpvar:gc-cons-threshold][gc-cons-threshold]] just before I enter
;; ;; the minibuffer, and restore it to it's original value a few seconds after it's closed.
;; (defvaralias 'minibuffer-enter-hook 'minibuffer-setup-hook)

;; (defhook! minibuffer-setup-hook&increase-garbage-collection ()
;;   "Boost garbage collection settings to `gcmh-high-cons-threshold"
;;   [:depth 10]
;;   (when (require 'gcmh nil t)
;;     (setq gc-cons-threshold gcmh-high-cons-threshold)))

;; (defhook! minibuffer-exit-hook&decrease-garbage-collection ()
;;   "Reset garbage collection settings to `gcmh-low-cons-threshold'."
;;   [:depth 90]
;;   (require 'gcmh)
;;   (when (require 'gcmh nil t)
;;     (setq gc-cons-threshold gcmh-low-cons-threshold)))
;;;; magit
(oo-call-after-load '(evil magit) #'evil-magit-init)
;;;; no-littering
(setq no-littering-etc-directory oo-etc-dir)
(setq no-littering-var-directory oo-var-dir)

(require 'no-littering)
;;;; org
;;;;; org-superstar
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(opt! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))

(opt! org-superstar-leading-bullet ?\s)

(opt! org-superstar-special-todo-items t)
;;;;; org-appear
(oo-add-hook 'org-mode-hook #'org-appear-mode)

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
(opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
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

(oo-bind :alt #'org-edit-src-code #'oo-dwim-edit-src-code)
(oo-bind :h "," #'org-edit-src-code :localleader t)
;; (oo-bind 'org-mode-map "e" #'org-edit-src-code)
                                        ;(oo-bind ':h "es" #'org-edit-src-code :wk "source block" :localleader t)

(oo-bind 'org-src-mode-map "," #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "a" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "c" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
;;;;; org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")

(oo-bind 'oo-quick-map "j" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "a" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "j" #'org-capture :wk "capture")

(opt! org-archive-save-context-info nil)

(opt! org-archive-location (concat org-directory "archive.org::"))

(oo-add-hook 'org-insert-heading-hook #'org-id-get-create)


(opt! evilem-style 'at)
(opt! evilem-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))
;;;;; miscellaneous
(opt! org-hide-emphasis-markers t)
;;;; outli
(oo-add-hook 'prog-mode-hook #'outli-mode)
;;;; rainbow-delimiters
(oo-add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(oo-add-hook 'reb-mode-hook #'rainbow-delimiters-mode)

(opt! rainbow-delimiters-max-face-count 9)
;;;; recentf
(opt! recentf-filename-handlers '(file-truename))

(oo-add-hook 'emacs-startup-hook #'recentf-mode)

;; TODO: Figure out why this is an error with eldev eval.
;; For some reason this gives an error when I use eldev eval. I have to figure
;; out what eldev is doing here.
(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)

(oo-add-advice #'recentf-mode :around #'oo-funcall-silently)
(oo-add-advice #'recentf-cleanup :around #'oo-funcall-silently)
(oo-add-advice #'recentf-save-list :around #'oo-funcall-silently)

;; TODO: Add back =adjoin!= if I removed it.
(opt! recentf-filename-handlers (cl-adjoin #'abbreviate-file-name recentf-filename-handlers))

(opt! recentf-filename-handlers (cl-adjoin 'substring-no-properties recentf-filename-handlers))

(opt! recentf-exclude (cl-adjoin (regexp-quote (recentf-expand-file-name oo-config-dir)) recentf-exclude))

(opt! recentf-exclude (cl-adjoin (regexp-quote (recentf-expand-file-name oo-data-dir)) recentf-exclude))
;;;; savehist
(oo-add-hook 'on-first-input-hook #'savehist-mode)

(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))
;; This should be already taken care of by no-littering.
(opt! savehist-file (expand-file-name "savehist" oo-data-dir))

(opt! savehist-additional-variables (cl-adjoin 'register-alist savehist-additional-variables))

(defadvice! savehist-save@BFremove-kill-ring-properties (&rest _)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))
;;;; smartparens
(opt! sp-highlight-wrap-tag-overlay nil)

(opt! sp-highlight-pair-overlay nil)

(opt! sp-highlight-wrap-overlay nil)

(opt! sp-show-pair-delay 0.2)

(defhook! minibuffer-setup-hook&enable-smartparens-maybe ()
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(oo-add-hook 'text-mode-hook #'smartparens-mode)

(oo-add-hook 'prog-mode-hook #'smartparens-mode)

(oo-add-hook 'text-mode-hook #'turn-on-show-smartparens-mode)

(oo-add-hook 'prog-mode-hook #'turn-on-show-smartparens-mode)

(oo-bind 'oo-toggle-map "s" #'smartparens-mode)
;;;; supersave
(oo-add-hook 'on-first-file-hook #'super-save-mode)

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
;;;; vertico
(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

;; Try to split up loading.
(defhook! emacs-startup-hook&require-vertico ()
  (require 'vertico nil t))

(oo-add-hook 'on-first-input-hook #'vertico-mode)

(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

(opt! vertico-quick1 "asdf")
(opt! vertico-quick2 "jkl;")

(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)

(opt! orderless-matching-styles '(orderless-initialism
                                  orderless-regexp))

(setq vertico-buffer-display-action
      '(display-buffer-in-direction
        (direction . below)
        (window-height . ,(+ 3 vertico-count))))

(defhook! vertico-mode-hook&enable-orderless ()
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))))

(oo-popup-at-bottom "\\*Vertico")

(oo-bind 'vertico-map :i "TAB" #'vertico-next)
(oo-bind 'vertico-map :i "C-k" #'vertico-previous)
(oo-bind 'vertico-map :i "C-j" #'vertico-next)
(oo-bind 'vertico-map :i ";" #'vertico-quick-exit)
(oo-bind 'vertico-map :i "C-;" #'vertico-quick-exit)
(oo-bind 'vertico-map :i [backtab] #'vertico-previous)

(oo-bind 'vertico-map :i "C-o" #'embark-act)
;;;; which-key
(oo-add-hook 'emacs-startup-hook #'which-key-mode)

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
;;;; ws-butler
(oo-add-hook 'prog-mode-hook #'ws-butler-mode)

;;; uncategorized
;;;; initial buffer choice
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
;;;; general hooks
(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(oo-add-hook 'prog-mode-hook #'hs-minor-mode)

(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

(oo-add-hook 'on-first-input-hook #'minibuffer-depth-indicate-mode)

(oo-add-hook 'prog-mode-hook 'auto-fill-mode)

(oo-add-hook 'text-mode-hook #'visual-line-mode)

(oo-add-hook 'text-mode-hook #'auto-fill-mode)

(oo-add-hook 'text-mode-hook #'abbrev-mode)

(oo-add-hook 'prog-mode-hook #'abbrev-mode)
;;;; bindings
(oo-bind :nm "+" #'text-scale-increase)
(oo-bind :nm "-" #'text-scale-decrease)

(oo-bind 'oo-toggle-map "r" #'read-only-mode)
(oo-bind 'oo-toggle-map "t" #'load-theme)
(oo-bind 'oo-toggle-map "d" #'toggle-debug-on-error)

(oo-bind 'oo-toggle-map "p" (lambda () (interactive) (profiler-start 'cpu+mem)))
(oo-bind 'oo-toggle-map "P" #'profiler-stop)
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
(defadvice! load-theme@ARdisable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(defhook! after-init-hook&load-theme ()
  "Load `modus-operandi' theme."
  (when (display-graphic-p)
    (load-theme 'modus-operandi :no-confirm nil)))
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
;;;; eval binding
;; I evaluate things so often and even in non-emacs
(defvar oo-eval-map (make-sparse-keymap))
(define-prefix-command 'oo-eval-prefix-command 'oo-eval-map)
(oo-bind 'oo-leader-map "e" #'oo-eval-prefix-command :wk "eval")
;;; provide
(provide '98-init-features)
