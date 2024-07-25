;;; oo-init-features.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(hook! prog-mode-hook&abbrev-mode)
(hook! text-mode-hook&abbrev-mode)
;;;; ace-window
(opt! aw-swap-invert t)
(opt! aw-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxb")))
;;;; aggressive-indent
(hook! emacs-lisp-mode-hook&aggressive-indent-mode)
;;;; avy
(opt! avy-style 'pre)
(opt! avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))
(opt! avy-background nil)
(opt! avy-timeout-seconds 0.3)
;;;; captain-mode
(hook! prog-mode-hook&captain-mode)
(hook! text-mode-hook&captain-mode)
(defhook! text-mode-hook&set-captain-local-vars ()
  (setq-local captain-predicate #'always)
  (setq-local captain-sentence-start-function #'captain--default-sentence-start))
;;;; consult
(opt! consult-preview-key nil)
(opt! consult-fontify-preserve nil)

(alt! imenu consult-imenu consult)
(alt! display-buffer oo-pop-to-buffer consult)
(alt! switch-to-buffer consult-buffer consult)
(alt! yank-pop consult-yank-pop consult)
(alt! apropos consult-apropos consult)
(alt! man consult-man consult)
;;;; corfu
;; TODO: make it so moving on a candidate if I press espace insert that candidate.
(opt! corfu-preview-current t)
(opt! corfu-preselect-first t)
(opt! corfu-quit-at-boundary nil)
(opt! corfu-auto t)
(opt! corfu-auto-delay 0.1)
(opt! corfu-auto-prefix 1)
(opt! corfu-bar-width 0)

;; When using evil, neither `corfu-map' nor `tempel-map' bindings will work
;; because the maps are overridden by evil.  In order for them to work, we need
;; to boost give the maps greater precedence.
(with-eval-after-load 'evil
  (with-no-warnings
    (evil-make-overriding-map corfu-map)
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)))

(bind! i corfu-map "<tab>"   #'corfu-next)
(bind! i corfu-map [backtab] #'corfu-previous)
(bind! i corfu-map "S-TAB"   #'corfu-previous)
(bind! i corfu-map "C-;"     #'corfu-quick-complete)
(bind! i corfu-map "C-j"     #'corfu-next)
(bind! i corfu-map "C-k"     #'corfu-previous)
(bind! i corfu-map "C-p"     #'corfu-previous)
(bind! i corfu-map ";"       #'corfu-quick-complete)
(bind! i corfu-map "SPC"     #'corfu-insert)
;;;; corfu-history
(hook! corfu-mode-hook&corfu-history-mode)
;;;; corfu-quick
(opt! corfu-quick1 "ajskdlghty")
(opt! corfu-quick2 "ajskdlghty")
;; https://github.com/minad/corfu/issues/12
;;;; chezmoi
;; I need the command to write the source from the target.  The command
;; =chezmoi-apply= does this but I would like it to do it automatically if I am
;; already editing a target-file.
(defhook! after-save-hook&chezmoi-write-maybe (&rest _)
  (when (aand (require 'chezmoi nil t)
              (buffer-file-name)
              (chezmoi-target-file it))
    (with-demoted-errors "error:%S" (chezmoi-write))))
;;;; dashboard
(require 'dashboard)

;; If I put dashboard configuration in its own.
(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))
(setq dashboard-init-info #'oo-dashboard-init-info)
(setq dashboard-banner-logo-title "Welcome!")
(setq dashboard-startupify-list (-difference dashboard-startupify-list '(dashboard-insert-items dashboard-insert-footer)))
(setq dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))
(setq dashboard-center-content t)
;;;; denote
(opt! denote-directory "~/Documents/notes/")
(opt! denote-file-type 'text)
;;;; dired
(hook! dired-mode-hook&dired-omit-mode)
;; By default hide details.
(hook! dired-mode-hook&dired-hide-details-mode)
(opt! dired-clean-confirm-killing-deleted-buffers nil)
(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)

;; Dired is very picky about when these bindings happen.  It is the only package
;; I have had that is that picky.  I have noticed that unlike every other
;; package I have tried dired bindings do not work by trying to set them when
;; `dired-mode-map' is bound.  You need to use (eval-after-load 'dired ...).
;; Also, even if you have the `eval-after-load' it work work from the
;; `oo-after-load-dired' file--do not ask me why.  Again, only package I have
;; had this happen with.
(bind! (n m) dired-mode-map "h" #'dired-up-directory)
(bind! (n m) dired-mode-map "l" #'dired-find-file)
;;;; dirvish
(opt! dirvish-use-mode-line nil)
(opt! dirvish-attributes '(file-size subtree-state))
(opt! dirvish-default-layout nil)
(alt! dired dirvish dirvish)
;;;; emmet
(hook! html-mode-hook&emmet-mode)
;;;; emms
(opt! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(opt! emms-directory (expand-file-name "emms/" oo-var-dir))

;; TODO: Make this dependent on whether mpv is installed.  And also figure out
;; how to do this lazily.  Ideally when I would intercept a "player-list is
;; empty" error and if I have mpv installed, add it and play the file.  I can do
;; this for `emms-play-file' but I need to check if to.
(opt! emms-player-list '(emms-player-mpv))
(oo-call-after-load 'emms #'require emms-player-mpv nil t)
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
(oo-call-after-load 'eshell #'require 'eshell-z)
(oo-call-after-load 'eshell #'require 'eshell-up)
(oo-call-after-load 'em-alias #'require 'oo-eshell-aliases)
;;;; evil
(defhook! after-init-hook&load-evil ()
  [:depth 10]
  (require 'evil nil t))
(hook! emacs-startup-hook&evil-mode)
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defhook! evil-mode-hook&make-intercept-map ()
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map oo-override-mode-map 'all t))
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

(advice-add #'load-theme :around (lambda (fn &rest args) (apply fn args) (evil-refresh-cursor)))

(oo-call-after-load 'evil #'require 'oo-evil-operators)

(bind! n "H" #'evil-first-non-blank)
(bind! n "L" #'evil-last-non-blank)
(bind! n "J" #'evil-scroll-page-down)
(bind! n "K" #'evil-scroll-page-up)

(bind! (n v) "g u" #'evil-upcase)
(bind! (n v) "g U" #'evil-downcase)

;; Pressing lowercase "o" is one less keystroke than "W" and it aligns with cio.
;; Though I will say I am not 100% sure it is the equivalent.
(bind! evil-motion-state-map "o" #'evil-forward-WORD-begin)

(bind! (n v) "g t" #'evil-goto-first-line)
(bind! (n v) "g b" #'evil-goto-line)
;;;; evil-cleverparens
(bind! evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(bind! evil-outer-text-objects-map "f" #'evil-cp-a-form)
;;;; evil-easymotion
(opt! evilem-style 'at)
(opt! evilem-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

(autoload #'oo-evilem-motion-beginning-of-word "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-WORD "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-end-of-word       "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-end-of-WORD       "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-char              "oo-evilem-motions" nil t 'function)
(autoload #'oo-evilem-motion-beginning-of-line "oo-evilem-motions" nil t 'function)

(bind! (n v) "w" #'oo-evilem-motion-beginning-of-word)
(bind! (n v) "W" #'oo-evilem-motion-beginning-of-WORD)
(bind! (n v) "e" #'oo-evilem-motion-end-of-word)
(bind! (n v) "E" #'oo-evilem-motion-end-of-WORD)
(bind! (n v o) "f" #'oo-evilem-motion-char)
(bind! (n v o) "H" #'oo-evilem-motion-beginning-of-line)
;;;; evil-exchange
(bind! (n v) "g x" #'evil-exchange)
(bind! (n v) "g X" #'evil-exchange-cancel)
(bind! (n v) "g a" #'evil-exchange)
(bind! (n v) "g A" #'evil-exchange-cancel)
;;;; evil-surround
(hook! prog-mode-hook&evil-surround-mode)
(hook! text-mode-hook&evil-surround-mode)
;;;; evil-textobj-anyblock
(bind! evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
(bind! evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
;;;; evil-textobj-line
(bind! evil-inner-text-objects-map "l" #'evil-inner-line)
(bind! evil-outer-text-objects-map "l" #'evil-a-line)
;;;; expreg
(bind! v "V" #'expreg-contract)
(bind! v "v" #'expreg-expand)
;;;; fill-adapt
(hook! auto-fill-mode-hook&filladapt-mode)
;;;; gcmh
(hook! emacs-startup-hook&gcmh-mode :depth 91)
(opt! gcmh-idle-delay 'auto)
(opt! gcmh-high-cons-threshold (* 8 1024 1024))
(opt! gcmh-low-cons-threshold (* 4 1024 1024))
;;;; grugru
(oo-call-after-load 'grugru #'require 'oo-grugru-definitions)
;;;; helm
(oo-popup-at-bottom "\\*Helm")
(set! helm-candidate-number-limit 50)

(bind! i helm-map "TAB" #'helm-next-line)
(bind! i helm-map [backtab] #'helm-previous-line)
(bind! i helm-map "C-j" #'helm-next-line)
(bind! i helm-map "C-k" #'helm-previous-line)

(bind! i helm-map "C-a" #'helm-select-action)
(bind! i helm-map "C-m" #'helm-toggle-visible-mark-forward)
;; (bind! i helm-map :ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
(bind! i helm-map "S-TAB" #'helm-mark-current-line)
(bind! i helm-map "C-;" #'ace-jump-helm-line)
;;;; helpful
(alt! describe-function helpful-callable helpful)
(alt! describe-command helpful-command helpful)
(alt! describe-variable helpful-variable helpful)
(alt! describe-key helpful-key helpful)
;;;; highlight-quoted
(hook! emacs-lisp-mode-hook&highlight-quoted-mode)
;;;; hungry-delete
;; Leave one space in between instead of deleting everything.
(opt! hungry-delete-join-reluctantly t)
;;;; idle-require
(oo-add-advice #'idle-require-load-next :around #'oo-funcall-silently)
;;;; lispyville
;; Do not bind any keys by default.
(hook! prog-mode-hook&lispyville-mode)
(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)
(opt! lispyville-key-theme nil)

(bind! i lispyville-mode-map "SPC" #'lispy-space)
(bind! i lispyville-mode-map ";" #'lispy-comment)

(bind! evil-outer-text-objects-map "c" #'lispyville-outer-comment)
(bind! evil-inner-text-objects-map "c" #'lispyville-inner-comment)

(bind! (n v) "g c" #'lispyville-comment-or-uncomment)
(bind! (n v) "g l" #'lispyville-comment-and-clone-dwim)
;;;; magit
(oo-call-after-load 'evil #'evil-magit-init)
(oo-popup-at-bottom "\\`magit")
;;;; marginalia
(hook! vertico-mode-hook&marginalia-mode)
;;;; modus-operandi
(defhook! after-init-hook&load-modus-operandi-theme ()
  "Load `modus-operandi' theme."
  (load-theme 'modus-operandi :no-confirm nil))
;;;; no-littering
(setq no-littering-etc-directory oo-etc-dir)
(setq no-littering-var-directory oo-var-dir)
(require 'no-littering)
;;;; notmuch
(opt! notmuch-sort-oldest-first nil)
;;;; orderless
(opt! orderless-matching-styles '(orderless-initialism orderless-regexp))

(defhook! vertico-mode-hook&enable-orderless ()
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))))
;;;; org
(opt! org-src-fontify-natively t)
(opt! org-hide-emphasis-markers t)
;;;; org-appear
(hook! org-mode-hook&org-appear-mode)
(opt! org-appear-autolinks t)
;;;; org-capture
(oo-popup-at-bottom "CAPTURE[^z-a]+")
(opt! org-archive-save-context-info nil)
(opt! org-archive-location (concat org-directory "archive.org::"))
(hook! org-insert-heading-hook&org-id-get-create)
;;;; org-id
(opt! org-id-track-globally t)
(opt! org-id-locations-file (expand-file-name "org-id-locations" oo-data-dir))
;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(opt! org-id-method 'ts)
(opt! org-id-link-to-org-use-id t)
;;;; org-refile
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
;;;; org-src
(oo-popup-at-bottom "\\*Org Src")
(opt! org-edit-src-persistent-message nil)
;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))
;; (adjoin! org-src-lang-modes '("lua" . lua))
(opt! org-src-ask-before-returning-to-edit-buffer nil)
(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)
(opt! org-src-window-setup 'plain)
;;;; org-superstar
(hook! org-mode-hook&org-superstar-mode)
(opt! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))
(opt! org-superstar-leading-bullet ?\s)
(opt! org-superstar-special-todo-items t)
;;;; outli
(hook! prog-mode-hook&outli-mode)
;; TODO: figure out how to make this a named advice.
(setf (cl-fourth (assoc 'emacs-lisp-mode outli-heading-config)) nil)
(advice-add 'load-theme :after (lambda (&rest _) (outli-reset-all-faces)))
;;;; rainbow-delimiters
(hook! prog-mode-hook&rainbow-delimiters-mode)
(hook! reb-mode-hook&rainbow-delimiters-mode)
;;;; recentf
(hook! emacs-startup-hook&recentf-mode)
(hook! kill-emacs-hook&recentf-save-list)

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
(oo-add-advice #'recentf-cleanup :around #'oo-funcall-silently)
(oo-add-advice #'recentf-save-list :around #'oo-funcall-silently)
(oo-add-advice #'recentf-mode :around #'oo-funcall-silently)

(opt! recentf-filename-handlers '(file-truename))
;;;; rx
;; (oo-call-after-load #'require 'oo-rx-definitions)
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

;; This allows me to have parens completion when I invoke the command `eval-expression'.
(defhook! minibuffer-setup-hook&enable-smartparens-maybe ()
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(opt! sp-highlight-wrap-tag-overlay nil)
(opt! sp-highlight-pair-overlay nil)
(opt! sp-highlight-wrap-overlay nil)
(opt! sp-show-pair-delay 0.2)

(oo-call-after-load 'smartparens #'require 'smartparens-config)
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
;;;; tempel
(bind! i tempel-map "C-j" #'tempel-next)
(bind! i tempel-map "C-k" #'tempel-previous)
(bind! i tempel-map "TAB" #'tempel-next)
(bind! i tempel-map [backtab] #'tempel-previous)
;;;; vertico
(hook! on-first-input-hook&vertico-mode)
(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)

(bind! i vertico-map "TAB" #'vertico-next)
(bind! i vertico-map "C-k" #'vertico-previous)
(bind! i vertico-map "C-j" #'vertico-next)
(bind! i vertico-map ";" #'vertico-quick-exit)
(bind! i vertico-map "C-;" #'vertico-quick-exit)
(bind! i vertico-map [backtab] #'vertico-previous)
(bind! i vertico-map "C-o" #'embark-act)
;;;; vertico-buffer
(hook! vertico-mode-hook&vertico-buffer-mode)

(opt! vertico-buffer-display-action
      '(display-buffer-in-direction
        (direction . below)
        (window-height . ,(+ 3 vertico-count))))

(oo-popup-at-bottom "\\*Vertico")
;;;; vertico-quick
(opt! vertico-quick1 "asdfgh")
(opt! vertico-quick2 "jkluionm")
;;;; wdired
(opt! wdired-confirm-overwrite nil)
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
;;;; window-divider
;; TODO: The display flickers when setting the initial theme.  Maybe this is
;; inevitable.  But maybe this has to do with me either disabling the previous
;; theme first or the order of setting the window-divider, or maybe I can
;; specify the default theme to load beforehand.  I need to play around with
;; settings and see if this flickering can be avoided.
(hook! after-init-hook&window-divider-mode :depth 12)
(opt! window-divider-default-bottom-width 7)
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-places t)
;;; provide
(provide 'oo-init-features)
;;; oo-init-features.el ends here
