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
;; feel like it was not worth it.  Also it made less sense for me to do so
;; considering.
;;
;;; Code:
;;;; avy
(opt! avy-style 'pre)
(opt! avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))
(opt! avy-background nil)
(opt! avy-timeout-seconds 0.3)
;;;; evil
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
;;;; emms
(opt! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(opt! emms-directory (expand-file-name "emms/" oo-var-dir))

;; TODO: Make this dependent on whether mpv is installed.  And also figure out
;; how to do this lazily.  Ideally when I would intercept a "player-list is
;; empty" error and if I have mpv installed, add it and play the file.  I can do
;; this for `emms-play-file' but I need to check if to.
(opt! emms-player-list '(emms-player-mpv))
;;;; vertico
(opt! vertico-quick1 "asdfgh")
(opt! vertico-quick2 "jkluionm")
(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)
;;;; orderless
(opt! orderless-matching-styles '(orderless-initialism orderless-regexp))
;;;; consult
(opt! consult-preview-key nil)
(opt! consult-fontify-preserve nil)
;;;; super-save
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
;;;; savehist
(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))
(opt! savehist-additional-variables (cl-adjoin 'register-alist savehist-additional-variables))
;;;; smartparens
(opt! sp-highlight-wrap-tag-overlay nil)
(opt! sp-highlight-pair-overlay nil)
(opt! sp-highlight-wrap-overlay nil)
(opt! sp-show-pair-delay 0.2)
;;; provide
(provide 'oo-init-settings)
;;; oo-init-settings.el ends here
