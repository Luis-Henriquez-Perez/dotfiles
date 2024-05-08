;;; oo-base-settings.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Here lies a collection of built-in settings that I want to take effect
;; immediately.  Many of them have to do with disabling default Emacs behaviors
;; that I don't like. I specifically place them at the forefront of my configuration
;; to ensure that they will always be evaluated regardless of what unexpected error
;; should occur afterwards.
;;
;;; Code:
;;; settings
;;;; load newer files
(setq-default load-prefer-newer t)
;;;; do not auto-enable =show-paren-mode= in editing modes
;; By default =show-paren-mode= is enabled in all editing mode (non-special
;; modes).  I want to control when to enable this mode normally--as in, add it to
;; hooks myself if I want it enabled.  Therefore, I disable it here.
(setq show-paren-predicate nil)
;;;; disable the blinking of matching parentheses
;; This made scrolling the cursor really slow.  Maybe because it was enabled
;; with =show-parens-mode= at the same time.  This isn't needed if I have
;; =show-parens-mode= already enabled.
(setq blink-matching-paren nil)
;;;; by default do not wrap lines
;; When a line is too long to be displayed in the screen do not wrap it around;
;; just let the rest of the line go out of view (with an indicator that there is
;; more to the line in the fringe).  Although I can see the whole line when it
;; wraps around, I find it makes the text confusing and harder to read.  If I
;; want this, then I will toggle it myself with [[][toggle-truncate-lines]].
(setq-default truncate-lines t)
;;;; automatically kill any processes when exiting emacs
;; If I start a process, like the =eat= shell for example, stop me from exiting
;; to ask me whether I want to kill it, just do it.
;; https://emacsredux.com/blog/2020/07/18/automatically-kill-running-processes-on-exit/
(setq confirm-kill-processes nil)
;;;; show newlines as =\n= instead of an actual newline
;; They are easier to deal with and do not occupy unnecessary lines.
(setq print-escape-newlines t)
;;;; set the fill-column 80 by default
(setq-default fill-column 80)
;;;; stop creating =auto-save-list= directory
;; See [[https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created][#18677]].
(setq auto-save-list-file-prefix nil)
;;;; use =yes-or-no-p= instead of =y-or-n-p=
;; Essentially, I am telling all Emacs functions that prompt the user for a =yes=
;; or =no= to instead allow me to type =y= or =p=.  [[helpfn:yes-or-no-p][yes-or-no-p]] is defined in c
;; source code.
(advice-add #'yes-or-no-p :override #'y-or-n-p)
;;;; don't create lockfiles
(setq create-lockfiles nil)
;;;; don't flash unstyled modeline at startup
;; When emacs starts up, the default modeline will show up.  Rendering this default
;; modeline at startup does slightly slow down emacs (insignificant on it's own but
;; these things add up).  So I disable it.
;; (setq-default mode-line-format nil)
;;;; don't ask me whether I want to kill a buffer with a live process
;; I got this from [[https://www.masteringemacs.org/article/disabling-prompts-emacs][this-post]].  Every time you try to kill a buffer with a live
;; process, Emacs will ask you if you're sure you want to kill it.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))
;;;; don't pass case-insensitive to =auto-mode-alist=
;; This is taken from =centaur-emacs=.  By default [[file:snapshots/*helpful variable: auto-mode-case-fold*.png][auto-mode-case-fold]] is
;; non-nil; when enabled the auto-mode-alist is traversed twice.  This double
;; traversal can be expensive and it seems unnecessary.
(setq auto-mode-case-fold nil)
;;;; don't suggest keybindings or the like for me
;; See [[https://stackoverflow.com/questions/19781529/how-to-disable-emacs-messages-like-you-can-run-the-command-x-with-y][this stackoverflow post]].  After invoking [[file:snapshots/*helpful command: execute-extended-command*.png][execute-extended-command]] on a
;; command that has an existing keybinding, or something that could be abbreviated,
;; emacs will suggest a shorter way.
(setq suggest-key-bindings nil)
;;;; stop asking me whether I want to enable file local variables
;; When installing packages with =quelpa=, I was prompted whether I wanted to apply
;; file local variables.  I'm guessing =straight.el= and =elpaca= disable this.
;; The value safe tells Emacs to only apply the "safe" local variables.  I'm
;; assuming this means ones like "mode" which tell Emacs to open the buffer at a
;; certain major mode.  At first I had this set to nil, but I wanted to open
;; [[][]] in =common-lisp-mode= and I realized Emacs wasn't doing it because I
;; told it not to with this variable.
(setq enable-local-variables :safe)
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
;;;; don't disable any commands
;; If non-nil certain commands such as narrowing are disabled.  The idea is that
;; a new user would think that emacs deleted the contents of their file if they
;; accidentally narrowed the buffer.  I am experienced enough so that I don't
;; Need this.
(setq disabled-command-function nil)
;;;; disable cursor blinking
;; By default the cursor blinks.  The point is so that it is easier to find on the
;; screen.  Usually, however, I have no trouble finding it so I disable it.
(blink-cursor-mode -1)
;;;; move files to trash instead of deleting them
;; By default Emacs actually deletes files.  By setting this to t, you tell Emacs
;; to move a file to trash instead of actually deleting it.  This is better because
;; if you accidentally delete a file or discover you can still just go get your
;; file from the trash.
(setq delete-by-moving-to-trash t)
;;;; enable recursive minibuffer
;; With this enabled, I can invoke the minibuffer while still being in the
;; minibuffer.  At the very least this is useful so that I can inspect which keys
;; are bound in the minibuffer.
(setq enable-recursive-minibuffers t)
;;;; recenter point if it goes 20 lines past what is visible
;; Note that the following comment is taken from noctuid's config: "Recenter the
;; point if it goes greater than 20 lines past what is visible the default, 0, is
;; kind of annoying because it recenters even if you just go one line down from
;; the window bottom, but a higher value is nice to automatically recenter after
;; any bigger jump."
(setq scroll-conservatively 20)
;;;; don't show the startup screen
;; By default Emacs displays [[][this startup screen]] at startup.  No thanks!  I
;; think these variables are all aliases for eachother.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;;;; skip fontification functions when there's input pending
(setq redisplay-skip-fontification-on-input t)
;;;; don't update things on the screen as frequently
;; This variable is.
;; https://github.com/hlissner/doom-emacs/blob/ooaadd89oobe45f912124d9d815d8790f540d38c/core/core.el#L177
(setq idle-update-delay 1)
;;;; don't make backups
(setq make-backup-files nil)
;;;; backup files to trash
(setq backup-directory-alist '((".*" . "~/.Trash")))
;;;; diable auto-save-mode
(setq auto-save-default nil)
(auto-save-mode -1)
;;;; don't echo keystrokes
;; By default emacs shows.
(setq echo-keystrokes 0)
;;;; ensure there's always a newline at the end of files
;; Several linux programs require a newline at the end of a file, such as
;; chrontab--this is more or less what noctuid said and I'll take his word for
;; it.
(setq require-final-newline t)
;;;; set the tab-width to =4=; it's =8= by default
(setq-default tab-width 4)
;;;; set the initial major mode to =fundamental-mode=
;; This improve startup time because packages enabled for emacs-lisp-mode are not
;; loaded immediately.
(setq initial-major-mode 'fundamental-mode)
;;;; don't display message advertising gnu system
;; They made the process of disabling this more difficult.
(advice-add #'display-startup-echo-area-message :around #'ignore)
;;;; disable initial scratch message
;; Don't display any documentation--or any message at all--in the =*scratch*=
;; buffer.  Emacs by default displays [[][a message in the scratch buffer]].
(setq initial-scratch-message nil)
;;;; don't add indent
(setq-default indent-tabs-mode nil)
;;;; don't beep
;; This variable controls whether emacs makes a sound when certain events happen
;; such as invoking a binding that doesn't have anything bound to it or trying
;; to exceed the end of the buffer--things like that.  Personally, I don't want
;; such beeping.  Setting this variable to nil still result in beeping, emacs
;; just uses its default function.  Instead, to be disabled it must
;; be set to [[file:snapshots/helpful-command:ignore.png][ignore]].
(setq ring-bell-function #'ignore)
;;;; disable repeated error message functions
;; When you try to move past the beginning and end of a buffer Emacs produces
;; error messages.
;; [[https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer][disable warnings]]
(defun oo-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (unless (memq (car data) '(buffer-read-only beginning-of-buffer end-of-buffer))
    (command-error-default-function data context caller)))

(setq command-error-function #'oo-command-error-function)
;;;; designate location of trash
;; designate the location of the trash directory
;; I accidentally sent files to the trash and I could not find them in my trash
;; directory.  I was confused because I knew that the variable
;; [[file:_helpful_variable__delete-by-moving-to-trash_.png][delete-by-moving-to-trash]] was non-nil and I even verified this to be the case
;; with [[file:_helpful_function__helpful-variable_.png][helpful-variable]].  After reading the documentation of [[][]] I realized
;; that emacs uses the [[][]].  To be honest I had no idea what this actually was
;; but I extracted what looked like the location, [[][]].
(setq trash-directory (expand-file-name "~/Trash"))
;;;; don't ask me whether to follow symlinks, just do it
;; By default Emacs will prompt you when you want to open a file a symlink
;; references.  It will ask you whether you want to follow the symlink.  For me
;; the answer is predominately yes.
(setq vc-follow-symlinks t)
(setq vc-follow-link t)
;;;; prevent =*Messages*= and =*scratch*= buffers from being killed
;; "Locking" a file can mean two different things (or both of these things at
;; once).  It can mean that Emacs cannot be exited while there are "locked"
;; buffers; it can also mean that the locked buffers cannot be killed (e.g. via
;; [[file:snapshots/_helpful_command__kill-buffer_.png][kill-buffer]]).  I don't think I ever want the former behavior.  Setting
;; [[][emacs-default-locking-mode]] to kill tells Emacs just to prevent buffers
;; with =emacs-lock-mode= enabled from being killed.  If you were to try to kill
;; one with something like =kill-buffer=, it would fail and you'd get a message
;; saying the buffer cannot be killed.

;; The =*Messages*= buffer could contain important information and should never
;; really be killed. See [[https://www.emacswiki.org/emacs/ProtectingBuffers][ProtectingBuffers]].
(setq emacs-lock-default-locking-mode 'kill)
(with-current-buffer "*Messages*" (emacs-lock-mode 1))
(with-current-buffer "*scratch*" (emacs-lock-mode 1))
;;;; stop emacs from asking to save buffers on quit
;; https://stackoverflow.com/questions/35658509/gnu-emacs-how-to-disable-prompt-to-save-modified-buffer-on-exit
;; https://emacs.stackexchange.com/questions/22275/save-a-particular-buffer-without-prompting-on-emacs-exit
;; https://stackoverflow.com/questions/6762686/prevent-emacs-from-asking-modified-buffers-exist-exit-anyway
;;;; uncategorized
;; if you don't use RTL ever, this could improve perf
;; https://news.ycombinator.com/item?id=39127859
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; improves terminal emulator (vterm/eat) throughput
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq inhibit-compacting-font-caches t)

(setq idle-update-delay 1.0)
;;;;; do not save abbrevs to a file
;; Do not manage abbrevs for me.
;; Do not prompt me for saving abbrevs.
;; (setq save-abbrevs 'silently)
(advice-add #'read-abbrev-file :around #'ignore)
(advice-add #'write-abbrev-file :around #'ignore)
(advice-add #'abbrev--possibly-save :around #'ignore)
;; (setq abbrev-file-name null-device)
;; By default Emacs calls this function on startup.  Thus if there is an
;; existing abbrev file it will.
(advice-add #'quietly-read-abbrev-file :around #'ignore)
;;; provide
(provide 'oo-base-settings)
;;; oo-base-settings.el ends here
