;; Stop creating the `auto-save-list-directory'.
;; https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created
(setq auto-save-list-file-prefix nil)

(setq auto-save-default nil)
(auto-save-mode -1)

;; Essentially, I am telling all Emacs functions that prompt the user for a =yes=
;; or =no= to instead allow me to type =y= or =p=.
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Don't create lockfiles.
(setq create-lockfiles nil)

;; When emacs starts up, the default modeline will show up.  Rendering this default
;; modeline at startup does slightly slow down emacs (insignificant on it's own but these things add
;; up).  So I disable it.
(setq-default mode-line-format nil)

;; I got this from [[https://www.masteringemacs.org/article/disabling-prompts-emacs][this-post]].
;; Every time you try to kill a buffer with a live process, Emacs will ask you if you're sure you
;; want to kill it.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; This is taken from =centaur-emacs=.  By default [[][]] is non-nil; when enabled
;; the auto-mode-alist is traversed twice.  This double traversal can be expensive
;; and it seems unnecessary.
(setq auto-mode-case-fold nil)

;; Don't suggest keybindings for me.
;; When you select a command from the minibuffer that already has a keybinding emacs will try to
;; prompt you saying that you could have invoked said keybinding instead.  I don't need this.

;; See [[https://stackoverflow.com/questions/19781529/how-to-disable-emacs-messages-like-you-can-run-the-command-x-with-y][this stackoverflow post]].  After invoking [[][execute-extended-command]] on a
;; command that has an existing keybinding, or something that could be abbreviated,
;; emacs will suggest a shorter way.
(setq suggest-key-bindings nil)

;; Don't enable local variables by default.
;; When installing packages with =quelpa=, I was prompted whether I wanted to apply
;; file local variables.  I'm guessing =straight.el= and =elpaca= disable this.
(setq enable-local-variables nil)

;; Follow links without asking me.
(setq vc-follow-symlinks t)
(setq vc-follow-link t)

;; Downloading themes with elpaca is safe.  I don't make a habit of grabbing random
;; themes from wierd places online and evaluating them.
(setq custom-safe-themes t)

;; I don't need it.  I'll be honest; to me it seems like the emacs's custom
;; interface is intended for people that don't know elisp.  For me it's completely
;; unnecessary.  Every variable I customize is in my emacs config.
(setq custom-file null-device)

(setq disabled-command-function nil)

;; By default the cursor blinks.  The point is so that it is easier to find on the
;; screen.  Usually, however, I have no trouble finding it so I disable it.
(blink-cursor-mode -1)

;; By default Emacs actually deletes files.  By setting this to t, you tell Emacs to move a file to
;; trash instead of actually deleting it.  This is better because if you accidentally delete a file or
;; discover you can still just go get your file from the trash.
(setq delete-by-moving-to-trash t)

;; With this enabled, I can invoke the minibuffer while still being in the
;; minibuffer.  At the very least this is useful so that I can inspect which keys
;; are bound in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Note that the following comment is taken from noctuid's config: "Recenter the
;; point if it goes greater than 20 lines past what is visible the default, 0, is
;; kind of annoying because it recenters even if you just go one line down from
;; the window bottom, but a higher value is nice to automatically recenter after
;; any bigger jump."
(setq scroll-conservatively 20)

;; By default Emacs displays [[][this startup screen]] at startup.  No thanks! I
;; think these variables are all aliases for eachother.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

(setq redisplay-skip-fontification-on-input t)

;; https://github.com/hlissner/doom-emacs/blob/01aadd8900be45f912124d9d815d8790f540d38c/core/core.el#L177
(setq idle-update-delay 1)

;; Don't make backup files.
(setq make-backup-files nil)

;; But if I decide to make them later put them in the trash.
(setq backup-directory-alist '((".*" . "~/.Trash")))

;; Don't display my keystrokes as I type.
;; If I want this I'll get it with which-key.
(setq echo-keystrokes 0)

;; "Several linux programs require a newline at the end of a file, such as
;; chrontab"--this is more or less what noctuid said and I'll take his word for
;; it.
(setq require-final-newline t)

(setq-default tab-width 4)

;; This improve startup time because packages enabled for emacs-lisp-mode are not
;; loaded immediately.
(setq initial-major-mode 'fundamental-mode)

;; Don't display advertisement for the gnu system. They made the process of disabling this more
;; difficult.
(advice-add #'display-startup-echo-area-message :around #'ignore)

;; Don't display any documentation--or any message at all--in the =*scratch*= buffer.
(setq initial-scratch-message nil)

;; Use spaces by default; not tabs.
(setq-default indent-tabs-mode nil)

;; This variable controls whether emacs makes a sound when certain events happen
;; such as invoking a binding that doesn't have anything bound to it or trying
;; to exceed the end of the buffer--things like that.  Personally, I don't want
;; such beeping.  Setting this variable to nil still result in beeping, emacs
;; just uses its default function.  Instead, to be disabled it must
;; be set to ignore.
(setq ring-bell-function #'ignore)

;; Whenever the cursor hits the beginning or end of the buffer, emacs signals in error in the
;; Message buffer; same thing when you're trying to edit a read-only buffer. I'd rather not see
;; These messages--especially the beginning/end of buffer one. First, they tend to crowd up the
;; *Messages* buffer; and, second, I usually don't need the indicator anyway--I can see that if
;; nothing happens I've reached the top of the buffer. I might consider adding an visual indicator
;; via `beacon'.
(defun oo-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (unless (memq (car data) '(buffer-read-only beginning-of-buffer end-of-buffer))
    (command-error-default-function data context caller)))

(setq command-error-function #'oo-command-error-function)

(provide 'oo-settings)
