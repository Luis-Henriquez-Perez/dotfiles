(require 'cl-lib)

;; * defaults
;; Here lies a collection of built-in settings that I want to take effect
;; immediately.  Many of them have to do with disabling default Emacs behaviors
;; that I don't like. I specifically place them at the forefront of my configuration
;; to ensure that they will always be evaluated regardless of what unexpected error
;; should occur afterwards.

;; ** prevent =*Messages*= and =*scratch*= buffers from being killed
;; :PROPERTIES:
;; :ID:       20230907T101201.734381
;; :END:
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
;; ** stop creating =auto-save-list= directory
;; :PROPERTIES:
;; :ID:       20230827T140209.010138
;; :END:
;; See [[https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created][#18677]].
(setq auto-save-list-file-prefix nil)
;; ** use =yes-or-no-p= instead of =y-or-n-p=
;; :PROPERTIES:
;; :ID:       82a84315-2018-42e0-bd1a-74af7b722593
;; :END:
;; Essentially, I am telling all Emacs functions that prompt the user for a =yes=
;; or =no= to instead allow me to type =y= or =p=.  [[helpfn:yes-or-no-p][yes-or-no-p]] is defined in c
;; source code.
(advice-add #'yes-or-no-p :override #'y-or-n-p)
;; ** don't create lockfiles
;; :PROPERTIES:
;; :ID:       20230825T124046.455636
;; :END:
(setq create-lockfiles nil)
;; ** don't flash unstyled modeline at startup
;; :PROPERTIES:
;; :ID:       20230731T181236.344398
;; :END:
;; When emacs starts up, the default modeline will show up.  Rendering this default
;; modeline at startup does slightly slow down emacs (insignificant on it's own but
;; these things add up).  So I disable it.
(setq-default mode-line-format nil)
;; ** don't ask me whether I want to kill a buffer with a live process
;; :PROPERTIES:
;; :ID:       20230807T002031.281189
;; :END:
;; I got this from [[https://www.masteringemacs.org/article/disabling-prompts-emacs][this-post]].  Every time you try to kill a buffer with a live
;; process, Emacs will ask you if you're sure you want to kill it.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; ** don't pass case-insensitive to =auto-mode-alist=
;; :PROPERTIES:
;; :ID:       20230731T183648.053172
;; :END:
;; This is taken from =centaur-emacs=.  By default [[file:snapshots/*helpful variable: auto-mode-case-fold*.png][auto-mode-case-fold]] is
;; non-nil; when enabled the auto-mode-alist is traversed twice.  This double
;; traversal can be expensive and it seems unnecessary.
(setq auto-mode-case-fold nil)
;; ** don't suggest keybindings or the like for me
;; :PROPERTIES:
;; :ID:       20230807T002419.333469
;; :END:
;; See [[https://stackoverflow.com/questions/19781529/how-to-disable-emacs-messages-like-you-can-run-the-command-x-with-y][this stackoverflow post]].  After invoking [[file:snapshots/*helpful command: execute-extended-command*.png][execute-extended-command]] on a
;; command that has an existing keybinding, or something that could be abbreviated,
;; emacs will suggest a shorter way.
(setq suggest-key-bindings nil)
;; ** stop asking me whether I want to enable file local variables
;; :PROPERTIES:
;; :ID:       20230806T164746.702869
;; :END:
;; When installing packages with =quelpa=, I was prompted whether I wanted to apply
;; file local variables.  I'm guessing =straight.el= and =elpaca= disable this.
;; The value safe tells Emacs to only apply the "safe" local variables.  I'm
;; assuming this means ones like "mode" which tell Emacs to open the buffer at a
;; certain major mode.  At first I had this set to nil, but I wanted to open
;; [[][]] in =common-lisp-mode= and I realized Emacs wasn't doing it because I
;; told it not to with this variable.
(setq enable-local-variables :safe)
;; ** don't ask me whether to follow symlinks, just do it
;; :PROPERTIES:
;; :ID:       20230731T162025.361386
;; :END:
;; By default Emacs will prompt you when you want to open a file a symlink
;; references.  It will ask you whether you want to follow the symlink.  For me
;; the answer is predominately yes.
(setq vc-follow-symlinks t)
(setq vc-follow-link t)
;; ** don't ask me for permission to enable a theme
;; :PROPERTIES:
;; :ID:       20230731T162020.231251
;; :END:
;; By default Emacs will ask you whether you are sure you want to enable a theme
;; as a precaution because a theme could contain malicious code.  Downloading
;; themes with elpaca is safe.  I don't make a habit of grabbing random themes
;; from wierd places online and evaluating them.  So I don't need.
(setq custom-safe-themes t)
;; ** don't create a custom file
;; :PROPERTIES:
;; :ID:       20230731T162013.703695
;; :END:
;; I don't need it.  I'll be honest; to me it seems like the emacs's custom
;; interface is intended for people that don't know elisp.  For me it's completely
;; unnecessary.  Every variable I customize is in my emacs configuration.
(setq custom-file null-device)
;; ** don't disable any commands
;; :PROPERTIES:
;; :ID:       20230731T162007.289836
;; :END:
;; If non-nil certain commands such as narrowing are disabled.  The idea is that
;; a new user would think that emacs deleted the contents of their file if they
;; accidentally narrowed the buffer.  I am experienced enough so that I don't
;; Need this.
(setq disabled-command-function nil)
;; ** disable cursor blinking
;; :PROPERTIES:
;; :ID:       20230731T161959.758192
;; :END:
;; By default the cursor blinks.  The point is so that it is easier to find on the
;; screen.  Usually, however, I have no trouble finding it so I disable it.
(blink-cursor-mode -1)
;; ** move files to trash instead of deleting them
;; :PROPERTIES:
;; :ID:       20230731T162509.072098
;; :END:
;; By default Emacs actually deletes files.  By setting this to t, you tell Emacs
;; to move a file to trash instead of actually deleting it.  This is better because
;; if you accidentally delete a file or discover you can still just go get your
;; file from the trash.
(setq delete-by-moving-to-trash t)
;; ** enable recursive minibuffer
;; :PROPERTIES:
;; :ID:       20230731T174221.389416
;; :END:
;; With this enabled, I can invoke the minibuffer while still being in the
;; minibuffer.  At the very least this is useful so that I can inspect which keys
;; are bound in the minibuffer.
(setq enable-recursive-minibuffers t)
;; ** recenter point if it goes 20 lines past what is visible
;; :PROPERTIES:
;; :ID:       20230731T204800.923611
;; :END:
;; Note that the following comment is taken from noctuid's config: "Recenter the
;; point if it goes greater than 20 lines past what is visible the default, 0, is
;; kind of annoying because it recenters even if you just go one line down from
;; the window bottom, but a higher value is nice to automatically recenter after
;; any bigger jump."
(setq scroll-conservatively 20)
;; ** don't show the startup screen
;; :PROPERTIES:
;; :ID:       20230731T205458.499736
;; :END:
;; By default Emacs displays [[][this startup screen]] at startup.  No thanks!  I
;; think these variables are all aliases for eachother.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;; ** skip fontification functions when there's input pending
;; :PROPERTIES:
;; :ID:       20230803T163524.316856
;; :END:
(setq redisplay-skip-fontification-on-input t)
;; ** don't update things on the screen as frequently
;; :PROPERTIES:
;; :ID:       20230801T055728.697987
;; :END:
;; This variable is.
;; https://github.com/hlissner/doom-emacs/blob/01aadd8900be45f912124d9d815d8790f540d38c/core/core.el#L177
(setq idle-update-delay 1)
;; ** don't make backups
;; :PROPERTIES:
;; :ID:       20230812T062001.541694
;; :END:
(setq make-backup-files nil)
;; ** backup files to trash
;; :PROPERTIES:
;; :ID:       20230812T062140.061699
;; :END:
(setq backup-directory-alist '((".*" . "~/.Trash")))
;; ** diable auto-save-mode
;; :PROPERTIES:
;; :ID:       20230812T062512.579521
;; :END:
(setq auto-save-default nil)
(auto-save-mode -1)
;; ** don't echo keystrokes
;; :PROPERTIES:
;; :ID:       20230731T205343.423280
;; :END:
;; By default emacs shows.
(setq echo-keystrokes 0)
;; ** ensure there's always a newline at the end of files
;; :PROPERTIES:
;; :ID:       20230731T162322.281287
;; :END:
;; Several linux programs require a newline at the end of a file, such as
;; chrontab--this is more or less what noctuid said and I'll take his word for
;; it.
(setq require-final-newline t)
;; ** set the tab-width to =4=; it's =8= by default
;; :PROPERTIES:
;; :ID:       20230803T154240.186291
;; :END:
(setq-default tab-width 4)
;; ** set the initial major mode to =fundamental-mode=
;; :PROPERTIES:
;; :ID:       20230803T154745.494981
;; :END:
;; This improve startup time because packages enabled for emacs-lisp-mode are not
;; loaded immediately.
(setq initial-major-mode 'fundamental-mode)
;; ** don't display message advertising gnu system
;; :PROPERTIES:
;; :ID:       20230807T000924.633977
;; :END:
;; They made the process of disabling this more difficult.
(advice-add #'display-startup-echo-area-message :around #'ignore)
;; ** disable initial scratch message
;; :PROPERTIES:
;; :ID:       20230807T000558.580301
;; :END:
;; Don't display any documentation--or any message at all--in the =*scratch*=
;; buffer.  Emacs by default displays [[][a message in the scratch buffer]].
(setq initial-scratch-message nil)
;; ** don't add indent
;; :PROPERTIES:
;; :ID:       20230803T154112.206542
;; :END:
(setq-default indent-tabs-mode nil)
;; ** don't beep
;; :PROPERTIES:
;; :ID:       20230731T162326.479823
;; :END:
;; This variable controls whether emacs makes a sound when certain events happen
;; such as invoking a binding that doesn't have anything bound to it or trying
;; to exceed the end of the buffer--things like that.  Personally, I don't want
;; such beeping.  Setting this variable to nil still result in beeping, emacs
;; just uses its default function.  Instead, to be disabled it must
;; be set to [[file:snapshots/helpful-command:ignore.png][ignore]].
(setq ring-bell-function #'ignore)
;; ** disable repeated error message functions
;; :PROPERTIES:
;; :ID:       20230815T235002.324826
;; :END:
;; When you try to move past the beginning and end of a buffer Emacs produces
;; error messages.
;; [[https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer][disable warnings]]
(defun oo-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (unless (memq (car data) '(buffer-read-only beginning-of-buffer end-of-buffer))
    (command-error-default-function data context caller)))

(setq command-error-function #'oo-command-error-function)
