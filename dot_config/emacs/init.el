;; -*- lexical-binding: t -*-

;; This variable controls how often.  Setting it to =most-positive-fixnum=, a very big
;; number, essentially disables garbage collection.  The garbage collection is later
;; reset to a reasonable value.
(setq gc-cons-threshold most-positive-fixnum)

;; The variable `oo-debug-p' is snatched from [[https://github.com/hlissner/doom-emacs][Doom's]] [[https://github.com/hlissner/doom-emacs/blob/develop/core/core.el][doom-debug-mode]].  The point of this variable is
;; to serve as an indicator of whether the current Emacs instance is run for
;; debugging.  When Emacs is set up for debugging it prints out many messages about
;; what its doing via [[hfn:void-log][oo-log]].
(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")

;; It's useful to store directories which I reference frequently in variables and
;; functions.  This way I can reference the full path.  Certain directories are
;; important; and I end up referencing them alot.  One of these is my
;; cache directory.
(defvar oo-cache-dir (concat user-emacs-directory "cache/")
  "Directory containing files used for caching information.")

(when (not (or (daemonp) noninteractive init-file-debug))
  ;; During startup emacs renders several messages.  The messages may be important so
  ;; we don't want to get rid of them altogether.  This code prevents these message
  ;; from flashing on the screen.  However they are still logged to the =*messages*=
  ;; buffer.
  (when (display-graphic-p)
    (setq-default inhibit-redisplay t)
    (setq-default inhibit-message t)
    (defun reset-inhibit-vars ()
      (setq-default inhibit-redisplay nil)
      (setq-default inhibit-message nil)
      (redraw-frame))
    (add-hook 'window-setup-hook #'reset-inhibit-vars)
    (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
      (and init-file-had-error (reset-inhibit-vars))))

  ;; Suppress file handlers operations at startup. The value of `file-name-handler-alist' is
  ;; consulted on each call to `require' and `load'. Here I disable it (set it to nil) and schedule it
  ;; to be re-enabled after startup. I got this from centaur emacs.
  (defvar original-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (defun emacs-startup-hook&restore-file-name-handler-alist ()
    (setq file-name-handler-alist original-file-name-handler-alist)
    (makunbound 'original-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'emacs-startup-hook&restore-file-name-handler-alist))

;; Put the base directory into the `load-path', making sure it's at the front.
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;; Set the settings as soon as possible so we can avoid any GUI display.
(require 'oo-base-settings)

;; Add the base directory to the load-path.
(let (font)
  (setq font (or (font-spec :name "Iosevka Comfy Wide"
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
  (set-face-attribute 'default nil :font font))

(require 'oo-base-packages)

(oo-bootstrap-packages (locate-user-emacs-file "packages/") (locate-user-emacs-file "recipes"))

(require 'oo-base-library)

;; (el-init-load)
