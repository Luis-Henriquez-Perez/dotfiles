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

