(load "config.el")

;; The [[][load-path]] is a list of paths that emacs uses to find features it
;; can load.
(push (expand-file-name "lisp" user-emacs-directory) load-path)
;; ** =require= the main files
;; This file contains the defaults.  I had been using =el-init= but for just
;; manually load.
(require 'oo-defaults)

;; This file bootstraps `elpaca'.  Still a work in progress.
(require 'oo-bootstrap-elpaca)

