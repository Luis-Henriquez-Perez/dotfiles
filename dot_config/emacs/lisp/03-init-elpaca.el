;;; 03-init-elpaca.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is where I use elpaca to install all of my packages.
;;
;;; Code:
;;; requirements
(require '02-base-lib)
;;; bootstrap
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca-test/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
;; (add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;;; do not process too many orders at once
;; Doing so causes me performance problems.
(setq elpaca-queue-limit 10)
;; Wait longer.
(setq elpaca-wait-interval 0.2)
;;; recipes
(elpaca evil-textobj-line)
(elpaca ace-jump-helm-line)
(elpaca aggressive-indent)
(elpaca ah)
(elpaca anaphora)
(elpaca benchmark-init)
;; TODO: add bindings for lorem ipsum.
(elpaca burly)
(elpaca buttercup)
;; Eldev complains about `buttercup' already being loaded.  So I have to think
;; about how I will go about installing it for normal usage.  In any case the
;; only reason I want it is for proper indenting and syntax highlighting when I
;; am actually editing my test files.
(elpaca cape)
(elpaca (captain :repo "git://git.sv.gnu.org/emacs/elpa" :branch "externals/captain" :ref "364ee98"))
(elpaca chezmoi)
(elpaca consult)
(elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref ...))
(elpaca dashboard)
(elpaca dirvish)
(elpaca doom-themes)
(elpaca eat)
(elpaca ef-themes)
(elpaca embark)
(elpaca emms)
(elpaca eros)
(elpaca eshell-up)
(elpaca eshell-z)
(elpaca esup)
(elpaca evil)
(elpaca evil-cleverparens)
(elpaca evil-easymotion)
(elpaca evil-exchange)
(elpaca evil-goggles)
(elpaca (evil-magit :fetcher github :repo "emacs-evil/evil-magit" :ref "f4a8c8d"))
(elpaca evil-surround)
(elpaca expand-region)
(elpaca fennel-mode)
(elpaca filladapt)
(elpaca fit-text-scale)
(elpaca gcmh)
(elpaca git-gutter)
(elpaca github-theme)
(elpaca goto-chg)
(elpaca grugru)
(elpaca helm)
(elpaca helm-system-packages)
(elpaca helpful)
(elpaca highlight-quoted)
(elpaca humanoid-themes)
(elpaca idle-require)
(elpaca leuven-theme)
(elpaca lgr)
(elpaca lispy)
(elpaca lispyville)
(elpaca lorem-ipsum)
(elpaca macrostep)
(elpaca magit)
(elpaca marginalia)
(elpaca material-theme)
(elpaca meow)
(elpaca modus-themes)
(elpaca monokai-theme)
(elpaca no-littering)
(elpaca (on :host github :repo "ajgrf/on.el"))
(elpaca orderless)
(elpaca org-appear)
(elpaca org-bookmark-heading)
(elpaca org-superstar)
(elpaca (outli :fetcher github :repo "jdtsmith/outli"))
(elpaca rainbow-delimiters)
(elpaca redacted)
(elpaca s)
(elpaca shut-up)
(elpaca smartparens)
(elpaca spacemacs-theme)
(elpaca ssh-agency)
(elpaca standard-themes)
(elpaca super-save)
(elpaca tempel)
(elpaca transpose-frame)
(elpaca treepy)
(elpaca vc-auto-commit)
(elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref ...))
(elpaca which-key)
(elpaca ws-butler)
(elpaca xr)
;;; process
(elpaca-process-queues)
(elpaca-wait)
;;; provide
(provide '03-init-elpaca)
;;; 03-init-elpaca.el ends here
