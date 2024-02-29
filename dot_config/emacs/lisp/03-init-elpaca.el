;;; oo-elpaca-lib.el --- elpaca setup -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/Luis-Henriquez-Perez/dotfiles
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
;;; requirements
(require '02-base-lib)
;;; bootstrap
(defvar elpaca-installer-version 0.6)
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
(elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref "956c81b"
                 :files (:defaults "extensions/vertico-buffer.el" "extensions/vertico-quick.el" "extensions/vertico-directory.el")))
(elpaca evil-cleverparens)
(elpaca which-key)
(elpaca no-littering)
(elpaca grugru)
(elpaca rainbow-delimiters)
(elpaca goto-chg)
(elpaca macrostep)
(elpaca evil)
(elpaca evil-goggles)
(elpaca smartparens)
(elpaca lispy)
(elpaca lispyville)
(elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref "a59c41d"
               :files (:defaults "extensions/corfu-history.el" "extensions/corfu-quick.el")))
(elpaca consult)
(elpaca burly)
(elpaca evil-surround)
(elpaca expand-region)
(elpaca helm)
(elpaca gcmh)
(elpaca meow)
(elpaca tempel)
(elpaca redacted)
(elpaca orderless)
(elpaca eros)
(elpaca helpful)
(elpaca eshell-up)
(elpaca eshell-z)
(elpaca emms)
(elpaca xr)
(elpaca super-save)
(elpaca helm-system-packages)
(elpaca fennel-mode)
(elpaca dirvish)
(elpaca captain)
(elpaca org-bookmark-heading)
(elpaca outshine)
(elpaca cape)
(elpaca aggressive-indent)
(elpaca ws-butler)
(elpaca magit)
(elpaca shut-up)
(elpaca esup)
(elpaca org-superstar)
(elpaca org-appear)
(elpaca idle-require)
(elpaca marginalia)
(elpaca leuven-theme)
(elpaca modus-themes)
(elpaca monokai-theme)
(elpaca material-theme)
(elpaca doom-themes)
(elpaca evil-exchange)
(elpaca lgr)
(elpaca anaphora)
(elpaca ace-jump-helm-line)
(elpaca s)
(elpaca standard-themes)
(elpaca benchmark-init)
(elpaca dashboard)
(elpaca github-theme)
(elpaca evil-easymotion)
(elpaca (on :host github :repo "ajgrf/on.el"))
;;; process
(elpaca-process-queues)
(elpaca-wait)
;;; provide
(provide '03-init-elpaca)
