;;; 03-init-straight.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
;; Use straight.el.

;;; Bootstrap straight
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-check-for-modifications nil)
(setq use-package-always-defer t)
(defvar bootstrap-version)
(let* ((straight-repo-dir
        (expand-file-name "straight/repos" user-emacs-directory))
       (bootstrap-file
        (concat straight-repo-dir "/straight.el/bootstrap.el"))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (concat
      "mkdir -p " straight-repo-dir " && "
      "git -C " straight-repo-dir " clone "
      "https://github.com/raxod502/straight.el.git && "
      "git -C " straight-repo-dir " checkout 2d407bc")))
  (load bootstrap-file nil 'nomessage))
;; This is a variable that has been renamed but straight still refers when
;; doing :sraight (:no-native-compile t)
(setq comp-deferred-compilation-black-list nil)
;;; Clone the packages and then set them to proper version
;; (for! (package packages)
;;   (straight-vc-clone-package)
;;   (straight-vc-checkout :commit)
;;   ;; Then we build the packages.
;;   )
;;; Enable the packages
(straight-use-package '(vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref "956c81b"
                                :files (:defaults "extensions/vertico-buffer.el" "extensions/vertico-quick.el" "extensions/vertico-directory.el")))
(straight-use-package 'evil-cleverparens)
(straight-use-package 'which-key)
(straight-use-package 'no-littering)
(straight-use-package 'grugru)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'goto-chg)
(straight-use-package 'macrostep)
(straight-use-package 'evil)
(straight-use-package 'evil-goggles)
(straight-use-package 'smartparens)
(straight-use-package 'lispy)
(straight-use-package 'lispyville)
(straight-use-package '(corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref "a59c41d"
               :files (:defaults "extensions/corfu-history.el" "extensions/corfu-quick.el")))
(straight-use-package 'consult)
(straight-use-package 'burly)
(straight-use-package 'evil-surround)
(straight-use-package 'expand-region)
(straight-use-package 'helm)
(straight-use-package 'gcmh)
(straight-use-package 'meow)
(straight-use-package 'tempel)
(straight-use-package 'redacted)
(straight-use-package 'orderless)
(straight-use-package 'eros)
(straight-use-package 'helpful)
(straight-use-package 'eshell-up)
(straight-use-package 'eshell-z)
(straight-use-package 'emms)
(straight-use-package 'xr)
(straight-use-package 'super-save)
(straight-use-package 'helm-system-packages)
(straight-use-package 'fennel-mode)
(straight-use-package 'dirvish)
(straight-use-package 'captain)
(straight-use-package 'org-bookmark-heading)
(straight-use-package 'outshine)
(straight-use-package 'cape)
(straight-use-package 'aggressive-indent)
(straight-use-package 'ws-butler)
(straight-use-package 'magit)
(straight-use-package 'shut-up)
(straight-use-package 'esup)
(straight-use-package 'org-superstar)
(straight-use-package 'org-appear)
(straight-use-package 'idle-require)
(straight-use-package 'marginalia)
(straight-use-package 'leuven-theme)
(straight-use-package 'modus-themes)
(straight-use-package 'monokai-theme)
(straight-use-package 'material-theme)
(straight-use-package 'doom-themes)
(straight-use-package 'evil-exchange)
(straight-use-package 'lgr)
(straight-use-package 'anaphora)
(straight-use-package 'ace-jump-helm-line)
(straight-use-package 's)
(straight-use-package 'standard-themes)
(straight-use-package 'benchmark-init)
(straight-use-package 'general)
(straight-use-package 'dashboard)
(straight-use-package 'evil-easymotion)
(straight-use-package '(on :host github :repo "ajgrf/on.el"))

(provide '03-init-straight)
;;; 03-init-straight.el ends here
