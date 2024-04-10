;;; 03-init-package.el --- install packages -*- lexical-binding: t; -*-
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
;; This file sets up package.el and installs the packages that I use.
;;
;;; Code:
(require 'cl-lib)
(require 'package)
(require 'package-vc)

;; I want a clearer name.  When I saw the name "elpa" I had no idea what was in
;; that folder.
(setq package-user-dir (locate-user-emacs-file "packages/"))

(add-to-list 'package-archives '("gnu-elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("gnu-elpa" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; installed by default from Emacs 28 onwards
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-archive-priorities '(("melpa" . 10) ("gnu-elpa" . 9) ("nongnu" . 8)))

;; I do not know you might say this is too much boilerplate.  I could just be
;; more direct and set `package-selected-packages' to the packages I pushed
;; below.  But I like the flexibility.
(cl-pushnew 'which-key package-selected-packages)
(cl-pushnew 'no-littering package-selected-packages)
(cl-pushnew 'grugru package-selected-packages)
(cl-pushnew 'rainbow-delimiters package-selected-packages)
(cl-pushnew 'goto-chg package-selected-packages)
(cl-pushnew 'evil-goggles package-selected-packages)
(cl-pushnew 'macrostep package-selected-packages)
(cl-pushnew 'evil package-selected-packages)
(cl-pushnew 'evil-goggles package-selected-packages)
(cl-pushnew 'smartparens package-selected-packages)
(cl-pushnew 'lispy package-selected-packages)
(cl-pushnew 'lispyville package-selected-packages)
(cl-pushnew 'corfu package-selected-packages)
(cl-pushnew 'consult package-selected-packages)
(cl-pushnew 'burly package-selected-packages)
(cl-pushnew 'evil-surround package-selected-packages)
(cl-pushnew 'expand-region package-selected-packages)
(cl-pushnew 'helm package-selected-packages)
(cl-pushnew 'gcmh package-selected-packages)
(cl-pushnew 'meow package-selected-packages)
(cl-pushnew 'tempel package-selected-packages)
(cl-pushnew 'redacted package-selected-packages)
(cl-pushnew 'orderless package-selected-packages)
(cl-pushnew 'eros package-selected-packages)
(cl-pushnew 'eshell-up package-selected-packages)
(cl-pushnew 'eshell-z package-selected-packages)
(cl-pushnew 'emms package-selected-packages)
(cl-pushnew 'xr package-selected-packages)
(cl-pushnew 'super-save package-selected-packages)
(cl-pushnew 'helm-system-packages package-selected-packages)
(cl-pushnew 'fennel-mode package-selected-packages)
(cl-pushnew 'dirvish package-selected-packages)
(cl-pushnew 'captain package-selected-packages)
(cl-pushnew 'org-bookmark-heading package-selected-packages)
(cl-pushnew 'outshine package-selected-packages)
(cl-pushnew 'cape package-selected-packages)
(cl-pushnew 'aggressive-indent package-selected-packages)
(cl-pushnew 'ws-butler package-selected-packages)
(cl-pushnew 'magit package-selected-packages)
(cl-pushnew 'shut-up package-selected-packages)
(cl-pushnew 'vertico package-selected-packages)
(cl-pushnew 'esup package-selected-packages)
(cl-pushnew 'org-superstar package-selected-packages)
(cl-pushnew 'org-appear package-selected-packages)
(cl-pushnew 'idle-require package-selected-packages)
(cl-pushnew 'leuven-theme package-selected-packages)
(cl-pushnew 'modus-themes package-selected-packages)
(cl-pushnew 'monokai-theme package-selected-packages)
(cl-pushnew 'material-theme package-selected-packages)
(cl-pushnew 'doom-themes package-selected-packages)
(cl-pushnew 'evil-exchange package-selected-packages)
(cl-pushnew 'lgr package-selected-packages)
(cl-pushnew 'ef-themes package-selected-packages)
(cl-pushnew 'benchmark-init package-selected-packages)
;; (cl-pushnew 'embark package-selected-packages)
(cl-pushnew 'anaphora package-selected-packages)
;; (cl-pushnew 'buttercup package-selected-packages)

(cl-pushnew '(evil-magit :url "https://github.com/emacs-evil/evil-magit") package-vc-selected-packages)
(cl-pushnew '(outli :url "https://github.com/jdtsmith/outli") package-vc-selected-packages)
(cl-pushnew '(on :url "https://github.com/ajgrf/on.el") package-vc-selected-packages)

;; The function `package-install-selected-packages' does not activate the
;; packages which causes a problem fo rme.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; (logsym! package--initialized)

(unless package-archive-contents
  (package-refresh-contents))

;; This is kind of what `package-install-selected-packages' does, but it
;; messages and stuff and it does not show which package is not available, which
;; I did not like.
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (if (assq package package-archive-contents)
        (package-install package 'dont-select)
      (message "package %s is not available"))))

(package-vc-install-selected-packages)

(provide '03-init-package)
;;; 03-init-package.el ends here
