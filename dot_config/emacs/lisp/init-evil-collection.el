;;; init-evil-collection.el --- Initialize `evil-collection' -*- lexical-binding: t; -*-
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
;; Initialize `evil-collection'.
;;
;;; Code:
(autoload #'evil-collection-2048-game-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-2048-game-hook evil-collection-2048-game-setup)

(autoload #'evil-collection-ag-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ag-hook evil-collection-ag-setup)

(autoload #'evil-collection-alchemist-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-alchemist-hook evil-collection-alchemist-setup)

(autoload #'evil-collection-anaconda-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-anaconda-mode-hook evil-collection-anaconda-mode-setup)

(autoload #'evil-collection-apropos-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-apropos-hook evil-collection-apropos-setup)

(autoload #'evil-collection-arc-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-arc-mode-hook evil-collection-arc-mode-setup)

(autoload #'evil-collection-atomic-chrome-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-atomic-chrome-hook evil-collection-atomic-chrome-setup)

(autoload #'evil-collection-auto-package-update-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-auto-package-update-hook evil-collection-auto-package-update-setup)

(autoload #'evil-collection-beginend-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-beginend-hook evil-collection-beginend-setup)

(autoload #'evil-collection-bluetooth-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-bluetooth-hook evil-collection-bluetooth-setup)

(autoload #'evil-collection-bm-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-bm-hook evil-collection-bm-setup)

(autoload #'evil-collection-bookmark-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-bookmark-hook evil-collection-bookmark-setup)

(autoload #'evil-collection-buff-menu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-buff-menu-hook evil-collection-buff-menu-setup)

(autoload #'evil-collection-bufler-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-bufler-hook evil-collection-bufler-setup)

(autoload #'evil-collection-calc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-calc-hook evil-collection-calc-setup)

(autoload #'evil-collection-calendar-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-calendar-hook evil-collection-calendar-setup)

(autoload #'evil-collection-cider-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-cider-hook evil-collection-cider-setup)

(autoload #'evil-collection-cmake-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-cmake-mode-hook evil-collection-cmake-mode-setup)

(autoload #'evil-collection-color-rg-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-color-rg-hook evil-collection-color-rg-setup)

(autoload #'evil-collection-comint-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-comint-hook evil-collection-comint-setup)

(autoload #'evil-collection-company-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-company-hook evil-collection-company-setup)

(autoload #'evil-collection-compile-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-compile-hook evil-collection-compile-setup)

(autoload #'evil-collection-consult-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-consult-hook evil-collection-consult-setup)

(autoload #'evil-collection-corfu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-corfu-hook evil-collection-corfu-setup)

(autoload #'evil-collection-crdt-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-crdt-hook evil-collection-crdt-setup)

(autoload #'evil-collection-csv-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-csv-hook evil-collection-csv-setup)

(autoload #'evil-collection-custom-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-custom-hook evil-collection-custom-setup)

(autoload #'evil-collection-cus-theme-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-cus-theme-hook evil-collection-cus-theme-setup)

(autoload #'evil-collection-dashboard-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-dashboard-hook evil-collection-dashboard-setup)

(autoload #'evil-collection-daemons-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-daemons-hook evil-collection-daemons-setup)

(autoload #'evil-collection-deadgrep-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-deadgrep-hook evil-collection-deadgrep-setup)

(autoload #'evil-collection-debbugs-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-debbugs-hook evil-collection-debbugs-setup)

(autoload #'evil-collection-debug-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-debug-hook evil-collection-debug-setup)

(autoload #'evil-collection-devdocs-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-devdocs-hook evil-collection-devdocs-setup)

(autoload #'evil-collection-dictionary-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-dictionary-hook evil-collection-dictionary-setup)

(autoload #'evil-collection-diff-hl-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-diff-hl-hook evil-collection-diff-hl-setup)

(autoload #'evil-collection-diff-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-diff-mode-hook evil-collection-diff-mode-setup)

(autoload #'evil-collection-dired-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-dired-hook evil-collection-dired-setup)

(autoload #'evil-collection-dired-sidebar-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-dired-sidebar-hook evil-collection-dired-sidebar-setup)

(autoload #'evil-collection-disk-usage-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-disk-usage-hook evil-collection-disk-usage-setup)

(autoload #'evil-collection-distel-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-distel-hook evil-collection-distel-setup)

(autoload #'evil-collection-doc-view-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-doc-view-hook evil-collection-doc-view-setup)

(autoload #'evil-collection-docker-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-docker-hook evil-collection-docker-setup)

(autoload #'evil-collection-eat-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eat-hook evil-collection-eat-setup)

(autoload #'evil-collection-ebib-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ebib-hook evil-collection-ebib-setup)

(autoload #'evil-collection-ebuku-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ebuku-hook evil-collection-ebuku-setup)

(autoload #'evil-collection-edbi-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-edbi-hook evil-collection-edbi-setup)

(autoload #'evil-collection-edebug-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-edebug-hook evil-collection-edebug-setup)

(autoload #'evil-collection-ediff-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ediff-hook evil-collection-ediff-setup)

(autoload #'evil-collection-eglot-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eglot-hook evil-collection-eglot-setup)

(autoload #'evil-collection-elpaca-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-elpaca-hook evil-collection-elpaca-setup)

(autoload #'evil-collection-ement-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ement-hook evil-collection-ement-setup)

(autoload #'evil-collection-explain-pause-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-explain-pause-mode-hook evil-collection-explain-pause-mode-setup)

(autoload #'evil-collection-eldoc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eldoc-hook evil-collection-eldoc-setup)

(autoload #'evil-collection-elfeed-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-elfeed-hook evil-collection-elfeed-setup)

(autoload #'evil-collection-elisp-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-elisp-mode-hook evil-collection-elisp-mode-setup)

(autoload #'evil-collection-elisp-refs-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-elisp-refs-hook evil-collection-elisp-refs-setup)

(autoload #'evil-collection-elisp-slime-nav-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-elisp-slime-nav-hook evil-collection-elisp-slime-nav-setup)

(autoload #'evil-collection-embark-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-embark-hook evil-collection-embark-setup)

(autoload #'evil-collection-emms-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-emms-hook evil-collection-emms-setup)

(autoload #'evil-collection-emoji-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-emoji-hook evil-collection-emoji-setup)

(autoload #'evil-collection-epa-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-epa-hook evil-collection-epa-setup)

(autoload #'evil-collection-ert-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ert-hook evil-collection-ert-setup)

(autoload #'evil-collection-eshell-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eshell-hook evil-collection-eshell-setup)

(autoload #'evil-collection-eval-sexp-fu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eval-sexp-fu-hook evil-collection-eval-sexp-fu-setup)

(autoload #'evil-collection-evil-mc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-evil-mc-hook evil-collection-evil-mc-setup)

(autoload #'evil-collection-eww-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-eww-hook evil-collection-eww-setup)

(autoload #'evil-collection-fanyi-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-fanyi-hook evil-collection-fanyi-setup)

(autoload #'evil-collection-finder-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-finder-hook evil-collection-finder-setup)

(autoload #'evil-collection-flycheck-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-flycheck-hook evil-collection-flycheck-setup)

(autoload #'evil-collection-flymake-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-flymake-hook evil-collection-flymake-setup)

(autoload #'evil-collection-forge-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-forge-hook evil-collection-forge-setup)

(autoload #'evil-collection-free-keys-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-free-keys-hook evil-collection-free-keys-setup)

(autoload #'evil-collection-geiser-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-geiser-hook evil-collection-geiser-setup)

(autoload #'evil-collection-ggtags-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ggtags-hook evil-collection-ggtags-setup)

(autoload #'evil-collection-git-timemachine-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-git-timemachine-hook evil-collection-git-timemachine-setup)

(autoload #'evil-collection-gited-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-gited-hook evil-collection-gited-setup)

(autoload #'evil-collection-gnus-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-gnus-hook evil-collection-gnus-setup)

(autoload #'evil-collection-go-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-go-mode-hook evil-collection-go-mode-setup)

(autoload #'evil-collection-grep-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-grep-hook evil-collection-grep-setup)

(autoload #'evil-collection-guix-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-guix-hook evil-collection-guix-setup)

(autoload #'evil-collection-hackernews-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-hackernews-hook evil-collection-hackernews-setup)

(autoload #'evil-collection-helm-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-helm-hook evil-collection-helm-setup)

(autoload #'evil-collection-help-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-help-hook evil-collection-help-setup)

(autoload #'evil-collection-helpful-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-helpful-hook evil-collection-helpful-setup)

(autoload #'evil-collection-hg-histedit-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-hg-histedit-hook evil-collection-hg-histedit-setup)

(autoload #'evil-collection-hungry-delete-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-hungry-delete-hook evil-collection-hungry-delete-setup)

(autoload #'evil-collection-ibuffer-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ibuffer-hook evil-collection-ibuffer-setup)

(autoload #'evil-collection-image-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-image-hook evil-collection-image-setup)

(autoload #'evil-collection-image-dired-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-image-dired-hook evil-collection-image-dired-setup)

(autoload #'evil-collection-image+-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-image+-hook evil-collection-image+-setup)

(autoload #'evil-collection-imenu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-imenu-hook evil-collection-imenu-setup)

(autoload #'evil-collection-imenu-list-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-imenu-list-hook evil-collection-imenu-list-setup)

(autoload #'evil-collection-indent-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-indent-hook evil-collection-indent-setup)

(autoload #'evil-collection-indium-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-indium-hook evil-collection-indium-setup)

(autoload #'evil-collection-info-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-info-hook evil-collection-info-setup)

(autoload #'evil-collection-ivy-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ivy-hook evil-collection-ivy-setup)

(autoload #'evil-collection-js2-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-js2-mode-hook evil-collection-js2-mode-setup)

(autoload #'evil-collection-leetcode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-leetcode-hook evil-collection-leetcode-setup)

(autoload #'evil-collection-lispy-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-lispy-hook evil-collection-lispy-setup)

(autoload #'evil-collection-lms-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-lms-hook evil-collection-lms-setup)

(autoload #'evil-collection-log-edit-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-log-edit-hook evil-collection-log-edit-setup)

(autoload #'evil-collection-log-view-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-log-view-hook evil-collection-log-view-setup)

(autoload #'evil-collection-lsp-ui-imenu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-lsp-ui-imenu-hook evil-collection-lsp-ui-imenu-setup)

(autoload #'evil-collection-lua-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-lua-mode-hook evil-collection-lua-mode-setup)

(autoload #'evil-collection-kotlin-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-kotlin-mode-hook evil-collection-kotlin-mode-setup)

(autoload #'evil-collection-macrostep-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-macrostep-hook evil-collection-macrostep-setup)

(autoload #'evil-collection-man-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-man-hook evil-collection-man-setup)

(autoload #'evil-collection-magit-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-magit-hook evil-collection-magit-setup)

(autoload #'evil-collection-magit-repos-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-magit-repos-hook evil-collection-magit-repos-setup)

(autoload #'evil-collection-magit-section-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-magit-section-hook evil-collection-magit-section-setup)

(autoload #'evil-collection-magit-todos-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-magit-todos-hook evil-collection-magit-todos-setup)

(autoload #'evil-collection-markdown-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-markdown-mode-hook evil-collection-markdown-mode-setup)

(autoload #'evil-collection-monky-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-monky-hook evil-collection-monky-setup)

(autoload #'evil-collection-mpc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-mpc-hook evil-collection-mpc-setup)

(autoload #'evil-collection-mpdel-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-mpdel-hook evil-collection-mpdel-setup)

(autoload #'evil-collection-mpdired-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-mpdired-hook evil-collection-mpdired-setup)

(autoload #'evil-collection-mu4e-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-mu4e-hook evil-collection-mu4e-setup)

(autoload #'evil-collection-mu4e-conversation-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-mu4e-conversation-hook evil-collection-mu4e-conversation-setup)

(autoload #'evil-collection-neotree-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-neotree-hook evil-collection-neotree-setup)

(autoload #'evil-collection-newsticker-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-newsticker-hook evil-collection-newsticker-setup)

(autoload #'evil-collection-notmuch-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-notmuch-hook evil-collection-notmuch-setup)

(autoload #'evil-collection-nov-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-nov-hook evil-collection-nov-setup)

(autoload #'evil-collection-omnisharp-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-omnisharp-hook evil-collection-omnisharp-setup)

(autoload #'evil-collection-org-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-org-hook evil-collection-org-setup)

(autoload #'evil-collection-org-present-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-org-present-hook evil-collection-org-present-setup)

(autoload #'evil-collection-org-roam-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-org-roam-hook evil-collection-org-roam-setup)

(autoload #'evil-collection-osx-dictionary-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-osx-dictionary-hook evil-collection-osx-dictionary-setup)

(autoload #'evil-collection-outline-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-outline-hook evil-collection-outline-setup)

(autoload #'evil-collection-p4-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-p4-hook evil-collection-p4-setup)

(autoload #'evil-collection-package-menu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-package-menu-hook evil-collection-package-menu-setup)

(autoload #'evil-collection-pass-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-pass-hook evil-collection-pass-setup)

(autoload #'evil-collection-pdf-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-pdf-hook evil-collection-pdf-setup)

(autoload #'evil-collection-popup-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-popup-hook evil-collection-popup-setup)

(autoload #'evil-collection-proced-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-proced-hook evil-collection-proced-setup)

(autoload #'evil-collection-process-menu-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-process-menu-hook evil-collection-process-menu-setup)

(autoload #'evil-collection-prodigy-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-prodigy-hook evil-collection-prodigy-setup)

(autoload #'evil-collection-profiler-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-profiler-hook evil-collection-profiler-setup)

(autoload #'evil-collection-python-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-python-hook evil-collection-python-setup)

(autoload #'evil-collection-quickrun-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-quickrun-hook evil-collection-quickrun-setup)

(autoload #'evil-collection-racer-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-racer-hook evil-collection-racer-setup)

(autoload #'evil-collection-racket-describe-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-racket-describe-hook evil-collection-racket-describe-setup)

(autoload #'evil-collection-realgud-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-realgud-hook evil-collection-realgud-setup)

(autoload #'evil-collection-reftex-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-reftex-hook evil-collection-reftex-setup)

(autoload #'evil-collection-replace-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-replace-hook evil-collection-replace-setup)

(autoload #'evil-collection-restclient-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-restclient-hook evil-collection-restclient-setup)

(autoload #'evil-collection-rg-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-rg-hook evil-collection-rg-setup)

(autoload #'evil-collection-ripgrep-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ripgrep-hook evil-collection-ripgrep-setup)

(autoload #'evil-collection-rjsx-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-rjsx-mode-hook evil-collection-rjsx-mode-setup)

(autoload #'evil-collection-robe-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-robe-hook evil-collection-robe-setup)

(autoload #'evil-collection-rtags-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-rtags-hook evil-collection-rtags-setup)

(autoload #'evil-collection-ruby-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ruby-mode-hook evil-collection-ruby-mode-setup)

(autoload #'evil-collection-scheme-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-scheme-hook evil-collection-scheme-setup)

(autoload #'evil-collection-scroll-lock-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-scroll-lock-hook evil-collection-scroll-lock-setup)

(autoload #'evil-collection-selectrum-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-selectrum-hook evil-collection-selectrum-setup)

(autoload #'evil-collection-sh-script-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-sh-script-hook evil-collection-sh-script-setup)

(autoload #'evil-collection-shortdoc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-shortdoc-hook evil-collection-shortdoc-setup)

(autoload #'evil-collection-simple-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-simple-hook evil-collection-simple-setup)

(autoload #'evil-collection-simple-mpc-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-simple-mpc-hook evil-collection-simple-mpc-setup)

(autoload #'evil-collection-slime-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-slime-hook evil-collection-slime-setup)

(autoload #'evil-collection-sly-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-sly-hook evil-collection-sly-setup)

(autoload #'evil-collection-snake-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-snake-hook evil-collection-snake-setup)

(autoload #'evil-collection-so-long-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-so-long-hook evil-collection-so-long-setup)

(autoload #'evil-collection-speedbar-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-speedbar-hook evil-collection-speedbar-setup)

(autoload #'evil-collection-tab-bar-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tab-bar-hook evil-collection-tab-bar-setup)

(autoload #'evil-collection-tablist-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tablist-hook evil-collection-tablist-setup)

(autoload #'evil-collection-tabulated-list-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tabulated-list-hook evil-collection-tabulated-list-setup)

(autoload #'evil-collection-tar-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tar-mode-hook evil-collection-tar-mode-setup)

(autoload #'evil-collection-telega-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-telega-hook evil-collection-telega-setup)

(autoload #'evil-collection-term-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-term-hook evil-collection-term-setup)

(autoload #'evil-collection-tetris-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tetris-hook evil-collection-tetris-setup)

(autoload #'evil-collection-thread-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-thread-hook evil-collection-thread-setup)

(autoload #'evil-collection-tide-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tide-hook evil-collection-tide-setup)

(autoload #'evil-collection-timer-list-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-timer-list-hook evil-collection-timer-list-setup)

(autoload #'evil-collection-transmission-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-transmission-hook evil-collection-transmission-setup)

(autoload #'evil-collection-trashed-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-trashed-hook evil-collection-trashed-setup)

(autoload #'evil-collection-tuareg-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-tuareg-hook evil-collection-tuareg-setup)

(autoload #'evil-collection-typescript-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-typescript-mode-hook evil-collection-typescript-mode-setup)

(autoload #'evil-collection-vc-annotate-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vc-annotate-hook evil-collection-vc-annotate-setup)

(autoload #'evil-collection-vc-dir-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vc-dir-hook evil-collection-vc-dir-setup)

(autoload #'evil-collection-vc-git-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vc-git-hook evil-collection-vc-git-setup)

(autoload #'evil-collection-vdiff-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vdiff-hook evil-collection-vdiff-setup)

(autoload #'evil-collection-vertico-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vertico-hook evil-collection-vertico-setup)

(autoload #'evil-collection-view-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-view-hook evil-collection-view-setup)

(autoload #'evil-collection-vlf-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vlf-hook evil-collection-vlf-setup)

(autoload #'evil-collection-vterm-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vterm-hook evil-collection-vterm-setup)

(autoload #'evil-collection-vundo-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-vundo-hook evil-collection-vundo-setup)

(autoload #'evil-collection-w3m-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-w3m-hook evil-collection-w3m-setup)

(autoload #'evil-collection-wdired-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-wdired-hook evil-collection-wdired-setup)

(autoload #'evil-collection-wgrep-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-wgrep-hook evil-collection-wgrep-setup)

(autoload #'evil-collection-which-key-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-which-key-hook evil-collection-which-key-setup)

(autoload #'evil-collection-woman-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-woman-hook evil-collection-woman-setup)

(autoload #'evil-collection-xref-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-xref-hook evil-collection-xref-setup)

(autoload #'evil-collection-xwidget-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-xwidget-hook evil-collection-xwidget-setup)

(autoload #'evil-collection-yaml-mode-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-yaml-mode-hook evil-collection-yaml-mode-setup)

(autoload #'evil-collection-youtube-dl-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-youtube-dl-hook evil-collection-youtube-dl-setup)

(autoload #'evil-collection-zmusic-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-zmusic-hook evil-collection-zmusic-setup)

(autoload #'evil-collection-ztree-setup "evil-collection" nil nil 'function)
(hook! oo-after-load-ztree-hook evil-collection-ztree-setup)
;;; provide
(provide 'init-evil-collection)
;;; init-evil-collection.el ends here
