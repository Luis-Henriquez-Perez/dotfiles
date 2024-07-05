;;; oo-after-load-corfu.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is my configuration for corfu.
;;
;;; Code:
(require 'corfu)

;; https://github.com/minad/corfu/issues/12
(bind! corfu-map :ieg "<tab>"   #'corfu-next)
(bind! corfu-map :ieg [backtab] #'corfu-previous)
(bind! corfu-map :ieg "S-TAB"   #'corfu-previous)
(bind! corfu-map :ieg "C-;"     #'corfu-quick-complete)
(bind! corfu-map :ieg "C-j"     #'corfu-next)
(bind! corfu-map :ieg "C-k"     #'corfu-previous)
(bind! corfu-map :ieg "C-p"     #'corfu-previous)
(bind! corfu-map :ieg ";"       #'corfu-quick-complete)
(bind! corfu-map :ieg "SPC"     #'corfu-insert)

;; When using evil, neither `corfu-map' nor `tempel-map' bindings will work
;; because the maps are overridden by evil.  In order for them to work, we need
;; to boost give the maps greater precedence.
(with-eval-after-load 'evil
  (with-no-warnings
    (evil-make-overriding-map corfu-map)
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)))

(opt! corfu-quick1 "ajskdlghty")
(opt! corfu-quick2 "ajskdlghty")

(hook! corfu-mode-hook&corfu-history-mode)

;; TODO: make it so moving on a candidate if I press espace insert that candidate.
(opt! corfu-preview-current t)

(opt! corfu-preselect-first t)

(opt! corfu-quit-at-boundary nil)

(opt! corfu-auto t)

(opt! corfu-auto-delay 0.1)

(opt! corfu-auto-prefix 1)

(opt! corfu-bar-width 0)
;;; provide
(provide 'oo-after-load-corfu)
;;; oo-after-load-corfu.el ends here
