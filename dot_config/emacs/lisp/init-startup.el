;;; init-startup.el --- initialize startup -*- lexical-binding: t; -*-
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
;; Initialize startup.
;;
;;; Code:
;;;; don't show the startup screen
;; By default Emacs displays [[][this startup screen]] at startup.  No thanks!  I
;; think these variables are all aliases for eachother.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;;;; stop creating =auto-save-list= directory
;; See [[https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created][#18677]].
(setq auto-save-list-file-prefix nil)
;;;; set the initial major mode to =fundamental-mode=
;; This improve startup time because packages enabled for emacs-lisp-mode are not
;; loaded immediately.
(setq initial-major-mode 'fundamental-mode)
;;;; disable initial scratch message
;; Don't display any documentation--or any message at all--in the =*scratch*=
;; buffer.  Emacs by default displays [[][a message in the scratch buffer]].
(setq initial-scratch-message nil)
;;;; don't display message advertising gnu system
;; They made the process of disabling this more difficult.
(advice-add #'display-startup-echo-area-message :around #'ignore)
;;;; emacs-startup-hook
(defhook! restore-startup-values (emacs-startup-hook)
  [:depth 91]
  (require 'oo-init-modeline)
  (setq file-name-handler-alist (get-register :file-name-handler-alist))
  (setq gc-cons-threshold (* 32 1024 1024))
  (run-with-timer 5 nil #'oo-lower-garbage-collection))

;; (defhook! init-after-load-functions (emacs-startup-hook)
;;   "Call `oo-call-after-load-functions' once.
;; Also add it as a hook to `after-load-functions' so that it is invoked whenever a
;; file is loaded."
;;   (oo-call-after-load-functions)
;;   (hook! after-load-functions oo-call-after-load-functions))
;;; provide
(provide 'init-startup)
;;; init-startup.el ends here
