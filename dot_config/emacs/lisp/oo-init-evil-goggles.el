;;; oo-init-evil-goggles.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Here I aspire to make using `evil-goggles' completely lazy.  This package in
;; fact does not need to be loaded until just before using one of the
;; `evil-goggles' commands.  In theory I could just advise the appropriate
;; commands to before they are called, load `evil-goggles'.  You want to use
;; this even on the commands `evil-goggles' has by default.  The idea is you do
;; not even enable `evil-goggles' mode in a hook.
;; 
;; The thing is with setup that.
;;
;;; Code:
(defun! oo--generate-evil-goggle-code (command)
  "Return code to lazily setup `evil-goggles' for COMMAND."
  (set! name (intern (format "%s@enable-evil-goggles" command)))
  `((defun ,name (orig-fn &rest args)
      (require 'evil-goggles)
      (unless (bound-and-true-p evil-goggles-mode)
        (evil-goggles-mode 1))
      (alet (assoc ',command evil-goggles--commands)
        (adjoining! evil-goggles--commands (cons command (cdr it)) :test #'equal :key #'car))
      (advice-remove ',command #',name)
      (apply orig-fn args))
    (advice-add ',command :around #',name)))

;; Let me explain why this macro is needed.  O.K. so `evil-goggles' is a good
;; example.  The package `evil-goggles' would benefit from being lazy loaded.
;; Although its only needed just before certain commands are invoked the
;; standard way to load it is by running.
(defmacro setup! (key &rest args)
  "Cross-configuration macro."
  (macroexp-progn (apply #'oo--generate-evil-goggle-code args)))

(setup! :evil-goggles evil-yank)
(setup! :evil-goggles evil-delete)
(setup! :evil-goggles evil-change)
(setup! :evil-goggles evil-yank)
(setup! :evil-goggles evil-delete)
(setup! :evil-goggles evil-change)
(setup! :evil-goggles evil-delete)
(setup! :evil-goggles evil-delete)
(setup! :evil-goggles evil-substitute)
(setup! :evil-goggles evil-change)
(setup! :evil-goggles evil-join)

(setup! :evil-goggles lispyville-yank                            evil-yank)
(setup! :evil-goggles lispyville-delete                          evil-delete)
(setup! :evil-goggles lispyville-change                          evil-change)
(setup! :evil-goggles lispyville-yank-line                       evil-yank-line)
(setup! :evil-goggles lispyville-delete-line                     evil-delete-line)
(setup! :evil-goggles lispyville-change-line                     evil-change-line)
(setup! :evil-goggles lispyville-delete-char-or-splice           evil-delete-char)
(setup! :evil-goggles lispyville-delete-char-or-splice-backwards evil-delete-backward-char)
(setup! :evil-goggles lispyville-substitute                      evil-substitute)
(setup! :evil-goggles lispyville-change-whole-line               evil-change-whole-line)
(setup! :evil-goggles lispyville-join                            evil-join)

(setup! :evil-goggles evil-operator-eval evil-change)
(setup! :evil-goggles evil-operator-eval evil-change)

(+ 1 1)
;; This is the code in `evil-goggles' that actually makes the command active.
;; In my opinion, this should have been made into a function by the package.
;; Instead, the package seems to assume that those are the only commands you
;; will want to animate which considering the rich variety of packages in Emacs,
;; is short-sighted.
(dolist (command-cfg evil-goggles--commands)
  (let ((cmd (car command-cfg))
        (advice (plist-get (cdr command-cfg) :advice))
        (switch (plist-get (cdr command-cfg) :switch))
        (after  (plist-get (cdr command-cfg) :after)))
    (when (symbol-value switch)
      (advice-add cmd (if after :after :before) advice))))
;;; provide
(provide 'oo-init-evil-goggles)
;;; oo-init-evil-goggles.el ends here
