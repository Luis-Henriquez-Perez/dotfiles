;;; init-chezmoi.el --- initialize chezmoi -*- lexical-binding: t; -*-
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
;; Initialize chezmoi.
;;
;;; Code:
(require 'base)

(autoload #'chezmoi-find "chezmoi" nil t 'function)
(autoload #'chezmoi-write "chezmoi" nil t 'function)

;; I need the command to write the source from the target.  The command
;; =chezmoi-apply= does this but I would like it to do it automatically if I am
;; already editing a target-file.
(progn
  (defun after-save-hook&chezmoi-write-maybe
      (&rest args)
    (info! "Running hook %s..." 'after-save-hook&chezmoi-write-maybe)
    (condition-case err
        (progn "Add the Tempel Capf to `completion-at-point-functions'."
               (setq-local completion-at-point-functions
                           (cons
                            (function tempel-expand)
                            completion-at-point-functions)))
      (error
       (if oo-debug-p
           (signal
            (car err)
            (cdr err))
         (error! "Error %s calling %s in %s because of %s" 'after-save-hook 'after-save-hook&chezmoi-write-maybe
                 (car err)
                 (cdr err))))))
  (add-hook 'after-save-hook
            (function after-save-hook&chezmoi-write-maybe)
            nil nil))
;;; provide
(provide 'init-chezmoi)
;;; init-chezmoi.el ends here
