;;; oo-base-variables.el --- Variables -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;;
;; Created: 02 Feb 2024
;;
;; URL:
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
;;; Commentary:
;;
;; This file defines constants.  It is modeled after
;; https://github.com/d12frosted/environment/tree/master/emacs.
;;
;;; Code:

(defconst oo-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a value of $HOME.")

(defconst oo-config-dir (file-name-as-directory
                         (or (getenv "XDG_CONFIG_HOME")
                             (concat oo-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst oo-emacs-dir (file-name-as-directory
                        (expand-file-name "emacs/" oo-config-dir))
  "The path to this Emacs directory.")

;; (defconst path-local-dir (concat (file-name-as-directory
;;                                   (or (getenv "XDG_CACHE_HOME")
;;                                       (concat oo-home-dir ".cache")))
;;                                  "emacs/")
;;   "The root directory for local Emacs files.

;; Use this as permanent storage for files that are safe to share
;; across systems.")

;; (defconst oo-etc-dir (concat oo-local-dir "etc/")
;;   "Directory for non-volatile storage.

;; Use this for files that don't change much, like servers binaries,
;; external dependencies or long-term shared data.")

(defconst oo-cache-dir (concat oo-emacs-dir ".cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(provide 'oo-base-variables)
;;; oo-base-variables.el ends here
