;;; init.el --- Custom configuration -*- lexical-binding: t; -*-
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
;; URL: MYURL
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
;; These are my personal Emacs configurations. Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;;; emacs
;; For now I put everything in a single file.  The reason I choose to
;; do this is because it is simply easier for me with the knowledge I
;; have about compiling than having to consider loading order and

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; features with =require=.
(require 'oo-base-library)
(require 'oo-base-settings)
;; (require 'oo-base-custom)
;; (require 'oo-base-package)

;;;; disable garbage collection until I'm done with startup
;; This variable controls how often.  Setting it to =most-positive-fixnum=, a very
;; big number, essentially disables garbage collection.  The garbage collection is
;; later reset to a reasonable value.
;; (startup-set! gc-cons-threshold most-positive-fixnum)

;; This is the percentage of the heap before.
;; (startup-set! gc-cons-percentage 0.8)
;;;; don't search for whenever a package is loaded
;; (startup-set! file-name-handler-alist nil)
;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
;; (startup-set! mode-line-format nil set-default)

;;; init.el ends here
(provide 'init)
