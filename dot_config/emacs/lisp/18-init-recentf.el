;; Copyright (C) 2024

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(oo-add-hook 'emacs-startup-hook #'recentf-mode)

(opt! recentf-max-menu-items 0)

(opt! recentf-max-saved-items 700)

(opt! recentf-save-file (concat oo-cache-dir "recentf"))

(opt! recentf-auto-cleanup (* 60 10))

(opt! recentf-filename-handlers '(file-truename))

(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)

(oo-add-advice #'recentf-mode :around #'oo-funcall-silently)
(oo-add-advice #'recentf-cleanup :around #'oo-funcall-silently)
(oo-add-advice #'recentf-save-list :around #'oo-funcall-silently)

;; TODO: Add back =adjoin!= if I removed it.
(cl-pushnew recentf-filename-handlers #'abbreviate-file-name)

(cl-pushnew recentf-filename-handlers #'substring-no-properties)

(cl-pushnew recentf-exclude (recentf-expand-file-name no-littering-var-directory))

(cl-pushnew recentf-exclude (recentf-expand-file-name no-littering-etc-directory))

(provide '80-init-recentf)
