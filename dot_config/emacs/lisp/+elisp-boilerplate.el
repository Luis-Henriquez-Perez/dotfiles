;;; oo-elisp-boilerplate.el --- elisp file headers and footers -*- lexical-binding: t; -*-
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
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html
;; This file is to help me automate adding package boilerplate.  It is not
;; enough to have it as snippets because I don't even want to go into every file
;; and setup the package header.  Instead, I want to just programmatically
;; invoke a command that adds them all for me--or fixes/updates it if need
;; me.
;;
;;; Code:
(defun oo-copyright-license ()
  (string-join (list ";;"
                     ";; Copyright (c) 2024 Free Software Foundation, Inc."
                     ";;"
                     ";; Author: Luis Henriquez-Perez <luis@luishp.xyz>"
                     ;; If the author is the same person as the maintainer, I do not need to specify them.
                     ;; ";; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>"
                     ;; In the manual its recommended not to write the version on every file, just the main one.
                     ;; ";; Version: 0.1"
                     ;; According to the linter, =package-requires= should only be on the main elisp file.
                     ;; ";; Package-Requires: ((emacs \"29.1\"))"
                     ";; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/"
                     ";;"
                     ";; This file is not part of GNU Emacs."
                     ";;"
                     ";; This program is free software; you can redistribute it and/or"
                     ";; modify it under the terms of the GNU General Public License as"
                     ";; published by the Free Software Foundation, either version 3 of the"
                     ";; License, or (at your option) any later version."
                     ";;"
                     ";; This program is distributed in the hope that it will be useful, but"
                     ";; WITHOUT ANY WARRANTY; without even the implied warranty of"
                     ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU"
                     ";; General Public License for more details."
                     ";;"
                     ";; You should have received a copy of the GNU General Public License"
                     ";; along with this program. If not, see <http://www.gnu.org/licenses/>."
                     ";;"
                     "")
               "\n"))

(defun oo--ensure-provide (file)
  "Ensure FILE ends with proper provide footer."
  (let* ((feature (file-name-sans-extension (file-name-nondirectory file)))
         (provide-name feature)
         (top-rx (rx ";;; provide\n"))
         (provide-rx (rx bol "(provide '" (group (1+ (not (any "\n")))) ")" eol))
         (footer-commentary (format ";;; %s.el ends here\n" feature))
         (footer-rx "\\(\n*\\)\\(?:^\\);;; \\([^[:blank:]]+\\)\\.el ends here\\(\n*\\)\\'"))
    (with-current-buffer (find-file-noselect file)
      ;; Look for provide form.
      (cond ((re-search-forward provide-rx nil t)
             (setq provide-name (match-string 1))
             ;; If it's there, make sure that the feature is correct.  If it is not
             ;; correct, fix it.  Then change.
             (unless (equal provide-name feature)
               (replace-match feature nil nil nil 1))
             ;; Go to the beginning and check if there's a provide header.  If
             ;; not, add it.
             (goto-char (match-beginning 0))
             (unless (save-match-data (looking-back top-rx))
               (insert ";;; provide\n"))
             ;; Now check the end of it, and add the eof-comment afterwards if
             ;; needed.
             (goto-char (line-end-position))
             (if (looking-at footer-rx)
                 (unless (equal (match-string 2) feature)
                   (replace-match "\n" nil nil nil 1)
                   (replace-match feature nil nil nil 2)
                   (replace-match "\n" nil nil nil 3))
               (save-excursion (insert "\n")
                               (insert footer-commentary))))
            (t
             ;; Remove an eof comment if there is one.  And remove a header at
             ;; the end if there is one.  We will re-add it now.
             (goto-char (point-max))
             (insert (format ";;; provide\n(provide '%s)\n%s" feature footer-commentary)))))))
;;;###autoload
(defun oo-ensure-provide ()
  (interactive)
  (oo--ensure-provide (buffer-file-name)))

(defun oo-header-regexp ()
  "Return the regular expression for an emacs package header."
  (rx-to-string '(: bos ";;;" (one-or-more space)
                    (group (one-or-more (not space)))
                    ".el" (one-or-more space)
                    "---"
                    (one-or-more space)
                    (1+ nonl)
                    "-*- lexical-binding: t; -*-\n")))

(defun oo--ensure-file-header ()
  "Ensure that file has a title, description."
  ;; Make sure that the file has a title.
  (let* ((file (buffer-file-name))
         (filename (file-name-sans-extension (file-name-nondirectory file)))
         (header-rx (oo-header-regexp))
         (lisence-rx (rx-to-string (oo-copyright-license)))
         (code-rx nil)
         (commentary "TODO: Add brief description."))
    ;; Use lice to insert the copyright.
    ;; But comment the uncommented blank lines in between.
    ;; And add the appropriate information between the copyright.
    ;; Ensure header.
    (save-excursion
      (goto-char (point-min))
      (if (looking-at header-rx)
          ;; (replace-regexp-in-string "[[:blank:]]+" "\s" string)
          (progn (replace-match filename nil 'literal nil 1)
                 (goto-char (match-end 0)))
        (insert (format ";;; %s.el --- TODO: add commentary -*- lexical-binding: t; -*-\n" filename)))
      ;; Ensure lisence.
      (unless (looking-at lisence-rx)
        (message "NO LICENSE")
        (insert (oo-copyright-license)))
      ;; Ensure commentary.
      (if (looking-at ";;; Commentary:\n\\(?:\\(?:^;;$\\)\n\\|\\(?:^;;[^;].*$\\)\n\\)*")
          (goto-char (match-end 0))
        (insert ";;; Commentary:\n;;\n;; TODO: add commentary\n;;\n"))
      (if (looking-at "\\`;;;[[:blank:]]Code:\n")
          (goto-char (match-end 0))
        (insert ";;; Code:\n")))))

;;;###autoload
(defun oo-ensure-file-header ()
  (interactive)
  (oo--ensure-file-header))

;; TODO: create a test file for a file

;;;###autoload
(defun oo-ensure-boilerplate ()
  (interactive)
  (oo-ensure-file-header)
  (oo-ensure-provide))
;;; TODO: when switching to scratch buffer, set it to emacs-lisp-mode
;;; provide
(provide '+elisp-boilerplate)
;;; +elisp-boilerplate.el ends here
