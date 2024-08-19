;;; oo-commands.el --- Generic commands -*- lexical-binding: t; -*-
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
;; This file contains non-package specific commands that I use generally.  Some
;; of these commands will not be perfect in that they are specific to me instead
;; of generalized polished commands you might see in packages.  Instead,
;; these functions are very specific to me and my workflow.
;;
;;; Code:
(require 'base)
;;;; opening specific files
;; A complicating factor is the fact that I use the chezmoi directory as the
;; main way to edit these files.
(defun! oo--chezmoi-source-path (target-dir)
  "Get the source path for a given TARGET-DIR using chezmoi."
  (cl-assert (executable-find "chezmoi"))
  (set! command (format "chezmoi source-path %s" (shell-quote-argument target-dir)))
  (set! source-path (string-trim (shell-command-to-string command)))
  ;; Remove any trailing newlines from the output
  (and (not (string-empty-p source-path))
       source-path))

(defun oo-open-emacs-config ()
  "Open Emacs configuration."
  (interactive)
  (switch-to-buffer (dired (oo--chezmoi-source-path user-emacs-directory))))

(defun oo-open-emacs-init-file ()
  "Open init file."
  (interactive)
  (switch-to-buffer (find-file-noselect (oo--chezmoi-source-path user-init-file))))

(defun oo-open-emacs-lisp-dir ()
  "Open lisp directory."
  (interactive)
  (switch-to-buffer (find-file-noselect (oo--chezmoi-source-path oo-lisp-dir))))
;;;; window splitting
(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;;; font
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (set! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))
;;;; sorting
;; This is meant to sort the great number of install package forms I have in
;; `init-elpaca'.
(defun! oo-sort-elpaca-forms (beg end)
  "Sort elpaca forms lexicographically by package name."
  (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

(defun! oo-sort-autoload-forms (beg end)
  "Sort autoload forms lexicographically by package name."
  (set! rx "(autoload[[:blank:]]+#'[^[:space:]]+[[:blank:]]+\"\\(.+?\\)\".+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

;; This is meant to sort the great number of `require' forms in the init file.
(defun! oo-sort-require-forms (beg end)
  "Sort require forms lexicographically by feature name."
  (set! rx (rx (seq "(require" (one-or-more blank) "'" (group (1+ nonl))")")))
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

;; If I have a region selected, use that.
;; If it is interactive and I do not have a region selected use the beginning
;; of the current line and the end of the buffer.
(defun! oo-sort-dwim (beg end)
  "Sort lines the way I like it."
  (interactive
   (if (region-active-p)
	   (list (region-beginning) (region-end))
	 (list (line-beginning-position) (point-max))))
  (set! regexp (rx "(" (group (or "autoload" "require" "elpaca"))))
  (save-excursion
    (goto-char beg)
    (re-search-forward regexp end t nil))
  (pcase (match-string 1)
	("autoload" (oo-sort-autoload-forms beg end))
	("require" (oo-sort-require-forms beg end))
	("elpaca" (oo-sort-elpaca-forms beg end))
	(_ (error "No sorting method detected"))))

;;;; miscellaneous
(defun oo-dwim-narrow (keep-narrowing-p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: narrow to region, outline heading, org-src-block, org-subtree, or
defun, whichever applies first.

With prefix KEEP-NARROWING-P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not keep-narrowing-p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((equal 'comment (oo-in-string-or-comment-p))
         (save-excursion (outli-toggle-narrow-to-subtree)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-narrow-to-block) t)
             (org-narrow-to-subtree)))
        (t
         (narrow-to-defun))))
;; You could actually do this via abbrev-mode as well.  And actually it might be
;; better in a sense because.
(defun! oo-dwim-space ()
  "Replace two consecutive spaces with a period."
  (interactive)
  (set! rx "\\([[:word:]]\\)\\([[:space:]][[:space:]]\\)\\([^[:space:]]+\\)")
  (cond ((and (or (derived-mode-p 'text-mode)
                  (oo-in-string-or-comment-p))
              (looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" nil))
         (replace-match "\\1.\s\s"))
        (t
         (insert "\s"))))

(defun! oo-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))

(defun oo-kill-emacs-no-confirm ()
  "Kill Emacs without confirmation."
  (let (confirm-kill-emacs)
    (call-interactively #'kill-emacs)))

;; Keep track of the themes that I have loaded and do not allow repetitions.
(defvar oo-loaded-themes nil
  "Themes that have already been loaded.")

(defun! oo-load-random-theme ()
  "Load a random theme."
  (interactive)
  (set! not-loaded (-difference (custom-available-themes) oo-loaded-themes))
  (set! theme (seq-random-elt not-loaded))
  (condition-case err
      (progn (load-theme theme)
             (push theme oo-loaded-themes)
             (message "Loaded theme `%s'..." theme))
    (signal (car err) (cdr err))))

;; This idea is based on the following link where xah lee talks about why the
;; scratch buffer is outdated.  It does not follow the trend of "untitled1",
;; "untitled2" as xah lee recommended because it is just easier and more
;; consistent to use Emacs's buffer naming style.
;; http://xahlee.info/emacs/emacs/modernization_scratch_buffer.html
(defun! oo-new-buffer ()
  "Create a new blank buffer."
  (interactive)
  (display-buffer (generate-new-buffer "untitled")))

;; This has to do with chezmoi.
(defun! update-emacs-config ()
  ;; Clear the existing files that are not part of chezmoi.
  ())
(defun oo-copyright-license ()
  "Return the copyright license."
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
    (save-excursion
      (goto-char (point-min))
      (if (looking-at header-rx)
          (progn (replace-match filename nil 'literal nil 1)
                 (goto-char (match-end 0)))
        (insert (format ";;; %s.el --- TODO: add commentary -*- lexical-binding: t; -*-\n" filename)))
      ;; Ensure license.
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

(defun oo--create-lisp-dir-file (name dir comment1 comment2)
  "Auxiliary function."
  (set! filename (expand-file-name name dir))
  (cl-assert (not (file-exists-p filename)))
  (with-current-buffer (find-file filename)
    (oo--ensure-file-header)
    (goto-char (point-min))
    ;; This is a kind of roundabout way of doing it.  Not sure if it is the
    ;; "best" way whatever that means, but it works.
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment1)
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment2)
    (save-excursion (oo--ensure-provide filename))))

(defun! oo-create-new-init-file (feature)
  "Create a new init file for feature."
  (interactive "sFeature: ")
  (set! lisp-dir (oo--chezmoi-source-path oo-lisp-dir))
  (set! filename (format "init-%s.el" feature))
  (set! comment1 (format "Initialize %s" feature))
  (set! comment2 (format "Initialize %s." feature))
  (oo--create-lisp-dir-file filename lisp-dir comment1 comment2))

(defun! oo-create-new-config-file (feature)
  "Create a new config file for feature."
  (interactive "sFeature: ")
  (set! lisp-dir (oo--chezmoi-source-path oo-lisp-dir))
  (set! filename (format "config-%s.el" feature))
  (set! comment1 (format "Configure %s" feature))
  (set! comment2 (format "Configure %s." feature))
  (oo--create-lisp-dir-file filename lisp-dir comment1 comment2))

(defun! oo-create-new-test-file (file)
  "Create a new config file for feature."
  (interactive "sFile: ")
  (set! dir oo-test-dir)
  (set! filename (format "base-%s-test.el" feature))
  (set! comment1 (format "Test %s" feature))
  (set! comment2 (format "Test %s." feature))
  (oo--create-lisp-dir-file filename test-dir comment1 comment2))

;;;###autoload
(defun oo-ensure-boilerplate ()
  (interactive)
  (oo-ensure-file-header)
  (oo-ensure-provide))
;;; provide
(provide 'oo-commands)
;;; oo-commands.el ends here
