;;; 99-uncategorized.el --- TODO: Needs commentary.  -*- lexical-binding: t; -*-
;; Copyright (C) 2024  -*- lexical-binding: t; -*-

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

(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(oo-add-hook 'prog-mode-hook #'hs-minor-mode)

(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

;; (oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)

(oo-add-hook 'on-first-input-hook #'minibuffer-depth-indicate-mode)

(oo-add-hook 'prog-mode-hook 'auto-fill-mode)

(oo-add-hook 'text-mode-hook #'visual-line-mode)

(oo-add-hook 'text-mode-hook #'auto-fill-mode)

(defun! oo-copyright-license (file title commentary)
  (format str file title commentar))

(defun oo-ensure-provide (file)
  "Ensure FILE ends with proper provide stuff."
  (let* ((feature (file-name-sans-extension (file-name-nondirectory file)))
         (provide-name feature)
         (top-rx (rx ";;; provide\n"))
         (provide-rx (rx bol "(provide '" (group (1+ (not (any "\n")))) ")" eol))
         (header-commentary (format ";;; %s.el ends here\n" feature))
         (header-rx (rx-to-string `(seq ,header-commentar))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
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
                 (insert top-header))
               ;; Now check the end of it, and add the eof-comment afterwards if
               ;; needed.
               (goto-char (match-end 0))
               (unless (looking-back header-rx nil t)
                 (insert header-commentary)))
              (t
               ;; Remove an eof comment if there is one.  And remove a header at
               ;; the end if there is one.  We will re-add it now.
               (goto-char (point-max))
               (insert (format ";;; provide\n(provide '%s)\n%s" feature header-commentary))))))))

(defun oo/ensure-provide ()
  (interactive)
  (oo-ensure-provide (buffer-file-name)))

(defun oo-ensure-file-header (file)
  "Ensure that file has a title, description."
  ;; Make sure that the file has a title.
  (let ((filename (file-name-sans-extension (file-name-nondirectory file)))
        (header-rx "\\`;;;[[:space:]]\\([^[:space:]]+\\)\\.el[[:space:]]---")
        (commentary "TODO: Add brief description."))
    ;; Use lice to insert the copyright.
    ;; But comment the uncommented blank lines in between.
    ;; And add the appropriate information between the copyright.
    (looking-at "Copyright (c)")
    (replace-match "Copyright (c), Luis Henriquez <luis@luishp.xyz>")
    ;; (lice "gpl-3.0")
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward header-rx nil t nil)
             ;; Make sure that file header is the appropriate name.
             (unless (equal filename (match-string 1))
               (replace-match filename nil 'literal nil 1)))
            ;; Make sure that right after that, the copyright section is there.
            ;; (unless (looking-at license-rx)
            ;;   (insert lisence))
            ((goto-char (point-min))
             (pcase filename
               ((pred (string-match-p "\\`[[:digit:]][[:digit:]]-init"))
                (setq commentary "Initialize `%s'"))
               ((pred (string-match-p "\\`[[:digit:]][[:digit:]]-config"))
                (setq commentary "Configure `%s'")))
             (goto-char (point-min))
             (insert (format ";;; %s.el --- %s\n" filename commentary))
             ;; Add a commentary if the file is an init or config.
             ;; (unless (looking-at license-rx)
             ;;   (insert lisence))
             )))
    ;; Also ensure that the title matches the file.  If not, fix it.
    ;; Then ensure that the lexical binding line is there.
    (add-file-local-variable-prop-line 'lexical-binding t)
    ;; And then ensure that the copyright is under that.
    ))

(defun oo/ensure-file-header ()
  (interactive)
  (oo-ensure-file-header (buffer-file-name)))
;; (defun! oo-add-copyright-boilerplate (file)
;;   "Add copyright boilerplate to file."
;;   ;; (set! feature)
;;   (set! filename ())

;;   (flet! init-file-p ())
;;   (flet! config-file-p ())

;;   (pcase file
;;     ((pred init-file-p)
;;      (set! title "")
;;      (set! commentary "Initialize `%s'." feature))
;;     ((pred config-file-p)
;;      (set! title "")
;;      (set! commentary "Configuration for `%s'." feature))
;;     (_
;;      (set! title "")
;;      (set! commentary "")))

;;   (goto-char (point-min))
;;   (insert license)
;;   ;; -----------------------------
;;   ;; file ends here line
;;   (set! ends-here-rx (rx-to-string (seq ,(format "%s ends here" file))))
;;   (re-search-forward )
;;   ;; If the `provide' line is not present add it or if does not match the
;;   ;; feature that is supposed to be provided, change it to do so.
;;   )
;;;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defadvice! load-theme@ARdisable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(defhook! after-init-hook&load-theme ()
  "Load `modus-operandi' theme."
  (when (display-graphic-p)
    (load-theme 'modus-operandi :no-confirm nil)))
;;; bindings
(oo-bind :nm "+" #'text-scale-increase)
(oo-bind :nm "-" #'text-scale-decrease)
;;; initial buffer choice
(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
              (get-buffer-create "*scratch*"))
    (lgr-info oo-lgr "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)

;;; TODO: when switching to scratch buffer, set it to emacs-lisp-mode
;;; provide
(provide '99-uncategorized)
;;; 99-uncategorized.el ends here
