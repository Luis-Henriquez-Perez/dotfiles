(require 'edit-indirect)
(require 'org)

;; This is an extension that uses =edit-indirect= to edit source blocks and
;; paragraphs.

;; The package =edit-indirect= uses the value [[file:_helpful_function__edit-indirect-guess-mode-function_.png][edit-indirect-guess-mode-function]] to
;; decide what mode the edit-buffer will be in.  The default value of
;; =edit-indirect-guess-mode-function= is [[][]] which just sets the buffer to
;; the result of calling =normal-mode=.  According to its [[][documentation]],
;; the function =normal-mode= is what I used to guess what the appropriate mode
;; should be of a file opened with [[][find-file]]. and since the buffer is not visiting a
;; file and has no blank and blank, this ends up just being [[][fundamental-mode]].
;; Instead of this, or rather in addition to this, I would like the guessing the
;; mode to factor in information from the parent buffer as well.  Specifically, if
;; the parent buffer is an org buffer and a source block is being edited, I want
;; this function to infer that the mode is the one specified by the source block.

(set! edit-indirect-guess-mode-function #'oo-edit-indirect-run-guess-mode-hook)

(defvar oo-edit-indirect-guess-mode-hook nil
  "Hooks run until a hook returns a function.
The function returned is called with no arguments and should set the mode of the
edit-indirect buffer.")

(defun! oo-edit-indirect-run-guess-mode-hook (parent-buffer beg end)
  (with-current-buffer parent-buffer
    (let! mode-fn (run-hook-with-args-until-success 'oo-edit-indirect-guess-mode-hook beg end)))
  (funcall (or mode-fn #'fundamental-mode)))

;; The while loop here leads into infinite recursion. I need to fix this function.
(defun! oo-get-paragraph-bounds ()
  "Return the beginning and end points of the paragraphs between the
property drawer and the source block in the current heading."
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (oo-narrow-to-heading)
      (let! beg (progn (org-end-of-meta-data t) (point)))
      (let! elt (org-element-at-point))
      (while (and elt (equal 'paragraph (org-element-type elt)))
        (goto-char (org-element-property :end elt))
        (let! elt (org-element-at-point)))
      (let! end (point))
      (list beg end))))

(defun! oo-dwim-edit-paragraph ()
  "Edit consecutive paragraphs after a headline."
  (interactive)
  (let! edit-indirect-guess-mode-function (lambda (&rest _) (org-mode)
                                            (when (bound-and-true-p evil-mode)
                                              (evil-insert-state 1))))
  (let! (beg end) (oo-get-paragraph-bounds))
  (let! edit-indirect-after-creation-hook edit-indirect-after-creation-hook)
  (alet (lambda () (add-hook 'edit-indirect-before-commit-hook #'edit-indirect-before-commit-hook&insert-newline-maybe 100 t))
    (add-hook 'edit-indirect-after-creation-hook it))
  (edit-indirect-region beg end t))

(provide 'oo-ext-org-edit-paragraph)
