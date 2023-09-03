(oo-add-hook 'org-insert-heading-hook #'evil-append 1)

(defun edit-indirect-before-commit-hook&insert-newline-maybe (&rest _)
  "Add a newline to edit-indirect buffers if they don't have one."
  (alet (buffer-string)
    (unless (or (string-empty-p it) (string-match-p (rx (1+ anything) "\n" eos) it))
      (insert (prog1 (concat (buffer-string) "\n") (erase-buffer))))))

(defun +org-narrow-to-heading ()
  "Narrow region to the heading at point."
  (save-excursion
    (org-back-to-heading t)
    (let! narrow-beg (point))
    (let! narrow-end (save-excursion
                       (or (outline-next-heading) (end-of-buffer))
                       (point))))
  (narrow-to-region narrow-beg narrow-end))

(defun! +org-get-paragraph-bounds ()
  "Return the beginning and end points of the paragraphs between the
property drawer and the source block in the current heading."
  (interactive)
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (+org-narrow-to-heading)
      (let! beg (progn (org-end-of-meta-data t) (point)))
      (let! elt (org-element-at-point))
      (while (and elt (equal 'paragraph (org-element-type elt)))
        (goto-char (org-element-property :end elt))
        (let! elt (org-element-at-point)))
      (let! end (point))
      (list beg end))))

(defun! +org/dwim-edit-paragraph ()
  "Edit consecutive paragraphs after a headline."
  (interactive)
  (let! edit-indirect-guess-mode-function (lambda (&rest _) (text-mode)))
  (let! (beg end) (+org-get-paragraph-bounds))
  (let! edit-indirect-after-creation-hook edit-indirect-after-creation-hook)
  (alet (lambda () (add-hook 'edit-indirect-before-commit-hook #'edit-indirect-before-commit-hook&insert-newline-maybe 100 t))
    (add-hook 'edit-indirect-after-creation-hook it))
  (edit-indirect-region beg end t))

(oo-popup-at-bottom "\\*edit-indirect[^z-a]+")
