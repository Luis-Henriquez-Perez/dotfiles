;; The purpose of this funciton is to

(defun! oo-convert-comment-bullets ()
  (interactive)
  (let! regexp (seq bol (group (one-or-more ";")) (one-or-more blank) (group (one-or-more "*"))))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let! num-bullets (length (match-string 2)))
      (let! num-start (length (match-string 1)))
      (let! text (s-repeat (+ num-bullets num-start) comment-start))
      (replace-match text))))

(provide oo-outshine-bullets)
