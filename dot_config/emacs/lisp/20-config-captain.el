;; Man, this is more involved than I thought it would be at first because
;; capitalization rules are different depending on whether your in prog-mode or
;; not and whether you are in a doc-string, or comment, or just a normal string.

;;;; determine where I am
(defun! oo--point-in-string-or-comment-p ()
  "Return 'string if point is in a string, 'comment if in a comment, nil otherwise."
  (interactive)
  (set! ppss (syntax-ppss))
  (cond ((nth 3 ppss) 'string)   ; Inside a string
        ((nth 4 ppss) 'comment)  ; Inside a comment
        (t nil)))

;; TODO: generalize regexp with `defun!', `cl-defun', etc.
(defvar oo-docstring-regexp "(defun[[:blank:]]\\([^[:space:]]+\\)[[:blank:]](\\(.*\\))\n[[:blank:]]*\"")

;; TODO: generalize this regexp for comments in different languages.
(defun oo--beg-comment-block-rx ()
  "Return a regular expression that matches the beginning of a comment block."
  (rx-to-string
   `(: (or bos
           ;; comments with more
           (: bol (>= 3 ,comment-start) (0+ any) eol "\n")
           ;; blank lines
           (: bol eol "\n")
           (: bol (not ,comment-start) (* any) eol "\n"))
       (: bol (zero-or-more blank) (= 2 ,comment-start) blank))))

(defun oo--prog-mode-should-capitalize-p ()
  "Return point if."
  (pcase (oo--point-in-string-or-comment-p)
    ('comment
      (or (save-match-data
            ;; Limit the look back to the start of the previous line.
            (and (looking-back (oo--beg-comment-block-rx) (line-beginning-position 0))
                 (match-end 0)))
          (captain--default-sentence-start)))
    ('string
      ;; If it's a docstring capitalize the first word of the doc-string.
      (when (looking-back oo-docstring-regexp (line-beginning-position 0))
        (match-end 0)))))

(defhook! text-mode-hook&set-captain-local-vars ()
  (setq-local captain-predicate #'always)
  (setq-local captain-sentence-start-function #'captain--default-sentence-start))

;; TODO: figure out the best way to add these things.
(defhook! prog-mode-hook&set-captain-local-vars ()
  (setq-local captain-predicate #'oo--point-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'oo--prog-mode-should-capitalize-p))

(provide '20-config-captain)
