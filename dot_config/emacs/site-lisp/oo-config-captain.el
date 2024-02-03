(defun! oo-point-in-string-or-comment ()
  "Return 'string if point is in a string, 'comment if in a comment, nil otherwise."
  (interactive)
  (let! ppss (syntax-ppss))
  (cond
   ((nth 3 ppss) 'string)   ; Inside a string
   ((nth 4 ppss) 'comment)  ; Inside a comment
   (t nil)))                ; Neither in a string nor a comment

;; I am going to be honest, I am not sure if this sentence start function
;; captures all cases.  But at least I have not been able to break it yet.  I
;; wish I had known about [[][backward-sentence]] before, it made writing this a
;; lot easier.  Thankfully, =backward-sentence= works in comments and strings.

;; In any case I am so thankful that I do not have to manually capitalize
;; sentences anymore.
(defun! oo-prog-mode-sentence-start-function ()
  "Return point at the start of the last sentence.
Meant to be used as the value of `captain-predicate'."
  ;; (let! rx (seq "."))
  (pcase (oo-point-in-string-or-comment)
    ('comment
     (awhen (car (bounds-of-thing-at-point 'sentence))
       (pushing! points it))
     (save-excursion (backward-sentence 1)
                     (when (bolp) (re-search-forward comment-start-skip nil t))
                     (pushing! points (point))))
    ('string
     (awhen (car (bounds-of-thing-at-point 'sentence))
       (pushing! points it))
     (save-excursion (backward-sentence 1)
                     (pushing! points (point)))))
  (apply #'max points))
