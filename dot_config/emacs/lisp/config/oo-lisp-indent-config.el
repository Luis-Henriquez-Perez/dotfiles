;; A problem with elisp indentation is indents quoted lists the way functions
;; should be indented.  It has been discussed in at least three stackoverflow
;; questions [[https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned/10233#10233][here]], [[https://stackoverflow.com/questions/49222433/align-symbols-in-plist][here]] and [[https://stackoverflow.com/questions/22166895/customize-elisp-plist-indentation][here]].  In all these questions the solutions have not
;; been satisfactory.  Some of them recommend using [[helpfn:common-lisp-indent-function][common-lisp-indent-function]] as
;; the value of [[helpvar:lisp-indent-function][lisp-indent-function]].  This works for indenting a quoted list
;; properly, but at the expense of changing the way that many other elisp forms are
;; indented.  Common Lisp's indentation is different from Elisp's.  Others recommend
;; using [[https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94][Fuco1's lisp indent function hack]].  This also is not ideal.  For one thing it
;; only works for quoted lists with keywords but not generic symbols.  Another thing
;; is that the change should really be occurring in [[helpfn:calculate-lisp-indent][calculate-lisp-indent]].
;; ~calculate-lisp-indent~ is a function that returns what the indentation should be
;; for the line at point.  Since Fuco1 did not modify ~calculate-lisp-indent~ the
;; *wrong* indentation still returned by this function and the modified
;; ~lisp-indent-function~ just cleans up the mess.  Better is just fixing the source
;; of the problem.  You can check out a more in-depth explanation looking at my
;; [[https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/][reddit-post]] or looking at an answer I gave to [[https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned][this question]].
(defadvice! properly-calculate-indent (override calculate-lisp-indent)
  "Add better indentation for quoted and backquoted lists.  "
  (:args &optional parse-start)
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      (= (point) calculate-lisp-indent-last-sexp)

                      (when-let (after (char-after (1+ containing-sexp)))
                        (char-equal after ?:))

                      (when-let (point (char-before containing-sexp))
                        (char-equal point ?'))

                      (let ((quoted-p nil)
                            (point nil)
                            (positions (nreverse (butlast (elt state 9)))))
                        (while (and positions (not quoted-p))
                          (setq point (pop positions))
                          (setq quoted-p
                                (or
                                 (and (char-before point)
                                      (char-equal (char-before point) ?'))
                                 (save-excursion
                                   (goto-char (1+ point))
                                   (looking-at-p "quote[\t\n\f\s]+(")))))
                        quoted-p))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.   Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.   Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.   (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))
