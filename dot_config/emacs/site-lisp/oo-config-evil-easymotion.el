(evilem-make-motion oo-goto-beginning-of-word
                    (lambda nil
                      (interactive)
                      "Jump to beginning of word at current buffer." nil
                      (block! nil
                        (let! regexp
                          (rx
                           (or
                            (seq bol
                                 (1+ white))
                            (seq
                             (1+ white))
                            (seq bol
                                 (1+ punct))
                            (seq
                             (1+ white)
                             (1+ punct))
                            (seq
                             (1+ punct)))
                           word))
                        (save-match-data
                          (when
                              (re-search-forward regexp nil t nil)
                            (backward-char)))))
                    :initial-point
                    (function point-min)
                    :scope 'page)

(evilem-make-motion oo-goto-end-of-word
                    (lambda nil
                      (interactive)
                      "Jump to the end of word in the current buffer." nil
                      (block! nil
                        (let! regexp
                          (rx
                           (1+ alnum)))
                        (save-match-data
                          (awhen
                              (save-excursion
                                (goto-char
                                 (1+
                                  (point)))
                                (re-search-forward regexp nil t nil))
                            (goto-char
                             (1-
                              (match-end 0)))))))
                    :initial-point
                    (function point-min)
                    :scope 'page)

(evilem-make-motion oo-goto-char
                    (lambda nil
                      (interactive)
                      "Jump to the character in current buffer." nil
                      (block! nil
                        (with!
                         (save-match-data))
                        (when
                            (save-excursion
                              (goto-char
                               (1+
                                (point)))
                              (re-search-forward
                               (rx-to-string
                                (char-to-string char))
                               nil t nil))
                          (goto-char
                           (1-
                            (match-end 0))))))
                    :bind
                    ((char
                      (read-char "Char: ")))
                    :scope 'page :initial-point
                    (function point-min))
