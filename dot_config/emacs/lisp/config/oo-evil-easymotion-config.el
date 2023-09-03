(defmacro! defevilem! (&rest args)
  "Convenience macro for defining an `evil-easymotion' motion."
  (declare (indent defun))
  (let! (name arglist metadata keys body) (oo-destructure-defun-plus args))
  (let! lambda `(lambda ,arglist (interactive) ,@metadata (block! nil ,@body)))
  `(progn (oo-autoload-fn ',name 'evil-easymotion)
	  (after! evil-easymotion (evilem-make-motion ,name ,lambda ,@keys))))

(defevilem! oo/goto-beginning-of-word ()
  "Jump to beginning of word at current buffer."
  (:scope 'page)
  (:initial-point #'point-min)
  (let! regexp (rx (or (seq bol (1+ white))
		       (seq (1+ white))
		       (seq bol (1+ punct))
		       (seq (1+ white) (1+ punct))
		       (seq (1+ punct)))
		   word))
  (save-match-data
    (when (re-search-forward regexp nil t nil)
      (backward-char))))

(defevilem! oo/goto-end-of-word ()
  "Jump to the end of word in the current buffer."
  (:scope 'page)
  (:initial-point #'point-min)
  (let! regexp (rx (1+ alnum)))
  (save-match-data
    (awhen (save-excursion (goto-char (1+ (point)))
                           (re-search-forward regexp nil t nil))
      (goto-char (1- (match-end 0))))))

(defevilem! oo/goto-char ()
  "Jump to the character in current buffer."
  (:initial-point #'point-min)
  (:scope 'page)
  (:bind ((char (read-char "Char: "))))
  (with! (save-match-data))
  (when (save-excursion (goto-char (1+ (point)))
			            (re-search-forward (rx-to-string (char-to-string char)) nil t nil))
    (goto-char (1- (match-end 0)))))

(set! evilem-keys (number-sequence ?a ?z))
(set! evilem-style 'at)

(bind! (:nv "w" #'oo/goto-beginning-of-word)
       (:nv "e" #'oo/goto-end-of-word)
       (:nv "f" #'oo/goto-char))

(provide 'oo-evil-easymotion-config)
