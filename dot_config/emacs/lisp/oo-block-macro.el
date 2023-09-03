;; -*- lexical-binding: t -*-

(require 'treepy)
(require 'dash)

(defun +treepy-skip (zipper)
  "Skip the current node."
  (let ((orig zipper))
    (while (and (not (treepy-right zipper)) (treepy-up zipper))
      (setq zipper (treepy-up zipper)))
    (if (treepy-right zipper)
	    (setq zipper (treepy-right zipper))
      ;; If we've reached the top level, that means there is no next node.  So
      ;; let's go back to where we were and go next until we reach the end.
      (setq zipper orig)
      (while (not (treepy-end-p zipper))
	    (setq zipper (treepy-next zipper)))
      zipper)))

(defalias 'let! '-setq)

(defmacro with! (wrapper &rest wrappers)
  "Enclose BODY with wrappers.
WRAPPER is the same as in `oo-wrap-forms'.
Meant to be used with `block!'.  See `oo-block-parse-with'.")

(defalias 'wrap 'with!)

(defmacro label! (&rest args)
  (declare (indent defun)))

(defalias 'letf! 'label!)

(defmacro excluding! (symbol &rest symbols)
  "Exclude any SYMBOL and SYMBOLS from being let-bound in `block!'.
See `oo-block-parse-excluding'."
  (cl-assert (-all-p #'symbolp (cons symbol symbols))))

(defalias 'without! 'excluding!)

(defalias 'return! 'cl-return)
(defalias 'return-from! 'cl-return-from)

(defmacro continue! ()
  "Skip the current iteration of loop.
This is meant to be used in `block!'.  For what counts as a loop is, see
`oo-block-macro-loop-macros' and `oo-block-parse-loop'."
  `(throw 'continue! nil))

(defalias 'skip! 'continue!)

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
For what counts as a loop is, see `oo-block-macro-loop-macros' and
`oo-block-parse-loop'."
  `(throw 'break! ,value))
(defalias 'break-with! 'break!)
(defalias 'exit! 'break!)
(defalias 'exit-with! 'exit!)

(defun oo-block-let-bindings (let no-let)
  "Return list of let bindings, ignoring no-let.
Ignore any symbols.  See `excluding!'."
  (let (binds)
    (when let
      (for! ((match-form value) let)
	    (if (and (symbolp match-form) (member match-form no-let))
	        (setq match-form '_)
	      (alet (oo-tree-map-nodes (-partial #'-contains-p no-let) (-const '_) match-form)
	        (setq match-form it)))
	    (pushing! binds (list match-form value))))
    (nreverse binds)))

(defun oo-block-parse-body (body)
  (let ((zipper (treepy-list-zip body))
	    (data nil))
    (while (not (treepy-end-p zipper))
      (pcase (treepy-node zipper)
	    (`(,(pred (-contains-p '(quote function \`))) . ,(guard t))
	     (setq zipper (+treepy-skip zipper)))
	    (`(,(and loop (or 'for! 'while 'dolist! 'dolist)) ,pred . ,body)
	     (alet `(catch 'break! (,loop ,pred (catch 'continue ,@body)))
	       (setq zipper (treepy-replace zipper it)))
	     ;; To avoid infinite recursion I need to get past the loop name.  Here I
	     ;; end up at the loop predicate.
	     (loop! (repeat 7)
	       (setq zipper (treepy-next zipper))))
	    (`(,(and name (or 'flet! 'labels! 'noflet! 'macrolet! 'letf!)) ,fn ,args . ,body)
	     (alet (cl-case name
		         (flet! 'cl-flet)
		         (label! 'cl-labels)
		         (noflet! 'noflet)
		         (macrolet! 'cl-macrolet)
		         (letf! 'cl-letf))
	       (setq zipper (treepy-replace zipper `(,it ((,fn ,args ,@body)) ,@(treepy-rights zipper))))
	       (while (treepy-right zipper)
	         (setq zipper (treepy-remove (treepy-right zipper))))))
	    (`(,(or 'without! 'excluding!) . ,(and symbols (guard (-all-p #'symbolp symbols))))
	     (appending! (plist-get data :no-let) symbols)
	     (setq zipper (treepy-remove zipper)))
	    (`(,(or 'wrap! 'with!) . ,(and wrappers (guard (-all-p #'listp wrappers))))
	     (setq zipper (treepy-replace zipper (oo-wrap-forms wrappers (treepy-rights zipper))))
	     (while (treepy-right zipper)
	       (setq zipper (treepy-remove (treepy-right zipper)))))
	    (`(,(and name (pred symbolp) (guard (string-match-p "ing!\\'" (symbol-name name)))) ,symbol . ,(guard t))
	     (alet (cl-case name
		         ((maxing! maximizing!) most-negative-fixnum)
		         ((minning! minimizing!) most-positive-fixnum)
		         (counting! 0))
	       (adjoining! (plist-get data :let) (list symbol it) (-on #'equal #'car)))
	     (setq zipper (treepy-next zipper)))
	    (`(,(or 'let! 'let->>! 'let-->!) ,match-form ,_ . ,(and plist (guard t)))
	     (alet (or (car (member match-form '(gc-cons-threshold gc-cons-percentage)))
		           (plist-get plist :init))
	       (adjoining! (plist-get data :let) (list match-form it) (-on #'equal #'car)))
	     (setq zipper (treepy-next zipper)))
	    ;; (`(alet! ,expr)
	    ;;  (pushing! (plist-get data :let) (list 'it expr))
	    ;;  (setq zipper (treepy-next zipper)))
	    (_
	     (setq zipper (treepy-next zipper)))))
    (list data zipper)))

(defmacro block! (name &rest body)
  "Define a lexically-scoped block named NAME.
Name may be any symbol.  Code inside body can call `return!'."
  (declare (indent 1))
  (-let* (((data zipper) (oo-block-parse-body body))
	      ((&plist :let :no-let) data)
	      (body (treepy-root zipper))
	      (let-bindings (oo-block-let-bindings let no-let)))
    `(cl-block nil (-let ,(oo-block-let-bindings let no-let) ,@body))))

(provide 'oo-block-macro)
