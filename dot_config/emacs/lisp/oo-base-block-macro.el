(require 'cl-lib)
(require 'dash)
(require 'noflet)
;; This library is for [[][walking lisp forms]].  Basically, it provides an iterator
;; that I can use to navigate a form.  The iterator allows me to move freely and
;; edit nodes precisely--allowing me to do things that would be very difficult with
;; [[][dash's]] [[][-tree-map-nodes]].
(require 'treepy)
(require 'pcase)
(require 'oo-base-ing-macros)

;; Load the treepy patch that I made.  Making it a feature I have to load with
;; =load= instead of feature that would be loaded with =require= maybe unconventional.  Maybe I
;; should actually make the patch into a feature.  I am not sure.  I wouldn't
;; call the configuration files for packages =features= per say, they are more
;; of just customization.  There is also the practical deterrent that making
;; them features would mean I would have to add at the very least =provide=
;; calls at the end of each of the files.
(load (locate-user-emacs-file "lisp/oo-config-treepy.el") nil :no-message)

;;; block! - an advanced macro
;; The =block!= macro goals:
;; 1 - reduce excessive nesting nesting by making intelligent assumptions about the intended scope of variables.
;; This includes:
;;  - automatically let binding variables
;;  - automatically let binding in designated macros--[[][the "ing!" macros]]
;;  - reduce the cluttered syntax that discourages the use of powerful macros such
;;    as [[][cl-letf]] and [[][cl-labels]]
;;  - providing a concise declarative way to wrap the macro body with common constructs
;; 2 - allow loops to use control flow contructs popular in other languages
;; These include:
;;  - return!
;;  - continue! (skip!) skip the current iteration of a loop
;;  - break! (exit!) exit the current loop
;;;; account for improper lists when mapping nodes
;; The dash function [[][-tree-map-nodes]] will fail when walking the body of a macro that has
;; improper lists.  While I wouldn't say improper lists are common in lisp code,
;; they do happen--particularly when using [[][dash]] and [[][loopy]] destructuring.  A
;; specific example of this is in the block macro.
(defun oo-tree-map-nodes (pred fun tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fun tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fun (car tree))
               (oo-tree-map-nodes pred fun (cdr tree))))
        (t
         tree)))
;;;; define placeholders
;; There are a number of what I call placeholder macros for block.  They are macros
;; that do pretty much nothing in an of themselves; they might at most verify the
;; arguments they recieve are correct.  They serve as markers used by the =block!=
;; macro to get information about what to do.  Additionally, defining these
;; placeholder macros tells syntax highlighting and indentation.  Trying to bind
;; symbols to functions with is usually quite cumbersome if the function you want
;; to write is more than one line long.  This is nice so
;; that =flet!=, =noflet!= and =label!= with the indentation of a normal function in
;; contrast with their counterparts =cl-flet=, =noflet=, and =cl-labels=.
(defalias 'let! '-setq)

;; Meant to be used with =oo-block-macro-handle-with=.  These macros are for just
;; making sure that the keywords =wrap!= and =with!= are properly syntax
;; highlighted and have the correct indentation.
(defmacro with! (wrapper &rest wrappers)
  "Enclose BODY with wrappers.
WRAPPER is the same as in `oo-wrap-forms'.
Meant to be used with `block!'.  See `oo-block-parse-with'.")

(defalias 'wrap 'with!)

(defmacro label! (&rest args)
  (declare (indent defun)))

(defalias 'letf! 'label!)
(defalias 'flet! 'label!)
(defalias 'noflet! 'label!)
(defalias 'stub! 'label!)

;; Sometimes we encounter symbols we don't want =block!= let binding.
(defmacro excluding! (symbol &rest symbols)
  "Exclude any SYMBOL and SYMBOLS from being let-bound in `block!'.
See `oo-block-parse-excluding'."
  ;; (cl-assert (-all-p #'symbolp (cons symbol symbols)))
  )

(defalias 'without! 'excluding!)

;; These are macros that capitalize on the catch blocks I generate with [[id:20230807T063155.724861][block!]] to
;; provide control flow structures like those found in other lanaguges.
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

;; At first I did this by creating functions.  Although this is nice for splitting
;; up code.
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
        (`(,(and name (or 'stub! 'flet! 'label! 'noflet! 'macrolet! 'letf!)) ,fn ,args . ,body)
         (alet (cl-case name
                 (stub! 'noflet)
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
           (adjoining! (plist-get data :let) (list symbol it) :test #'equal :key #'car))
         (setq zipper (treepy-next zipper)))
        (`(,(or 'let! 'let->>! 'let-->!) ,match-form ,_ . ,(and plist (guard t)))
         (alet (or (car (member match-form '(gc-cons-threshold gc-cons-percentage)))
                   (plist-get plist :init))
           (adjoining! (plist-get data :let) (list match-form it) :test #'equal :key #'car))
         (setq zipper (treepy-next zipper)))
        (`(alet! ,expr)
         (setq zipper (treepy-replace zipper `(alet ,expr ,@(treepy-rights zipper))))
         (while (treepy-right zipper)
           (setq zipper (treepy-remove (treepy-right zipper)))))
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

(provide 'oo-base-block-macro)
