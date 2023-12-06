(require 'oo-base-utils)
(require 'oo-modification-macros)

;;; oo-call-after-load
;; This feature provides the star function `oo-call-after-load' as well as.
;;;; create a hook that checks for any unbound symbols and binds them
;; :PROPERTIES:
;; :ID:       20230801T060330.101692
;; :END:
;; The abnormal hook [[][after-load-functions]] is run after any file is loaded.

;; I'll note that I push all the forms into a list and evaluate them all in the
;; body of one lambda as opposed to evaluating one lambda per form.  This is
;; important because lambda calls have an overhead that adds up.  It is far less
;; costly to invoke one lambda over N lambdas.
(defun! after-load-functions&set-bound-symbols (&rest _)
  "Set symbols that have been bound to the result of their corresponding expr.
Check each symbol in `oo-unbound-symbol-alist', removing those that have already been
bound and setting them to the result of evaluating expr."
  (for! ((elt &as symbol . expr) oo-unbound-symbol-alist)
    (cond ((boundp symbol)
           (pushing! exprs `(set! ,symbol ,expr)))
          (t
           (pushing! updated elt))))
  (setq oo-unbound-symbol-alist (nreverse updated))
  (when exprs (funcall `(lambda () ,@exprs))))

(add-hook 'after-load-functions #'after-load-functions&set-bound-symbols)
;;;; after feature is loaded
;; :PROPERTIES:
;; :ID:       20230824T140744.273989
;; :END:
;;;; oo-try-load-feature
;; :PROPERTIES:
;; :ID:       957942d3-9632-4e04-a6e6-9a4e03b26fba
;; :END:
(defun! oo-try-load-feature (fn)
  "Try to load feature that could contain FN."
  (unless (fboundp fn)
    (dolist (feature (oo-candidate-features fn))
      (require feature)
      (when (fboundp fn)
        (return! feature)))))
;;;; oo--call-after-load
;; :PROPERTIES:
;; :ID:       20230112T144220.969274
;; :END:
(defun oo--call-after-load (condition fn &rest args)
  "Call FN with ARGS after CONDITION is met.
For what CONDITION is see `oo-call-after-load'."
  (pcase condition
    (`(:or . ,conditions)
     (--each conditions (apply #'oo--call-after-load it fn args)))
    (`(:and . ,conditions)
     (apply #'oo--call-after-load conditions fn args))
    ((or (pred null) (and (pred symbolp) (pred featurep)))
     (apply fn args))
    (`(,condition . ,conditions)
     (apply #'oo--call-after-load condition #'oo--call-after-load conditions fn args))
    ((and feature (pred symbolp))
     (eval-after-load feature `(funcall #',fn ,@(mapcar #'macroexp-quote args))))
    (_
     (error "invalid condition `%S'" condition))))
;;;; oo-call-after-load-fn
;; :PROPERTIES:
;; :ID:       20230111T064508.607498
;; :END:
;; An error in a =eval-after-load= form interferes with any subsequent
;; =eval-after-load= forms.  Furthermore, if features depend on the feature you're
;; trying to load then the error interferes with loading them (its similar to
;; having an error in a hook).  This is why I ignore errors here (eventually I don't
;; want to completely ignore them, but for now this is what I do).

;; Admittedly, I didn't encounter any problems with this happening using the
;; built-in =eval-after-load= because most if not all of the forms I have written
;; are not dangerous to re-evaluate.  I know that when I use =eval-after-load= I
;; mean to have forms evaluated only once.  I want to be formal about the matter.

;; When something goes wrong I don't want to wonder what's going on.  I want to know
;; what is happening and when.  To this end, I ensure the function logs to the log
;; buffer.
(defun! oo-call-after-load-fn (fn args)
  "Return function to be used for `oo-call-after-load' based on FN.
The returned function calls FN with args ignoring any resulting errors.
Additionally, it does nothing and returns nil on any calls after its first
call."
  (let! fname (gensym "eval-after-load-fn-"))
  (let! closure (eval `(let ((first-call-p t))
                         (lambda (&rest _)
                           (when first-call-p
                             (setq first-call-p nil)
                             (lgr-info oo-lgr (s-truncate 50 (format "Calling %s with %S" ',fn ',args)))
                             (ignore-errors (apply #',fn ',args)))))
                      t))
  (fset fname closure)
  fname)
;;;; oo-call-after-load
;; :PROPERTIES:
;; :ID:       5f10adc7-11bc-4100-b775-6f1d43655754
;; :END:
;; This has to be separate from =oo--call-after-load= because I need to make sure
;; that =oo-call-after-load-fn= is called only one time.
(defun oo-call-after-load (condition fn &rest args)
  "Call FN with ARGS after CONDITION resolves.
CONDITION can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of CONDITIONS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-after-load'.  If CONDITION is a list of
CONDITIONS, call FN with ARGS only after all CONDITIONS have been met.  If
CONDITION is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If CONDITION is a list whose CAR is `:or', call FN with ARGS after any of
CONDITIONS in (CDR CONDITION) is met."
  (oo--call-after-load condition (oo-call-after-load-fn fn args)))
;;;; after!
;; :PROPERTIES:
;; :ID:       931dc720-6514-4564-acbf-1cfe37436f68
;; :END:
;; This is a convenince macro for =oo-call-after-load=.  The latter is concise when
;; there is an existing function and possibly arguments that I can add.  However,
;; =after!= is more convenient the case when I want a custom body and I don't want
;; to define a function.
(defmacro! after! (condition &rest body)
  "Eval BODY after CONDITION is met.
For what CONDITION is see `oo-call-after-load'."
  (declare (indent 1))
  `(oo-call-after-load ',condition (lambda () ,@body)))
;;;; defafter!
;; :PROPERTIES:
;; :ID:       892a40f9-7023-4cdc-a73c-c03d79037fca
;; :END:
;; I want to encourage named after blocks.  The name helps provide a
;; description of what I'm intending to accomplish with said after
;; block.  Although I don't have to define a named function to evaluate
;; the code of the after block, I do for two reasons.  One is that I more
;; easily ensure that the after block forms are run only once.  The other
;; is that I can rerun or examine a given `defafter!` form when
;; debugging.
(defmacro! defafter! (&rest args)
  "A wrapper around `after!' With same syntax as `defun'."
  (declare (indent defun))
  (let! (_ args _ body) (oo-defun-components args))
  `(after! ,args ,@body))

(provide 'oo-call-after-load)

(provide 'oo-call-after-load)
