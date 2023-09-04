(require 'oo-base-utils)
(require 'oo-modification-macros)

(defun oo-try-load-feature (fn)
  "Try to load feature that could contain FN."
  (unless (fboundp fn)
    (let ((candidates (oo-candidate-features fn))
          (feature nil))
      (while candidates
        (require (car candidates))
        (if (not (fboundp fn))
            (pop candidates)
          (setq feature (car candidates))
          (setq candidates nil)))
      feature)))

(defun oo-candidate-features (fn)
  "Return a list of candidate features for FN.
FN is a function symbol.  Look in the load path for names that match features."
  (let ((candidates nil)
        (base nil)
        (fname (symbol-name fn)))
    (for! (path load-path)
      (setq base (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
      (when (string-prefix-p base fname)
        (collecting! candidates (intern base))))
    (seq-sort-by (-compose #'length #'symbol-name) #'> candidates)))

(defun oo-autoload-fn (fn &optional feature)
  "If FN is bound return FN, otherwise return an interactive lambda."
  (unless (and (symbolp fn) (fboundp fn))
    (alet `(lambda (&rest _)
	         (interactive)
	         (if-let (feature (or ',feature (car (oo-possible-features #',fn))))
		         (progn (fmakunbound #',fn)
			            (message "Autoloading %s from %s" #',fn feature)
			            (require feature)
			            (cond ((fboundp #',fn)
			                   (alet (symbol-function #',fn)
				                 (if (keymapp it)
				                     (set-transient-map it)
				                   (call-interactively #',fn))))
			                  (t
			                   (error "Not able to load %s from %s." #',fn feature))))
	           (error "Not able to find feature for %s." #',fn)))
      (fset fn it)))
  fn)

(defmacro catch-autoloads! (&rest body)
  "Try to autoload any unbound functions."
  (let ((err (gensym "error"))
        (void-fn (gensym "void-function")))
    `(condition-case ,err
         (progn ,@body)
       (void-function
        (let ((,void-fn (cadr ,err)))
          (mapc #'oo-try-load-feature (-select #'symbolp (flatten-tree ',body)))
          (progn ,@body))))))

(provide 'oo-base-autoload)
