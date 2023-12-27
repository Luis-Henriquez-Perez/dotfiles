(require 'oo-base-block-definers)

;; ***** guess possible features from a function symbol
;; With the [[][autoload]] function elispers usually provide.  however, it is very
;; possible to.  There are a few packages that have abnormally named functions, but
;; these are by and large uncommon.
;; #+begin_src emacs-lisp
(defun oo-candidate-features (fn)
  "Return a list of candidate features for FN.
FN is a function symbol.  Look in the load path for names that match features."
  (let ((candidates nil)
        (base nil)
        (fname (symbol-name fn)))
    (for! (path load-path)
      (setq base (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
      (when (s-prefix-p base fname)
        (collecting! candidates (intern base))))
    (seq-sort-by (-compose #'length #'symbol-name) #'> candidates)))

;; generating my own custom autoload function
;; I'd like to avoid relying on autoloads from an autoload file.  Binding keys
;; provides a distinct opportunity to autoload commands considering we already name
;; the commands in the binding.
(defun oo-autoload-fn (fn &optional feature)
  "If FN is bound return FN, otherwise return an interactive lambda."
  (unless (and (symbolp fn) (fboundp fn))
    (alet `(lambda (&rest _)
             (interactive)
             (if-let (feature (or ',feature (car (oo-candidate-features #',fn))))
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

;; I have written several functions whose last arguments are =FN= and =&rest args= such
;; as [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]] and [[file:snapshots/_helpful_function__oo-call-after-keymap_.png][oo-call-after-keymap]].  And when I was trying to compose
;; these functions together in the body of multiple =oo-bind= functions I realized
;; that composing more than two of such functions produced a very long line.  The
;; line would have several sharp quoted functions. And I found it difficult to read
;; and understand what's going on.  Usually, I try to imagine what I want and when
;; I did that I found myself writing a [[file:snapshots/_helpful_macro__thread-last_.png][thread-last]] form. This is similar to
;; [[file:snapshots/_helpful_macro__thread-last_.png][thread-last]] and [[file:snapshots/_helpful_macro__->>_.png][->>]].  Except it is designed for functions that need a function
;; symbol and function arguments as their arguments such as [[file:snapshots/_helpful_function__funcall_.png][funcall]] and
;; [[file:snapshots/_helpful_function__oo-call-after-load_.png][oo-call-after-load]].
(defmacro! thread-partial! (&rest body)
  (flet! sharpquote (obj) `(function ,obj))
  (flet! sharpquote-first (obj) (cons (sharpquote (car obj)) (cdr obj)))
  (append (-last-item body)
          (apply #'append (reverse (-map #'sharpquote-first (-butlast body))))))

(defalias '-partial-> 'thread-partial!)
(defalias '-p-> 'thread-partial!)

;; One thing is the fact that because it is a function it can be composed and
;; chained.  Another is I can swap in and out the =condition-case= body and handlers
;; without having to write out the whole =condition-case= form.  Even though the
;; =condition-case= form is relatively simple I have admittedly had trouble
;; remembering its components off the top of my head.

;; I'd like to have the signature be something like ~(function &rest args &key ...)~;
;; that way it would be truly analogous to =funcall=. However, then the signature
;; would ambiguous if =function= has arguments that are the same as keys specified by
;; =&key=.
(defun! oo-condition-case-fn (fn &key (handlers 'error) (action #'ignore))
  "Return a function that calls ACTION when errors matching HANDLERS are raised.
ACTION is a function with three arguments the error object, FN and the list of
arguments FN will be called with."
  ;; To be honest I'm not sure if I need to make a gensym for the variable
  ;; `err'.  I do it just in case.
  (let! err (gensym "err"))
  `(lambda (&rest args)
     (condition-case ,err
         (apply #',fn args)
       (,handlers (funcall #',action ,err #',fn args)))))

;; This function is very similar to dash's [[file:snapshots/_helpful_function__-first_.png][-first]] or cl-lib's [[file:snapshots/_helpful_function__cl-find-if_.png][cl-find-if]].
;; These functions take a predicate and a list and they return the first element of
;; the list for which ~(pred element)~ returns non-nil.  The function =oo-first-success= also takes a
;; predicate and the list, but instead it returns the first non-nil return value of
;; ~(pred element)~.  For example, ~(oo-first-sucess 'numberp '(a t 0))~ would return
;; =t= instead of =0= as it would for =-first= or =cl-find-if= because ~(numberp 0)~ evaluates
;; to =t=. The name of this function is inspired by a similar function designed for
;; hooks [[file:snapshots/_helpful_function__run-hooks-with-args-until-success_.png][run-hook-with-args-until-success]].
(defun! oo-first-success (fn list)
  "Return the first non-nil (fn x) in LIST, else nil."
  (--each-while list (not (let! success (funcall fn it))))
  success)

;; ***** action for =oo-funcall-autoload=
;; :PROPERTIES:
;; :ID:       20230922T112858.778152
;; :END:
;; This is what should happen in the event of either a =void-function= or
;; =void-variable= error.  I separated this from =oo-funcall-autoload= because I
;; thought it was just too much to put in one function.  Also I wanted to use
;; =block!= for this but.
;; #+begin_src emacs-lisp
(defun! oo-autoload-action-function (error function args)
  "Try guesses to see if feature is bound.
ERROR is either a void-variable or void-function error."
  (let! (type symbol . rest) error)
  (cl-assert (member type '(void-variable void-function)))
  (let! bound-fn (cl-case type (void-variable #'boundp) (void-function #'fboundp)))
  (let! candidates (oo-candidate-features symbol))
  (dolist (feature candidates)
    (require feature nil t)
    (when (funcall bound-fn symbol)
      (return! (apply #'oo-funcall-autoload function args))))
  (signal (car error) (cdr error)))

;; This function is meant to act as a better replacement for autoloading.  It tries
;; to take advantage of the fact.

;; I tried making this as a macro called [[][catch-autoloads!]] but I ran in to
;; some issues.  First, my initial implementation ran into infinite recursion
;; during macroexpansion.  And then after I fixed that I had problems with lexical
;; binding.
(defun! oo-autoload-function (fn)
  "Call FN with ARGS trying to load features of any undefined symbols.
If an void-function or void-variable error is raised try to guess the parent
feature."
  (oo-condition-case-fn fn :handlers '(void-function void-variable) :action #'oo-autoload-action-function))

;; This is what should happen in the event of either a =void-function= or
;; =void-variable= error.  I separated this from =oo-funcall-autoload= because I
;; thought it was just too much to put in one function.  Also I wanted to use
;; =block!= for this but.
(defun! oo-autoload-action-function (error function args)
  "Try guesses to see if feature is bound.
ERROR is either a void-variable or void-function error."
  (let! (type symbol . rest) error)
  (cl-assert (member type '(void-variable void-function)))
  (let! bound-fn (cl-case type (void-variable #'boundp) (void-function #'fboundp)))
  (let! candidates (oo-candidate-features symbol))
  (dolist (feature candidates)
    (require feature nil t)
    (when (funcall bound-fn symbol)
      (return! (apply #'oo-funcall-autoload function args))))
  (signal (car error) (cdr error)))

(defun! oo-log-error-fn (fn)
  "Return a function that logs any errors instead of raising them."
  (flet! log (error fn args)
    (lgr-error oo-lgr "%s raised an %s error because of %S."
               (if (symbolp fn) fn "anonymous function")
               (car error)
               (cdr error))
    (pushing! oo-errors (cons fn error)))
  (oo-condition-case-fn fn :action #'log))

(provide 'oo-base-function-generators)
