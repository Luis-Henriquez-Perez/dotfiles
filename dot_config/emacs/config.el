;; #+TITLE: Emacs
;; #+AUTHOR: Luis Henriquez-Perez
;; #+PROPERTY: header-args :mkdirp yes :tangle ~/.config/emacs/init.el

;; [[file:snapshots/emacs-logo.png]]

;; This file constitutes my emacs configuration and settings.  When you have a long,
;; non-trivial emacs configuration documentation becomes *extremely* important.
;; * emacs
;; :PROPERTIES:
;; :ID:       20230801T181521.277553
;; :END:
;; Emacs is a lisp interpreter masquerading as a text-editor.
;; ** about
;; :PROPERTIES:
;; :ID:       20230929T111323.550699
;; :END:
;; *** why Emacs?
;; :PROPERTIES:
;; :ID:       20231001T045900.022457
;; :END:
;; *** why I choose an org mode configuration over an elisp one
;; :PROPERTIES:
;; :ID:       20230801T072846.682395
;; :END:
;; Choosing a literate configuration over a standard one was a hard decison.  And to
;; really understand the reasoning I had to migrate back and forth multiple times
;; between the two.  As I see it the main advantage of an elisp configuration is
;; that it is consistent with the way emacs itself organizes its files.

;; An org configuration is highly consistent with its org markup language. You can
;; use org for all of your dotfiles and that means all of your dotfiles will be
;; documented in the same syntax.  Furthermore, you modularize. You get a dotfile
;; manager for free.

;; What tipped the scales for choosing a literate configuration is that I already
;; had lots of commentary and documentation for my code--much more than in normal
;; Emacs configurations; and I used links and emphasis markers extensively.
;; Ultimately, I felt that it was better to go all in with =Org= than to half-ass use
;; it in an elisp configuration.
;; *** essentials
;; :PROPERTIES:
;; :ID:       20230822T163621.794346
;; :END:
;; **** load =seq=
;; :PROPERTIES:
;; :ID:       20230731T185833.788166
;; :END:
;; The package =seq= is a built in library of sequence manipulation function that
;; are in all likelyhood influenced by the popularity of =dash=.  Difference between
;; this and dash, though, is that dash is primarily focused on lists but this
;; library's functions and macros work on generic sequences.
(require 'seq)
;; **** load anaphora
;; :PROPERTIES:
;; :ID:       20230731T163834.298104
;; :END:
;; This package differs from dash in that I'm not certain if it is a dependency of
;; many packages--probably not; however, its macros are quite useful for me.
(require 'anaphora)
;; **** load dash
;; :PROPERTIES:
;; :ID:       20230731T163543.094048
;; :END:
;; By and large I strive not to load packages immediately because doing so slows
;; emacs startup.  But I use =dash= pervasively in my configuration--and a great many
;; packages depend on dash so I will likely end up loading it anyway.
(require 'dash)
(require 'dash-functional)
;; **** load shut-up
;; :PROPERTIES:
;; :ID:       71681f9f-2760-4cee-95a0-4aeb71191a42
;; :END:
;; This package provides a macro named =shut-up= that as its name suggests,
;; silences output of any forms within it :speak-no-evil:.  Emacs itself and many
;; emacs packages spew messages.  While I can see how in certain circumstances there
;; messages can be useful, most of the time they are superfluous.
(require 'shut-up)
;; **** setup extra hooks inspired by doom
;; :PROPERTIES:
;; :ID:       20230814T023212.445811
;; :END:
(require 'on)
;; **** enable logging
;; For debugging and good introspectability it's important to keep a log of
;; what's going on.  It's easy to lose track of what's happening as your Emacs
;; configuration grows and becomes more complex.  The way =lgr= works is I need
;; to create a =lgr= objct.
(require 'lgr)

(defvar oo-log-buffer (get-buffer-create "*oo-log*")
  "Buffer where information should be logged.")

(defvar oo-lgr (-> (lgr-get-logger "oo")
                   (lgr-add-appender (lgr-appender-buffer :buffer oo-log-buffer)))
  "Object used for logging.")
;; Additionally, prevent the log buffer from being killed as it is an important buffer.
(with-current-buffer oo-log-buffer (emacs-lock-mode 1))
;; **** load s
;; :PROPERTIES:
;; :ID:       20230827T092804.957847
;; :END:
;; =s= is an api for strings inspired by [[id:704fc35f-0ad0-4eb3-9eb5-d8335465dbd8][dash]].  It has many useful string functions
;; that are not built-in to Emacs.  Notably, it is functional.
(require 's)
;; **** noflet
;; :PROPERTIES:
;; :ID:       20230827T191737.314575
;; :END:
;; This package provides a macro named (you guessed it) =noflet=. A similar macro
;; to this used to be in Emacs. Overriding a function via =means= that the function
;; can be used anywhere.
(require 'noflet)
;; *** library
;; :PROPERTIES:
;; :ID:       20230825T135645.917210
;; :END:
;; This is the library of functions and macros that I define and use throughout the
;; rest of my configuration.  The goal behind them is to let me configure my Emacs
;; in a way that is easy, concise, precise and performant.  The macros here should
;; eliminate unnecessary boilerplate resulting in short but idiomatic emacs-lisp.
;; And in doing this, these macros will relieve me of unnecessary thought.
;; **** prime functions
;; :PROPERTIES:
;; :ID:       20230827T094608.927942
;; :END:
;; ***** oo-ampersand-symbol-p
;; :PROPERTIES:
;; :ID:       20221225T133943.556159
;; :END:
;; Emacs uses these symbols as sugars in =defun= and =defmacro= signatures.  I'm
;; defining a function specifically for identifying these symbols so I can
;; differentiate them from actual argument symbols.
(defun oo-ampersand-symbol-p (obj)
  "Return non-nil of OBJ is an ampersand symbol.
An ampersand symbol is a symbol that starts with `&'."
  (and (symbolp obj) (string-match-p "\\`&" (symbol-name obj))))
;; ***** oo-sharp-quoted-p
;; :PROPERTIES:
;; :ID:       20230813T070645.166068
;; :END:
(defsubst oo-sharp-quoted-p (obj)
  "Return non-nil if OBJ is sharp quoted."
  (equal (car-safe obj) 'function))
;; ***** wrap forms with wrappers
;; :PROPERTIES:
;; :ID:       20230807T184006.274165
;; :END:
;; This function is more for helping me write macros than for anything else.  It's
;; easy to wrap one form around a macro.  But this function automates the process of
;; wrapping =N= wrappers around a set of forms.
(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of lisp forms.  WRAPPER are a list of forms."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (-snoc wrapper forms)))
  forms)
;; ***** oo-non-keyword-symbol-p
;; :PROPERTIES:
;; :ID:       7fed6c75-60dd-4cb3-924e-62f276c5b065
;; :END:
;; Being able to distinguish between a non-keyword symbol is useful enough to merit
;; its own function.
(defun oo-non-keyword-symbol-p (object)
  "Return t if OBJECT is a symbol but not a keyword."
  (declare (pure t) (side-effect-free t))
  (and (symbolp object) (not (keywordp object))))
;; ***** convert a list of arguments to a string
;; :PROPERTIES:
;; :ID:       60f22f98-8204-45ae-9943-f19cdfe60459
;; :END:
;; This function is for converting something to a string, no questions
;; asked.  Similar to [[id:06bfc6f7-4c51-44e7-b32e-1434a602b55b][oo-symbol-intern]], I use it when I don't want to be bothered
;; with details and just want a string.
(defun oo-args-to-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))
;; ***** convert a list of arguments to a symbol
;; :PROPERTIES:
;; :ID:       06bfc6f7-4c51-44e7-b32e-1434a602b55b
;; :END:
;; This is a convenience function for quickly banging out a custom symbol.  For
;; doing this I could just use an idiom like ~(intern (format...))~.This idiom is
;; likely superior when I want a very specifically formatted symbol.  But with very
;; simple symbols, using this function can save me having to provide a string for
;; =format=.
(defun oo-args-to-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-args-to-string args)))
;; ***** convert a list of arguments into a keyword
;; :PROPERTIES:
;; :ID:       0618b8d7-e0a4-4e3e-8d89-b7d0ebe43917
;; :END:
;; Sometimes I want to create a keyword by interning a string or a symbol.  This
;; commands saves me having to add the colon at the beginning before interning.
(defun oo-args-to-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-args-to-symbol ":" args))
;; ***** invoke a function silently
;; :PROPERTIES:
;; :ID:       86007e07-0e54-4007-b990-1ba8d2a933aa
;; :END:
(defun oo-funcall-silently (function &rest args)
  "Call function silently.
Don't produce any output to the *Messages* buffer when calling function."
  (shut-up (apply function args)))
;; ***** register buffers to open at the bottom
;; :PROPERTIES:
;; :ID:       20230824T154801.717011
;; :END:
(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (alet `(,regexp
          (display-buffer-at-bottom)
          (side bottom)
          (slot 1)
          (window-height 0.5)
          (window-parameters ((no-other-window t))))
    (push it display-buffer-alist)))
;; ***** select symbols from a tree
;; :PROPERTIES:
;; :ID:       20230906T184925.910863
;; :END:
(defun oo-symbols (regexp tree)
  "Return symbols that match REGEXP in TREE."
  (thread-last (flatten-list tree)
               (--select (and (symbolp it) (oo-symbol-match-p regexp it)))
               (-uniq)))
;; ***** see if symbols match a string
;; :PROPERTIES:
;; :ID:       20230906T185202.488397
;; :END:
;; I find myself using the idiom ~(string-match-p regexp (symbol-name symbol))~ enough times.
(defsubst oo-symbol-match-p (regexp symbol)
  "Return non-nil if SYMBOL matches REGEXP."
  (string-match-p regexp (symbol-name symbol)))
;; ***** generate a function that checks if a symbol is bound
;; :PROPERTIES:
;; :ID:       20230908T092316.906858
;; :END:
;; For I want my advices, hooks and bindings to be dynamic in the sense that they only take effect when
;; it makes sense to do so.  For example [[id:20230801T175939.021467][this headline]] I append =evil-append-line= to
;; org-insert-heading-hook=.  But obviously I don't want this to happen if for whatever reason I'm not
;; in =evil-mode=.
(defun oo-bound-and-true-fn (symbol)
  "Return a function that checks if SYMBOL is bound and non-nil."
  `(lambda () (bound-and-true-p ,symbol)))
;; **** loop! - a generic looping macro
;; :PROPERTIES:
;; :ID:       20230801T062217.710179
;; :END:
;; This generic looping macro with predicate clauses inspired by =loopy=.  The goal
;; is to provide a unified syntax to cover all of my looping needs.  It should
;; "do-what-I-mean" whenever possible.

;; An implementation note: You might wonder why I check whether its a list if
;; =seq-doseq= already deals with the case that the sequence is a list.  The reason
;; is that =seq-doseq= is a wrapper around =seq-do= which works by wrapping the
;; iteration body in a lambda and calling it on every single iteration.  The lambda
;; adds an overhead.  So the answer is I do this for better performance when dealing
;; with lists.
(defmacro loop! (pred &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
This is the same as `dolist' except argument is MATCH-FORM.  match-form can be a
symbol as in `dolist', but.  LIST can be a sequence."
  (declare (indent 1))
  (pcase pred
    ;; ((pred oo-sharp-quoted-p)
    ;;  `(while (funcall ,pred) ,@body))
    ((or `(repeat ,n) (and n (pred integerp)))
     (cl-once-only (n)
       `(if (integerp ,n)
            (dotimes (_ ,n) ,@body)
          (error "Wrong type argument integerp: %S" ,n))))
    (`(,(and match-form (pred sequencep)) ,list)
     (alet (make-symbol "var")
       `(for! (,it ,list)
          (-let [,match-form ,it]
            ,@body))))
    (`(,(and var (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
               (dolist (,var ,list) ,@body))
              ((sequencep ,list)
               (seq-doseq (,var ,list) ,@body))
              ((integerp ,list)
               (loop! ,list ,@body))
              (t
               (error "Unknown list predicate: %S" ',pred)))))))

(defalias 'for! 'loop!)
;; **** combine =-setq= and =->>=
;; :PROPERTIES:
;; :ID:       20230801T164346.492565
;; :END:
;; The threading macro [[][->>]] can be particularly useful when you want to.  I've
;; often had cases in my code where I've.
(defmacro let-thread-last! (var &rest forms)
  "Bind VAR to the result of threading FORMS with `thread-last'."
  (declare (indent defun))
  `(-setq ,var (->> ,@forms)))

(defalias 'let->>! 'let-thread-last!)

(require 'oo-modification-macros)

;; **** autoloading functions
;; :PROPERTIES:
;; :ID:       20230827T094457.498083
;; :END:
;; ***** guess possible features from a function symbol
;; :PROPERTIES:
;; :ID:       20230807T120158.754139
;; :END:
;; With the [[][autoload]] function elispers usually provide.  however, it is very
;; possible to.  There are a few packages that have abnormally named functions, but
;; these are by and large uncommon.
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
;; ***** generating my own custom autoload function
;; :PROPERTIES:
;; :ID:       20230807T120406.285007
;; :END:
;; I'd like to avoid relying on autoloads from an autoload file.  Binding keys
;; provides a distinct opportunity to autoload commands considering we already name
;; the commands in the binding.
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
;; **** utility functions that depend on custom defun-like macros
;; :PROPERTIES:
;; :ID:       20230911T151324.348713
;; :END:
;; ***** define a function variant of =condition-case=
;; :PROPERTIES:
;; :ID:       20230921T150555.713342
;; :END:
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
;; ***** oo-first-success
;; :PROPERTIES:
;; :ID:       20231016T135328.931506
;; :END:
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
;; ***** oo-autoload-function
;; :PROPERTIES:
;; :ID:       20230922T092626.547932
;; :END:
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
;; ***** oo-log-error-fn
;; :PROPERTIES:
;; :ID:       20230921T144936.135609
;; :END:
(defun! oo-log-error-fn (fn)
  "Return a function that logs any errors instead of raising them."
  (flet! log (error fn args)
    (lgr-error oo-lgr "%s raised an %s error because of %S."
               (if (symbolp fn) fn "anonymous function")
               (car error)
               (cdr error))
    (pushing! oo-errors (cons fn error)))
  (oo-condition-case-fn fn :action #'log))
;; ***** substitute designated delimiters with lisp forms
;; :PROPERTIES:
;; :ID:       20230909T103354.849433
;; :END:
;; I have been dissatisfied with using excessive =,= and =,@= syntax with backquotes for building a
;; form.  Often the body can get confusing, large, and difficult to understand at a glance.  For
;; example, look at [[][this version]] of the macro. This
;; function allows me to do such form building incrementally and with normal lisp syntax as opposed to
;; backquote syntax.  Compare [[][]].
(defun! oo--subst (tree substs delimiter)
  "Replace nodes in TREE that match DELIMITER with SUBSTS."
  (let! zipper (treepy-list-zip tree))
  (while (and substs (not (treepy-end-p zipper)))
    (when (equal (treepy-node zipper) delimiter)
      (setq zipper (treepy-replace zipper (pop substs))))
    (setq zipper (treepy-next zipper)))
  (treepy-root zipper))
;; ***** main function to substitute
;; :PROPERTIES:
;; :ID:       20230909T152431.393800
;; :END:
;; This function is the "font-end" to [[][]].  I split it into two because I wanted two things.  One is I
;; wanted a function with a small number of argument to [[][format]].  But I also didn't want to forgoe
;; the potential uses of different "delimiters".  For now I used the symbol =$= but.  I specifically
;; use the dollar sign (=$=) as a delimiter because I almost never see it used in emacs or external
;; packages.  The one time I saw it was.
(defun oo-subst (tree &rest substs)
  (oo--subst tree substs '*))
;; **** define the function that adds hooks
;; :PROPERTIES:
;; :ID:       20230806T221225.154293
;; :END:
;; Adding hooks here I strive for the following goals.
;; 1 - all hook should follow the naming convention of =hook&function=
;; For example, a function symbol =foo= added to =emacs-lisp-mode-hook= would result in
;; =emacs-lisp-mode-hook&foo=.
;; 2 - hooks should try to load hook function if it is not bound
;; Hooks provide the perfect opportunity to autoload features.
;; 3 - if the hook function raises an error, suppress it but log the error
;; If a function raises an error while running a hook, the hook "short circuits".
;; By which I mean the following hooks are not run.  So if you have functions =a=, =b=, =c=,
;; and =d= in =foo-mode-hook= and =b= raises an error when the hook is run then functions =c=
;; and =b= won't be called.
;; 4 - provide a way to add a hook that expires
;; This is a feature popularized by =DOOM= and honestly I think it's only relevant
;; for those who are really try-harding to have a very fast and performant Emacs
;; configuration (like me).
;; 5 - log the occurrence of the hook
;; Hooks provide a great opportunity for knowing what is happening in your Emacs
;; configuration because every mode has.  You easily note whether a hook that
;; should be called isn't.  And you can see any errors in hooks.
(defun! oo-add-hook (hook function &key name args depth append local when mode expire no-log)
  "Generate a function that behaves like FUNCTION and add it to HOOK. 
DEPTH and LOCAL are the same as in `add-hook'.  When EXPIRE is non-nil, each
the generated function will remove itself from SYMBOL after it is run once.  If
EXPIRE is a function it should take no arguments and return non-nil when the hook should be
removed.  If NO-LOG, don't log the hook.  If NO-AUTOLOAD don't try to autoload FUNCTION.  ARGS
is be a list of arguments that should be passed in to FUNCTION when called"
  (let! name (or name (oo-args-to-symbol hook '& (if (symbolp function) function (gensym "anonymous-hook-")))))
  (let! body `(apply (oo-log-error-fn (oo-autoload-function #',function)) args))
  (when expire
    (setq body (oo-subst `(prog1 * *) body `(when ,expire (remove-hook ',hook ',name)))))
  (unless no-log
    (setq body (oo-subst `(progn * *) `(lgr-info oo-lgr "Running hook `%s'" ',name) body)))
  (when when
    (setq body (oo-subst `(when (funcall #',when) *) body)))
  (when mode
    (setq body `(when (bound-and-true-p ,mode) ,body)))
  (when args
    (setq body (oo-subst `(progn (ignore args) (let ((args (append ',args args))) *)) body)))
  (fset name `(lambda (&rest args) ,body))
  (add-hook hook name (or depth append) local))
;; **** defhook!
;; :PROPERTIES:
;; :ID:       20230807T140654.241044
;; :END:
;; This is a convenience macro for creating hooks.  It is the counterpart to
;; =defadvice!=.  Like =defadvice!=, it allows me to define hooks in a concise,
;; easily-readable, and familiar way.
(defmacro! defhook! (&rest rest)
  "Define a hook and add it to SYMBOL and SYMBOLS.
DOCSTRING and BODY are the docstring and body (respectively) of the defined
hook.  Optionally, this macro takes key-value pairs passed in as either.

Optional keyword arguments:

- `:args' the arguments of the hook, `(&rest)' by default.
- `:depth' - same as `depth' in `add-hook'.
- `:append' - same as `:depth'.
- `:local' - same as `local' in `add-hook'.
- `:expire' - a function that returns non-nil when the advice should be removed.

(fn NAME (SYMBOL [SYMBOLS] [KEY VALUE]...) [DOCSTRING] [[KEY VALUE]...] [BODY])"
  (declare (indent defun) (doc-string 3))
  (let! (name symbols metadata plist body) (oo-destructure-defun-plus rest))
  (dolist (symbol symbols)
    (let! args (or (plist-get plist :args) '(&rest _)))
    (let! lambda `(lambda ,args ,@metadata (block! nil ,@body)))
    (let! fname (intern (format "%s&%s+" symbol name)))
    (collecting! forms `(oo-add-hook ',symbol #',lambda ,@plist :name ',fname)))
  (macroexp-progn forms))
;; **** oo-add-advice
;; :PROPERTIES:
;; :ID:       eba3ee05-6adf-4474-9476-867107988705
;; :END:
;; There are two ways that I name an advice.  One is =SYMBOL@FUNCTION=.  The other is
;; =SYMBOL@A-NAME+=.  The former is when I generate an advice based on an existing
;; function.  While the latter is when I create a completely new advice.  The reason
;; I create this function is because I want to create dynamically named advices and
;; macros such as =defadvice!= are not good for this because their arguments are
;; unevaluated.
(defun! oo-add-advice (symbol place function &key name expire props when)
  "Return a function that advises SYMBOL at PLACE.
PROPS is the same as in `advice-add'.  When EXPIRE is non-nil, each function will
remove itself as advice after it is run once.  If EXPIRE is a function, call it
on the return value in order to determine whether to remove a function as advice."
  (let! name (or name (if (symbolp function) function (gensym "anonymous-advice-"))))
  (let! new-advice (oo-args-to-symbol symbol '@ name))
  (fset new-advice
        `(lambda (&rest args)
           (aprog1 (apply (oo-autoload-function #',function) args)
             (when (and ',expire (or (not (functionp ',expire)) (funcall ',expire it)))
               (advice-remove ',symbol ',new-advice)))))
  (advice-add symbol place new-advice props)
  new-advice)
;; **** defadvice!
;; :PROPERTIES:
;; :ID:       c0051221-0052-4828-8cdf-79c508c3c6b4
;; :END:
;; This macro lets me define advices in a concise, declarative and easy to read
;; syntax.  Additionally, I can name the advices with a meaningful name that can be
;; distinguished from other advices.  Note that these advices are named in the
;; format.

;; Although this function is primarily for side-effects, it seems--looking at
;; =defun= and =defadvice=--that the convention is to return the name of the
;; function defined by the macro.  I wasn't sure what to do in this case because
;; this macro can potentially define more than one function.  I chose to have it
;; return a the name of the defined advice if only one is defined; and a list of
;; advices if multiple are defined.  I'm unsure if this is the best
;; solution--perhaps always just returning a list is simpler and more
;; consistent.  We'll see.

;; Note that I allow the macro to take key-value pairs as an alist just before the
;; body as well as a plist (like =cl-defun='s =&key=) in the arglist.  This is not a
;; new idea--some evil macros allow this.  The reason for it is that the arglist can
;; easily become really long; and it looks bad if we indent it.
(defmacro! defadvice! (&rest args)
  "Define an advice for FUNCTION called FUNCTION@NAME+.
DOCSTRING and BODY are the docstring and body of the defined advice.  PLACE is
the same as in `add-function' except it is a symbol instead of a keyword.  FLAGS
is set of key value pairs.  Optionally,

Optional keyword arguments:
- `:args' - the arguments for the advice, \(&rest _) by default.
- `:props' - the same as in `advice-add'.
- `:expire' - a function that returns non-nil when the advice should be removed.

\(fn NAME (PLACE FUNCTION [OTHER-FUNCTIONS] [KEY VAL]...) [DOCSTRING] [interactive-form] [[KEY VAL]...]
BODY...)"
  (declare (indent defun) (doc-string 3))
  (let! (name (place . symbols) metadata plist body) (oo-destructure-defun-plus args))
  (dolist (symbol symbols)
    (let! lambda `(lambda ,(or (plist-get plist :args) '(&rest _)) ,@metadata ,@body))
    (let! kargs (map-delete (plist-put plist :name (macroexp-quote (oo-args-to-symbol name '+))) :args))
    (pushing! forms `(oo-add-advice ',symbol ,(oo-args-to-keyword place) ,lambda ,@kargs)))
  (macroexp-progn (nreverse forms)))
;; **** pop!
;; :PROPERTIES:
;; :ID:       ce534ac7-687e-4506-90ca-26ada8cc4309
;; :END:
(defmacro pop! (place &optional pred)
  "Set place to the result of."
  (mmt-with-gensyms (collected)
                    (cond ((null pred)
                           `(list (pop ,place)))
                          ((numberp pred)
                           `(let (,collected)
                              (dotimes (_ ,pred)
                                (pushing! ,collected (pop ,place)))
                              (nreverse ,collected)))
                          (t
                           `(let (,collected)
                              (while (and ,place (funcall ,pred (car ,place)))
                                (pushing! ,collected (pop ,place)))
                              (nreverse ,collected))))))
;; **** oo-call-with-advice
;; :PROPERTIES:
;; :ID:       20231101T190119.672566
;; :END:
;; I had already considered this function before when I first saw the concept as a
;; macro variant in [[https://emacs.stackexchange.com/questions/16490/emacs-let-bound-advice][this question]].  The use of [[file:snapshots/_helpful_macro__unwind-protect_.png][unwind-protect]] is to ensure that the
;; advice is removed after =FN= is called even if =FN= raises an error.
;; #+begin_src elisp
;; (defun! oo-call-with-advice (advices fn &rest args)
;;   "Call FN with ARGS with ADVICES active.
;; ADVICES is a list whose elements are of the form (HOW SYMBOL FUNCTION).  HOW,
;; SYMBOL and FUNCTION are the same as in `advice-add'.  Each advice specified by
;; ADVICES is done for the duration of FN applied to ARGS."
;;   (unwind-protect
;;       (progn (for! ((place symbol advice) advices)
;;                (advice-add symbol place advice))
;;              (let! result (apply fn args)))
;;     (for! ((_ symbol advice) advices)
;;       (advice-remove symbol advice)))
;;   result)
;; #+end_src
;; **** with-advice!
;; :PROPERTIES:
;; :ID:       20231102T091345.242561
;; :END:
;; This is the macro variant of [[id:20231101T190119.672566][oo-call-with-advice]].  This is sometimes more
;; convenient when I have arbitrary lisp that is not contained in a function.  I
;; will note though that =ADVICES= is not evaluated so this is useful only when I
;; already have function symbols ready as opposed to =oo-call-with-advice= where I
;; can have lisp forms that return functions.  Maybe I should let the macro
;; evaluate =ADVICES=? Not sure.
;; #+begin_src elisp
;; (defmacro with-advice! (advices &rest body)
;;   "Evaluate BODY with advices specified by ADVICES.
;; ADVICES is the same as in `oo-call-with-advice'."
;;   (declare (indent 1))
;;   `(oo-call-with-advice ',advices (lambda () ,(macroexp-progn body))))
;; #+end_src
;; **** =thread-partial=
;; :PROPERTIES:
;; :ID:       20231031T133059.398576
;; :END:
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
;; #+begin_src elisp
;; (defmacro! thread-partial! (&rest body)
;;   (flet! sharpquote (obj) `(function ,obj))
;;   (flet! sharpquote-first (obj) (cons (sharpquote (car obj)) (cdr obj)))
;;   (append (-last-item body)
;;           (apply #'append (reverse (-map #'sharpquote-first (-butlast body))))))

;; (defalias '-partial-> 'thread-partial!)
;; (defalias '-p-> 'thread-partial!)
;; #+end_src
;; **** oo--with-map-bang-let-binds
;; :PROPERTIES:
;; :ID:       20231104T100346.937332
;; :END:
;; At first I had this code in the macro [[id:20231022T103524.551620][with-map!]] but as I was writing code
;; for the [[][oo-bind]] I realized the value of having a function.  Having this
;; auxiliary gives me a much easier way to test =with-map!=.  Via the macro I have to
;; test whether the expansion is correct.  With this I can just test if its return
;; value is correct.
;; #+begin_src elisp
;; (defun! oo--with-map-bang-let-binds (map body &key regexp use-keywords)
;;   "Return the list of let bindings for `with-map!'.
;; REGEXP is the regular expression used to match symbols used for let bindings. It
;; should contain a group for the name of the symbol.
;; If USE-KEYWORDS is non-nil, use keywords instead of group symbol names."
;;   (setq regexp (or regexp "\\`\\$\\([^[:space:]]+\\)"))
;;   (let! mapvar (gensym "map"))
;;   (for! (obj (-flatten body))
;;     (when (and (symbolp obj) (s-matches-p regexp (symbol-name obj)))
;;       (let! name (funcall (if use-keywords #'oo-args-to-keyword #'intern)
;;                           (nth 1 (s-match regexp (symbol-name obj)))))
;;       (adjoining! binds `(,obj (map-elt ,mapvar ',name)) :key #'car)))
;;   (pushing! binds (list mapvar map))
;;   binds)
;; #+end_src
;; **** with-map!
;; :PROPERTIES:
;; :ID:       20231022T103524.551620
;; :END:
;; This is a macro used to.
;; #+begin_src elisp
;; (defmacro! with-map! (map &rest body)
;;   "Let bind dollar symbols to their value in MAP and evaluate BODY."
;;   (declare (indent defun))
;;   ;; (flet! option-p (form) (member (car-safe form) '(:use-keywords :prefix)))
;;   ;; (let! (&plist :use-keywords :prefix) (apply #'append (pop! body #'option-p)))
;;   `(let* ,(oo--with-map-bang-let-binds map body :use-keywords t) ,@body))
;; #+end_src
;; *** binding keys
;; :PROPERTIES:
;; :ID:       20230821T061643.475087
;; :END:
;; **** create an extensible and generic function for defining keys
;; :PROPERTIES:
;; :ID:       20231021T163136.012524
;; :END:
;; Keybindings in Emacs are extensive and multi-faceted.  There are several
;; variants of functions for defining keys; some built-in such as [[file:snapshots/_helpful_function__global-set-key_.png][global-set-key]],
;; [[file:snapshots/_helpful_function__local-set-key_.png][local-set-key]] and some provided by other packages such as [[file:snapshots/_helpful_function__evil-define-key*_.png][evil-define-key*]] from
;; [[https://github.com/emacs-evil/evil][evil]] and [[file:snapshots/_helpful_function__exwm-input-set-key_.png][exwm-input-set-key]] from [[https://elpa.gnu.org/packages/exwm.html][exwm]].  Instead of using several functions for
;; several different cases I want to have one function that can handle all these
;; cases.  I also want such a function to be extensible because there are
;; always new packages emerging with new binding functions.  It should be easy to
;; add support for new binding functions without having to rewrite or extend said
;; binding function.  Finally, I want the function to properly defer keybindings
;; based on several factors, the most common being whether they keymap is defined
;; or not (see [[id:20231018T170355.378301][defer function calls until keymap is loaded]]).
;; ***** I might need a macro for keymap deferment
;; :PROPERTIES:
;; :ID:       20231110T101425.546026
;; :END:
;; If I want to defer the keymap I need to pass in the keymap symbol
;; for the call to =oo-bind= because the arguments of a function are evaluated.  If
;; the keymap I pass in is not bound I'll get an error immediately even before the
;; function has had a chance to process the keymap because the symbol is void.  At
;; first I thought I would wrap a macro around =oo-bind= but then I realized I need
;; the conditions in the body of =oo-bind= to determine which argument is the keymap.
;; One way is simply to make =oo-bind= into a macro wrapper for
;; =oo--bind= that defers by keybinding.
;; ***** defer function until evil state is defined
;; :PROPERTIES:
;; :ID:       20231022T034224.856442
;; :END:
;; As the abbreviation =E.V.I.L.=, extensible VI layer, suggests =evil= is extensible.
;; It provides a macro =evil-define-state= I can use to define new states. I want
;; =oo-bind= to have the ability to allow me to bind to evil states even if they are
;; not defined yet.

;; To properly defer keybindings for evil states that have not yet been defined it would help
;; to have some hook I can use that runs whenever an evil state is defined.  That
;; way I can bind the keys it uses right when I need them.  I can
;; try to use the knowledge that every =evil= state has a deterministically named
;; keymap.  For example, an evil state, =foo=, will have the keymap
;; =evil-foo-state-map=.  Knowing this, I can reuse
;; [[file:snapshots/oo-call-after-keymap][oo-call-after-keymap]] with something like ~(oo-call-after-keymap
;; 'evil-foo-state-map #'evil-define-key* 'foo "k" #'definition)~.

;; I will note that, regardless, this hook will be useful when performing standard
;; operations on evil states.  Particularly for binding my evil keys.
;; ****** get me the key an evil state keyword matches
;; :PROPERTIES:
;; :ID:       20231102T145802.451799
;; :END:
;; This function is for getting the state that corresponds to an evil keyword.
;; In contrast to Doom this function only handles "mono-character" keywords.
;; #+begin_src elisp
;; (defun! oo-evil-state (evil-keyword)
;;   "Return the state that corresponds to the state keyword."
;;   (flet! letter (state) (seq-first (symbol-name state)))
;;   (let! target (seq-first (seq-rest (symbol-name evil-keyword))))
;;   (--first (equal (letter it) target) (map-keys evil-state-properties)))
;; #+end_src
;; **** evil leaders
;; :PROPERTIES:
;; :ID:       47c4c3c5-3859-46a4-a4ef-3d27c552eec0
;; :END:
;; These leaders are specifically for evil mode states (not including insert and
;; Emacs).  I choose the space (=SPC=) key for evil leaders because it is one of if
;; not the easiest key to press because of its central placement on the keyboard
;; and its sheer size--at least on the [[https://en.wikipedia.org/wiki/QWERTY][qwerty]] keyboard that I use.  The choice
;; of =SPC m= for the major mode specific keys is simply for the pnemonic =m= which
;; stands for "major mode".  The short major mode prefix key =,= is for cases when I
;; want to shorten a key binding.  Although obviously not as easy to remember as
;; =m=, it provides me with one shorter keypress in certain situations.
(defconst oo-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst oo-normal-localleader-key "SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-normal-localleader-short-key ","
  "A shorter alternative `oo-localleader-key'.")
;; **** emacs leaders
;; :PROPERTIES:
;; :ID:       f3299c73-837e-46f9-a29e-9932c4570858
;; :END:
;; These leaders are for evil insert and emacs states as well as vanilla
;; Emacs.  Note that evil Emacs state is different from vanilla Emacs.  One of the
;; goals with these bindings is to set up keybindings in the case that I disable
;; evil mode or in the case that I want to use my bindings in insert or Emacs
;; state--or even vanilla Emacs.  The choice behind the bindings is the same as
;; [[id:][before]], except I just prepended the =Meta= (a.k.a. the =Alt= key) to everything.
(defconst oo-insert-leader-key "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst oo-insert-localleader-key "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-localleader-key "C-c l m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-emacs-localleader-short-key "C-c s"
  "A short non-normal `oo-localleader-key'.")
;; **** alt! - the alternate binding macro
;; :PROPERTIES:
;; :ID:       20231013T084215.894651
;; :END:
;; Inspired by [[https://stackoverflow.com/questions/16090517/elisp-conditionally-change-keybinding][this]] stackoverflow question, this macro lets me create conditional
;; bindings for commands giving me a flexible and robust experience with key
;; bindings.  By "condition bindings" I mean key bindings that can invoke a
;; particular command based on certain conditions.  For example, =SPC h f=  might
;; invoke [[file:snapshots/_helpful_command__helpful_callable_.png][helpful-callable]] if the package helpful is present (see [[][]]), otherwise it
;; would fallback to [[file:snapshots/_helpful_command__describe-function_.png][describe-function]] instead.

;; As opposed to [[file:snapshots/_helpful_special_form__cond_.png][cond]], for example, which requires multiple conditions I designed
;; this macro to add one condition at a time.  I do not want to be tied to naming
;; all the conditions at once in general I write my configuration in such a way
;; that I can augment it incrementally as opposed to building one big block of
;; code.
(defvar oo-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(defun oo-alternate-command-choose-fn (command)
  "Return command that should be called instead of COMMAND."
  (or (oo-first-success #'funcall (gethash command oo-alternate-commands))
      command))
;; **** oo-alt-bind
;; :PROPERTIES:
;; :ID:       20231015T150557.170266
;; :END:
;; This provides a function alternative to [[][alt!]].  This function is all I am
;; going to use for alternate bindings, so I may scrap the macro variant.
(defun! oo-alt-bind (map orig alt &optional condition)
  "Remap keys bound to ORIG so ALT is called if CONDITION returns non-nil.
ORIG and ALT are command symbols.  CONDITION is a function that returns non-nil
when ALT should be invoked instead of ORIG."
  (flet! oo-when-fn (condition fn)
    `(lambda (&rest _) (when (funcall #',condition) #',alt)))
  (push (oo-when-fn (or condition #'always) alt) (gethash orig oo-alternate-commands))
  (define-key map `[remap ,orig] `(menu-item "" ,orig :filter oo-alternate-command-choose-fn)))
;; **** show the prefix being completed at the top
;; :PROPERTIES:
;; :ID:       20231110T074115.247845
;; :END:
;; I like knowing what prefix I typed in to get a certain key.
;; *** leader map bindings
;; :PROPERTIES:
;; :ID:       20230824T072915.346305
;; :END:
;; **** oo-leader-map
;; :PROPERTIES:
;; :ID:       20230723T201000.494680
;; :END:
;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)
;; **** binding the leader keys to the prefix map
;; :PROPERTIES:
;; :ID:       20231109T115326.379235
;; :END:
;; These keys need to be bound before =evil-mode-hook=, specifically right after
;; evil is loaded.  Otherwise, the bindings won't take effect immediately in buffers
;; that have already been created by emacs (such as =*scratch*= and
;; =*Messages*=).  Instead, they will only work after you switch states once.
(oo-bind 'oo-override-mode-map oo-emacs-leader-key #'oo-leader-prefix-command)
(oo-bind :i 'oo-override-mode-map oo-insert-leader-key #'oo-leader-prefix-command)
(oo-bind :nmv 'oo-override-mode-map oo-normal-leader-key #'oo-leader-prefix-command)
;; **** execute extended command
;; :PROPERTIES:
;; :ID:       20231109T122225.018350
;; :END:
;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(oo-bind 'oo-leader-map "SPC" #'execute-extended-command :wk "execute command")
(oo-bind :nmv 'oo-override-mode-map ";" #'execute-extended-command)
(oo-bind :i "A-x" #'execute-extended-command)
(oo-bind :i "M-x" #'execute-extended-command)
;; **** leader keymaps
;; :PROPERTIES:
;; :ID:       20231012T080646.472029
;; :END:
;; The leader keymaps are the keymaps that are triggered by [[my leader key]].  These maps
;; acts as the primary interface between me and emacs and should contain my most important
;; and most used bindings.  Additionally, They should be grouped by some (human)
;; logic.  I try to find a balance between assigning keys to keymaps and
;; key-bindings that are both memorable and quick to press on a QWERTY keyboard.
;; ***** manipulating windows
;; :PROPERTIES:
;; :ID:       20231012T080809.644948
;; :END:
;; My window keymap is focused on providing convenient bindings for manipulating
;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html][windows]].  Windows in Emacs are the medium through which buffers are
;; displayed.  Controlling the number of windows and the buffer each is assigned to
;; allows me to control the way information is displayed to me.  I make the window
;; I'm focusing on the largest, keeping smaller related windows near me in case I
;; want to switch to them or use them as reference for information.
;; ****** trigger the window map with =w= or =s=
;; :PROPERTIES:
;; :ID:       20230121T081551.206808
;; :END:
;; The window keymap will be accessed by pressing =<leader> w= or =<leader> s=.
;; Binding the keymap to the =w= key is more mnemonic but binding it to =s=, a QWERTY
;; homerow key, is easier to press.
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(oo-bind 'oo-leader-map "s" #'oo-window-prefix-command :wk "window")
(oo-bind 'oo-leader-map "w" #'oo-window-prefix-command :wk "window")
;; ****** maximize a window with =M=
;; :PROPERTIES:
;; :ID:       20231012T124029.490413
;; :END:
;; Of course sometimes you want to focus on a window more than others.  Typically
;; this is handled with =edwina= because it makes the master window take up more than
;; =50%= of the width--by default =55%= (see [[file:snapshots/_helpful_variable__edwina-mfact_.png][edwina-mfact]]).
(oo-bind 'oo-window-map "M" #'maximize-window :wk "maximize")
;; ****** arrange windows in a master-slave style with =a=
;; :PROPERTIES:
;; :ID:       20231012T102757.467653
;; :END:
;; I [[https://gitlab.com/ajgrf/edwina][edwina]] to automatically window configurations.  I choose the buffers I
;; want to display and it opens a window at a predictable location.  It is possible
;; that through some operation my windows could get out of wack.  It shouldn't
;; happen accidentally, but most often it will happen after I maximize or minimize
;; a window.  In that case, I can call =edwina-arrange= to proportion my windows properly.
(oo-bind 'oo-window-map "a" #'edwina-arrange :wk "arrange")
;; ****** splitting windows
;; :PROPERTIES:
;; :ID:       20231012T095920.079042
;; :END:
;; I'm unsure about whether to have any bindings splitting windows.  Since =edwina=
;; automates the way I split windows I do not use these splitting commands much.
;; However, I'm not so sure whether I should remove them entirely.
(oo-bind oo-window-map "v" #'split-window-horizontally :wk "vsplit")
(oo-bind oo-window-map "s" #'split-window-vertically :wk "split")
(oo-bind oo-window-map "V" #'oo-split-window-right-and-focus :wk "vsplit+focus")
(oo-bind oo-window-map "v" #'oo-split-window-below-and-focus :wk "split+focus")
;; ****** select a window with =w=, =j= or =o=
;; :PROPERTIES:
;; :ID:       20231012T094503.005810
;; :END:
;; There are commands such as.  I do not need these commands.  After moving left,
;; right, up or down some direction once, the effort needed to traverse a window
;; using directional window movement commands greatly increases.  The command
;; [[file:snapshots/_helpful_command__ace-window][ace-window]] in contrast scales really well with a greater number of
;; windows.  And it only loses slightly to directional window commands when moving
;; one time.

;; The command [[file:snapshots/_helpful_command__ace-window_.png][ace-window]] leverages [[https://github.com/abo-abo/avy][avy]] to select a window.  It assigns each window
;; a character (I'm using [[][letters]] close to the homerow) which it displays on
;; the upper right-hand corner of windows. I've found that
;; ace-window is the quickest way possible to switch focus form one live window to
;; another.

;; The mnemonic bind is =w= and the quick bindings--which I will likely use most
;; often--are =o= and =j=.
(oo-bind oo-window-map "w" #'ace-window :wk "select")
(oo-bind oo-window-map "j" #'ace-window :wk "select")
(oo-bind oo-window-map "o" #'ace-window :wk "select")
;; ****** swap two windows with =s=
;; :PROPERTIES:
;; :ID:       20231012T125502.066095
;; :END:
;; Often when I want to switch focus from my main window to one of its
;; subsidiaries; I will want to swap buffers from the two windows.
;; Actually, =edwina= does provide functions to do this: namely
;; [[_helpful_command__edwina-swap-next-window_.png][edwina-swap-next-window]] and [[file:snapshots/_helpful_command__edwina-swap-previous-window_.png][edwina-swap-previous-window]].
;; But I can do something similar, but much faster with.  This is a case where =s= is
;; mnemonic and easy to press.
(oo-bind oo-window-map "s" #'ace-swap-window :wk "swap")
;; ****** undo changes to window configuration with =u=
;; :PROPERTIES:
;; :ID:       20231012T103721.526349
;; :END:
;; There's a global mode called [[https://www.emacswiki.org/emacs/WinnerMode#:~:text=Winner%20Mode%20is%20a%20global%20minor%20mode%20that,included%20in%20GNU%20Emacs%2C%20and%20documented%20as%20winner-mode.][winner-mode]] that allow you to undo changes to
;; your window configuration.
(oo-bind oo-window-map "u" #'winner-undo :wk "undo")
;; ****** save window configuration with =b= or =S=
;; :PROPERTIES:
;; :ID:       20231012T105815.827013
;; :END:
;; The command [[file:snapshots/_helpful_command__burly-bookmark-windows_.png][burly-bookmark-windows]] creates a bookmark with the information
;; necessary to reproduce the current window configuration.  I can then restore the
;; window information I've bookmarked with [[file:snapshots/_helpful_command__burly-open-bookmark_.png][burly]].
(oo-bind oo-window-map "S" #'burly-bookmark-windows :wk "bookmark")
(oo-bind oo-window-map "b" #'burly-bookmark-windows :wk "bookmark")
;; ****** delete a window with =D= or =d=
;; :PROPERTIES:
;; :ID:       20230802T083002.848324
;; :END:
;; The letter =d= is both mnemonic for deleting windows and it is easy to press
;; because its own the home key.
(oo-bind oo-window-map "d" #'delete-window :wk "delete")
(oo-bind oo-window-map "D" #'delete-other-windows :wk "delete others")
;; ****** open a new window with =k=
;; :PROPERTIES:
;; :ID:       20231014T124637.707732
;; :END:
;; This binding overlaps with.  My reasoning is you can think of it as opening a
;; new window.
(oo-bind oo-window-map "k" #'display-buffer :wk "open")
;; ***** managing packages
;; :PROPERTIES:
;; :ID:       20231015T154822.354022
;; :END:
;; These keybinding concern emacs packages.
;; ****** oo-package-map
;; :PROPERTIES:
;; :ID:       20230826T034604.214779
;; :END:
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(oo-bind 'oo-leader-map "p" #'oo/package-prefix-command :wk "package")
;; ****** bindings for elpaca
;; :PROPERTIES:
;; :ID:       20230826T042513.670312
;; :END:
(oo-bind 'oo-package-map "b" #'elpaca-browse     :wk "browse")
(oo-bind 'oo-package-map "U" #'elpaca-update-all :wk "update all")
(oo-bind 'oo-package-map "u" #'elpaca-update     :wk "update")
(oo-bind 'oo-package-map "v" #'elpaca-visit      :wk "visit")
(oo-bind 'oo-package-map "i" #'elpaca-try        :wk "try")
(oo-bind 'oo-package-map "r" #'elpaca-rebuild    :wk "rebuild")
(oo-bind 'oo-package-map "d" #'elpaca-delete     :wk "delete")
(oo-bind 'oo-package-map "l" #'elpaca-log        :wk "log")
(oo-bind 'oo-package-map "m" #'elpaca-manager    :wk "manager")
;; ***** finding things quickly
;; :PROPERTIES:
;; :ID:       20231010T113043.015242
;; :END:
;; The purpose of this keymap is for finding things, hence the name "find".  In
;; general, the commands in this keymap.  Here I use the letter =f= not only as but
;; also because it is among the fastest letters to press on the QWERTY homerow.
;; ****** set the find map to =<leader> f=
;; :PROPERTIES:
;; :ID:       20230829T062931.006494
;; :END:
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(oo-bind 'oo-leader-map "f" #'oo-find-prefix-command :wk "find")
;; ****** display a buffer with =d=
;; :PROPERTIES:
;; :ID:       20231012T075406.015889
;; :END:
;; The command [[file:snapshots/_helpful_command__display-buffer_.png][display-buffer]] is one of my most used.  I use it for displaying
;; things side by side.
(oo-bind 'oo-find-map "d" #'switch-to-buffer)
(oo-bind 'oo-find-map "f" #'display-buffer)
;; ****** open a bookmark with =k= or =b=
;; :PROPERTIES:
;; :ID:       20231013T122735.615457
;; :END:
;; Again, we go with two alternate bindings.  Binding the =k= key is faster, but =b= is
;; perhaps more memorable.
(oo-bind 'oo-find-map "k" #'consult-bookmark :wk "bookmark")
(oo-bind 'oo-find-map "b" #'consult-bookmark :wk "bookmark")
;; ****** call =consult-line= with =s= or =l=
;; :PROPERTIES:
;; :ID:       20231012T075116.031108
;; :END:
;; The command [[file:snapshots/_helpful_command__consult-line_.png][consult-line]] lets me interactively search for a line in the buffer.
(oo-bind 'oo-find-map "s" #'consult-line :wk "line")
(oo-bind 'oo-find-map "l" #'consult-line :wk "line")
;; ****** open a burly bookmark with =j=
;; :PROPERTIES:
;; :ID:       20231014T100947.907182
;; :END:
(oo-bind 'oo-find-map "j" #'burly-open-bookmark)
;; ****** find headlines in an outline
;; :PROPERTIES:
;; :ID:       20231010T113105.068974
;; :END:
(oo-bind 'oo-find-map "h" #'consult-outline :wk "outline")
(oo-bind 'oo-find-map "h" #'consult-org-heading :wk "heading")
;; ***** quitting emacs
;; :PROPERTIES:
;; :ID:       20231013T041005.332381
;; :END:
;; This is a keymap for quitting Emacs--or rather, in Emacs parlance "killing" Emacs.
;; ****** oo-quit-map
;; :PROPERTIES:
;; :ID:       20230121T074341.672539
;; :END:
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(oo-bind oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")
;; ****** quit, no questions asked
;; :PROPERTIES:
;; :ID:       20231013T165521.694716
;; :END:
;; There have been time when.
;; #+begin_src elisp
;; #+end_src
;; ****** save buffers and quit
;; :PROPERTIES:
;; :ID:       ae435361-79e7-41c8-b490-8ec0f8d23a59
;; :END:
;; There's many ways to quit Emacs.  Sometimes I'd like to save all the buffers I
;; had been working on.  Sometimes, when I'm testing something and I mess up
;; [[helpvar:kill-emacs-hook][kill-emacs-hook]] I want Emacs to just quit even if it means ignoring that hook.
;; Most of the time, I know what I'm doing when I quit Emacs, so I don't want a
;; prompt asking me if I'm sure.
(oo-bind 'oo-quit-map "q" #'save-buffers-kill-emacs :wk "quit")
(oo-bind 'oo-quit-map "r" #'restart-emacs :wk "restart")
;; Should make this one restart with no prompt, just automatically save buffers
;; and exit processes.
(oo-bind 'oo-quit-map "R" #'restart-emacs :wk "restart")
(oo-bind 'oo-quit-map "E" #'restart-emacs-start-new-emacs :wk "new instance")
;; ***** take screenshots or snapshots
;; :PROPERTIES:
;; :ID:       20231013T123129.446429
;; :END:
;; Part of being a data-hoarder being able to take buffer snapshots and screenshots
;; at a moments notice.  In this headline I have the bindings to help me do that.
;; ***** help
;; :PROPERTIES:
;; :ID:       20231013T123641.662532
;; :END:
;; These are bindings for getting help.
;; ****** use =l= and =h= for getting help
;; :PROPERTIES:
;; :ID:       20231013T133942.558668
;; :END:
;; As per usual I have one convenience map.
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(oo-bind oo-leader-map "l" #'oo-help-prefix-command :wk "help")
(oo-bind oo-leader-map "h" #'oo-help-prefix-command :wk "help")
;; ****** goto the info manual
;; :PROPERTIES:
;; :ID:       20231013T163628.166141
;; :END:
(oo-bind oo-help-map "i" #'info)
;; ****** add mnemonic bindings for describe functions
;; :PROPERTIES:
;; :ID:       f28081e3-451c-4005-80cc-bffeafe0051c
;; :END:
;; Emacs has a family of describe functions that are used for help and
;; introspection.  To name a few, there's [[file:snapshots/_helpful_command__describe-function_.png][describe-function]], [[file:snapshots/_helpful_command__describe-character_.png][describe-character]].
;; The command =describe-callable=  and =describe-variable= are the ones I use the most
;; by far and I them it to be accessible quickly.  The various snapshots you see
;; named are a result of these functions and you can already guess buy how many
;; such snapshots there are how much I use these commands.
(oo-bind oo-help-map "m" #'describe-mode)
(oo-bind oo-help-map "l" #'describe-function)
(oo-bind oo-help-map "f" #'describe-function)
(oo-bind oo-help-map "j" #'describe-variable)
(oo-bind oo-help-map "v" #'describe-variable)
(oo-bind oo-help-map "h" #'describe-variable)
(oo-bind oo-help-map "C" #'describe-char)
(oo-bind oo-help-map "k" #'describe-key)
;; *** should I make a specific prefix command for localleader maps?
;; :PROPERTIES:
;; :ID:       20231118T140631.668588
;; :END:
;; *** keybindings
;; :PROPERTIES:
;; :ID:       20230801T182100.161631
;; :END:
;; For now all my keybindings are in this subtree because I want to be able to
;; profile my headlines.  I can just toggle between tangling and not tangling the
;; subtree and see how long my startup time takes with and without my bindings.
;; **** helpful binding
;; :PROPERTIES:
;; :ID:       20221212T081050.859658
;; :END:
(oo-bind :alt #'describe-function #'helpful-callable :feature 'helpful)
(oo-bind :alt #'describe-command #'helpful-command :feature 'helpful)
(oo-bind :alt #'describe-variable #'helpful-variable :feature 'helpful)
(oo-bind :alt #'describe-key #'helpful-key :feature 'helpful)
;; **** use =eros-eval-last-sexp= instead of =eval-last-sexp=
;; :PROPERTIES:
;; :ID:       20230801T211228.720895
;; :HEADER-ARGS: :tangle no
;; :END:
(oo-bind :alt #'eval-last-sexp #'eros-eval-last-sexp :feature 'eros)
;; **** bind =f= as a generic "form" text object
;; :PROPERTIES:
;; :ID:       20230802T094236.636314
;; :END:
;; The "form" textobject from =evil-cleverparens= is a generic text object for any
;; delimiters.  Typically to change inside or around delimiters I need to specify
;; the specific delimiter I want to do this for.  For example, I'd need to type
;; =ci"= for changing inside a string; likewise, =ci)= for inside a parentheses and
;; so on.  In many cases, particuarly in those where there is no ambiguity in the
;; surrounding delimiter, it is more convenient to type =cif= and have what I mean
;; be done.
(oo-bind 'evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(oo-bind 'evil-outer-text-objects-map "f" #'evil-cp-a-form)
;; **** bindings in source block
;; :PROPERTIES:
;; :ID:       df270638-f6a7-4f0e-abe7-dd0c4e7df7ce
;; :END:
;; Note that you should have bindings that are different for entering and exiting
;; source blocks.  Usually, it doesn't matter; but it is a problem when you have.
(oo-bind 'org-src-mode-map "," #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "a" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
(oo-bind 'org-src-mode-map "c" #'org-edit-src-exit :localleader t :mode 'org-src-mode)
;; **** bindings for editing source blocks
;; :PROPERTIES:
;; :ID:       0f5050ab-fa21-4f76-a14f-81d42bb7ac35
;; :END:
(oo-bind :alt #'org-edit-src-code #'oo-dwim-edit-src-code)
(oo-bind :h "," #'org-edit-src-code :localleader t)
;; (oo-bind 'org-mode-map "e" #'org-edit-src-code)
                                        ;(oo-bind ':h "es" #'org-edit-src-code :wk "source block" :localleader t)
;; **** add bindings for viewing macroexpansions in lisp code
;; :PROPERTIES:
;; :ID:       fefc5700-3bd7-4a89-ae7b-bf68368e8bc4
;; :END:
(oo-bind 'emacs-lisp-mode-map "me" #'macrostep-expand :localleader t :wk "expand")
(oo-bind 'emacs-lisp-mode-map "mc" #'macrostep-collapse :localleader t :wk "collapse")
(oo-bind 'emacs-lisp-mode-map "mC" #'macrostep-collapse-all :localleader t :wk "collapse all")
;; **** consult function integration
;; :PROPERTIES:
;; :ID:       20231124T145616.233192
;; :END:
(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)
;; **** bindings for commenting lines
;; :PROPERTIES:
;; :ID:       627bf650-0511-4c16-9686-ceb9e16545a4
;; :END:
;; These are bindings for commenting lines.
(oo-bind :n "g," #'lispyville-comment-or-uncomment)
(oo-bind :n "gc" #'lispyville-comment-and-clone-dwim)
(oo-bind :n "gl" #'lispyville-comment-and-clone-dwim)
;; **** enable abbrevs for =text-mode=
;; :PROPERTIES:
;; :ID:       20230906T105431.252657
;; :END:
;; Abbreviations is what =captain-mode= uses to auto-capitalized certain words that should always be
;; capitalized such as =Luis= or =I=. Furthermore, having access to abbreviations when writing text is
;; useful in and of itself.
(oo-add-hook 'text-mode-hook #'abbrev-mode)
;; **** binding for =oo-org/choose-capture-template=
;; :PROPERTIES:
;; :ID:       20230826T095058.752761
;; :HEADER-ARGS: :tangle no
;; :END:
(oo-bind :alt #'org-capture #'oo-choose-capture-template)
;; **** bind =g= to gumshoe in =oo-app-map
;; :PROPERTIES:
;; :ID:       20230803T054109.978278
;; :END:
(oo-bind 'oo-app-map "g" #'gumshoe-peruse-globally)
;; **** consult function integration
;; :PROPERTIES:
;; :ID:       e94f3781-157f-43f5-a479-6997510417c4
;; :END:
(oo-bind :alt #'switch-to-buffer #'consult-buffer :feature 'consult)
(oo-bind :alt #'yank-pop #'consult-yank-pop :feature 'consult)
(oo-bind :alt #'apropos #'consult-apropos :feature 'consult)
(oo-bind :alt #'man #'consult-man :feature 'consult)
;; **** expand region bindings
;; :PROPERTIES:
;; :ID:       f4d546d8-bf10-4952-b81c-599871864023
;; :END:
(oo-bind :v "V" #'er/contract-region)
(oo-bind :v "v" #'er/expand-region)
;; **** make =E= eval
;; :PROPERTIES:
;; :ID:       20230826T195700.931357
;; :END:
(oo-bind :v "E" #'lispy-eval-and-replace)
;; **** make =C-;= let me jump to different candidates
;; :PROPERTIES:
;; :ID:       20230829T191333.585024
;; :END:
(oo-bind :ie 'helm-map "C-;" #'ace-helm-jump-line)
;; **** lispy additions
;; :PROPERTIES:
;; :ID:       f70309e9-b36f-4f28-ad57-e41666b1326b
;; :END:
(oo-bind 'lispyville-mode-map :i "SPC" #'lispy-space)
(oo-bind 'lispyville-mode-map :i ";" #'lispy-comment)
;; **** create binding for additional eval operator
;; :PROPERTIES:
;; :ID:       20230825T145824.657619
;; :END:
(oo-bind :n "gr" #'evil-operator-eval)
;; **** vertico bindings
;; :PROPERTIES:
;; :ID:       1fd4ec1a-8868-400f-bfce-67947dd47e77
;; :END:
(oo-bind 'vertico-map :i "TAB" #'vertico-next)
(oo-bind 'vertico-map :i "C-k" #'vertico-previous)
(oo-bind 'vertico-map :i "C-j" #'vertico-next)
(oo-bind 'vertico-map :i ";" #'vertico-quick-exit)
(oo-bind 'vertico-map :i "C-;" #'vertico-quick-exit)
(oo-bind 'vertico-map :i [backtab] #'vertico-previous)

(oo-bind 'vertico-map :i "C-o" #'embark-act)
(oo-bind 'vertico-map :i "C-o" #'embark-collect)
(oo-bind 'vertico-map :n "C-o" #'embark-collect)
;; **** use =LEADER k k= to set a bookmark
;; :PROPERTIES:
;; :ID:       20230826T034833.143641
;; :END:
(oo-bind 'oo-miscellany-map "k" #'bookmark-set)
;; **** toggle
;; :PROPERTIES:
;; :ID:       4d45b880-24e4-4a4b-9e59-9ed33c3e2b0a
;; :END:
;; I have kind of a loose definition of "toggle" in this headline.
(defvar oo-toggle-map (make-sparse-keymap))
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(oo-bind 'oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")

(oo-bind 'oo-toggle-map "r" #'read-only-mode)
(oo-bind 'oo-toggle-map "t" #'load-theme)
(oo-bind 'oo-toggle-map "c" #'caps-lock-mode)
(oo-bind 'oo-toggle-map "r" #'redacted-mode)
(oo-bind 'oo-toggle-map "s" #'smartparens-mode)
(oo-bind 'oo-toggle-map "d" #'toggle-debug-on-error)
;; **** invoke eshell with =LEADER a e=
;; :PROPERTIES:
;; :ID:       20230826T041416.117728
;; :END:
(oo-bind 'oo-app-map "e" #'eshell)
;; **** binding for capture template
;; :PROPERTIES:
;; :ID:       20230826T091917.167519
;; :END:
;; The binding for a capture template should be fast because capturing is something that I intend to do
;; often.
(oo-bind 'oo-quick-map "j" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "a" #'org-capture :wk "capture")
(oo-bind 'oo-app-map "j" #'org-capture :wk "capture")
;; **** binding for opening a new emacs instance
;; :PROPERTIES:
;; :ID:       20230826T100833.475434
;; :END:
;; I use the capital letters =E= and =R= because these are bindings with big effects particularly
;; restarting the current emacs session. And I don't want to accidentally trigger these bindings.
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(oo-bind 'oo-leader-map "a" #'oo-app-prefix-command :wk "app")

(oo-bind 'oo-app-map "R" #'restart-emacs)
(oo-bind 'oo-app-map "E" #'restart-emacs-start-new-emacs)
;; **** add a binding for setting the font
;; :PROPERTIES:
;; :ID:       20230827T193115.932152
;; :END:
(oo-bind 'oo-toggle-map "f" #'oo-set-font-face)
;; **** bindings for evaluation
;; :PROPERTIES:
;; :ID:       3947b258-fc46-4fc9-8350-3fd25a69d749
;; :END:
(oo-bind 'emacs-lisp-mode-map "e" nil :localleader t :wk "eval")
(oo-bind 'emacs-lisp-mode-map "er" #'lispy-eval-and-replace :localleader t)
(oo-bind 'emacs-lisp-mode-map "eb" #'eval-buffer :localleader t)
(oo-bind 'emacs-lisp-mode-map "ed" #'eval-defun :localleader t)
(oo-bind 'emacs-lisp-mode-map "ee" #'eval-expression :localleader t)
(oo-bind 'emacs-lisp-mode-map "el" #'eval-last-sexp :localleader t)
(oo-bind 'emacs-lisp-mode-map "ep" #'eval-print-last-sexp :localleader t)
;; **** create bindings for adjusting text size
;; :PROPERTIES:
;; :ID:       00f60c97-473b-4f15-b546-ce9ef1488278
;; :END:
(oo-bind :nm "+" #'text-scale-increase)
(oo-bind :nm "-" #'text-scale-decrease)
;; **** use =J= and =K= for scrolling up and down the window
;; :PROPERTIES:
;; :ID:       20230905T182838.549102
;; :END:
;; I use the window scrolling commands pretty often but I never use the original commands in =J= and
;; =K=. The scrolling commands are bound to =C-f= and =C-b=; and on a qwerty keyboard these keys are
;; much harder to press.
(oo-bind :n "J" #'evil-scroll-page-down)
(oo-bind :n "K" #'evil-scroll-page-up)
;; *** miscellaneous
;; :PROPERTIES:
;; :ID:       20230827T100427.253291
;; :END:
;; **** hooks
;; :PROPERTIES:
;; :ID:       20230918T144248.184318
;; :END:
;; I realized that it's better to have all of my hooks in one place.  Hooks are the
;; main way I can decide which packages are "enabled" or not.  For example, I can
;; determine whether =helm= or =vertico= is enabled on startup by changing the
;; hook.  Additionally, it's simply useful to see everything you've added into a
;; hook so you can know at a glance what the hook should contain.
;; ***** text-mode-hook
;; :PROPERTIES:
;; :ID:       20230918T144259.804205
;; :END:
;; ***** prog-mode-hook
;; :PROPERTIES:
;; :ID:       20230918T144251.472987
;; :END:
(oo-add-hook 'prog-mode-hook 'auto-fill-mode)
;; ***** emacs-startup-hook
;; :PROPERTIES:
;; :ID:       20230918T145853.247691
;; :END:
(oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)
;; ***** show me the text cleanly--don't cut a line off with a =\=
;; :PROPERTIES:
;; :ID:       20230906T170532.689085
;; :END:
;; By default Emacs cuts off lines with a =\=.  It's kind of wierd not going to
;; lie.  It's rather have the text just wrap around.
(oo-add-hook 'text-mode-hook #'visual-line-mode)
;; ***** enable auto-filling for text-mode
;; :PROPERTIES:
;; :ID:       20230906T105249.637157
;; :END:
;; This mode automatically appends enter to a line I'm writing if the character I'm writing exceeds
;; =fill-column=. I find it essential for writing.
(oo-add-hook 'text-mode-hook #'auto-fill-mode)
;; ***** load evil
;; :PROPERTIES:
;; :ID:       e472e5d3-2760-420b-bbe1-2eb043368f67
;; :END:
;; As I mentioned, evil bindings should be done after evil is loaded but before
;; =evil-mode= is enabled.  For this reason, I set up =emacs-startup-hook= such that
;; the following things are done in a precise order: loading evil, binding keys,
;; and enabling evil mode.
(oo-add-hook 'after-init-hook #'require :args '(evil) :depth 10)

(oo-add-hook 'after-init-hook #'evil-mode :depth 90)
;; **** setup idle-require
;; :PROPERTIES:
;; :ID:       20230801T152335.293431
;; :END:
(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)
;; **** require packages and autoloads silently
;; :PROPERTIES:
;; :ID:       20230827T191553.875978
;; :END:
(oo-add-advice #'idle-require-load-next :around #'oo-funcall-silently)
;; **** load packages in four second intervals
;; :PROPERTIES:
;; :ID:       20230801T180205.246172
;; :END:
;; Wait 10 seconds before starting to load packages and then at that point load them every 5 idle seconds.
(set! idle-require-load-break 5)
(set! idle-require-idle-delay 10)
;; **** alternate between using four and eight
;; :PROPERTIES:
;; :ID:       20230827T100259.215246
;; :END:
(set! gcmh-high-cons-threshold (* 8 1024 1024))
(set! gcmh-low-cons-threshold (* 4 1024 1024))
;; **** gcmh
;; :PROPERTIES:
;; :ID:       86653a5a-f273-4ce4-b89b-f288d5d46d44
;; :END:
;; =gcmh= does three things.  It reduces garbage collection by setting, it adds a
;; hook telling Emacs to gargbage collect during idle time, and it tells Emacs
;; to garbage collect more frequently when it's idle.

;; Note that I add gcmh mode hook at the end so that all the other hook commands
;; are done before setting the =gc-cons-threshold= back to a normal value.
(set! gcmh-idle-delay 'auto)
;; **** boost garbage collection in minibuffer
;; :PROPERTIES:
;; :ID:       20230801T161328.023311
;; :END:
;; [[helpvar:minibuffer-setup-hook][minibuffer-setup-hook]] and [[helpvar:minibuffer-exit-hook][minibuffer-exit-hook]] are the hooks run just before
;; entering and exiting the minibuffer (respectively).  In the minibuffer I'll be
;; primarily doing searches for variables and functions.  There are alot of
;; variables and functions so this can certainly get computationally expensive.  To
;; keep things snappy I increase boost the [[helpvar:gc-cons-threshold][gc-cons-threshold]] just before I enter
;; the minibuffer, and restore it to it's original value a few seconds after it's closed.

(defhook! boost-garbage-collection (minibuffer-setup-hook)
  "Boost garbage collection settings to `gcmh-high-cons-threshold"
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defhook! defer-garbage-collection (minibuffer-exit-hook :append t)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold gcmh-low-cons-threshold))
;; **** enable smartparens during text-mode
;; :PROPERTIES:
;; :ID:       20230825T113835.071899
;; :END:
(oo-add-hook 'prog-mode-hook #'smartparens-strict-mode)
(oo-add-hook 'prog-mode-hook #'corfu-mode)
;; **** enable lispyville in =prog-mode=
;; :PROPERTIES:
;; :ID:       20230825T115048.040182
;; :END:
(oo-add-hook 'prog-mode-hook #'lispyville-mode)
;; **** don't display a scrollbar in the corfu popup
;; :PROPERTIES:
;; :ID:       20230825T125631.873960
;; :END:
(setq corfu-bar-width 0)
;; **** other detail
;; :PROPERTIES:
;; :ID:       20230825T115139.511854
;; :END:
(set! sp-show-pair-delay 0.2)
;; **** start trying to complete a word after 1 character
;; :PROPERTIES:
;; :ID:       20230901T112429.898067
;; :END:
(set! corfu-auto-prefix 1)
;; **** setup corfu
;; :PROPERTIES:
;; :ID:       8fb7738c-90ed-4517-896d-2880d71bbe52
;; :END:
(set! corfu-quit-at-boundary nil)
(set! corfu-auto t)
(set! corfu-auto-delay 0.1)
;; **** increase the default column fill
;; :PROPERTIES:
;; :ID:       20230825T122317.453428
;; :END:
(setq-default fill-column 80)
;; **** don't highlight the text I'm writing
;; :PROPERTIES:
;; :ID:       20230825T114729.128807
;; :END:
;; By default smartparens, highlights the text within an automatically inserted pair.  I find this
;; behavior jarring (and I'm not the only one).
(set! sp-highlight-wrap-tag-overlay nil)
(set! sp-highlight-pair-overlay nil)
(set! sp-highlight-wrap-overlay nil)
;; **** disable old themes before enabling new ones
;; :PROPERTIES:
;; :ID:       c2110d52-ce27-4f3a-b856-1e31200f597c
;; :END:
;; Sometimes we end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.
(defadvice! disable-old-themes (around load-theme)
  "Disable old themes before loading new ones."
  (:args orig-fn &rest args)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))
;; ** completion
;; :PROPERTIES:
;; :ID:       20230801T200750.688097
;; :END:
;; *** don't preview
;; :PROPERTIES:
;; :ID:       3a0b1ba3-5f06-4357-a5bd-10f6ab419416
;; :RELATED:  [[id:e9c1d67e-137f-4ee0-a736-dd3079675547][consult]]
;; :END:
;; By default =consult= previews buffers before operating on them. For example, =consult-buffer=
;; switches to the buffer you select. The idea is interesting, but I don't want to have this on by
;; default. It is disruptive and can greatly slow down =consult-buffer= because it actually opens each
;; buffer go through. More often than not I don't need the preview anyway.
(set! consult-preview-key nil)
;; *** recentf
;; :PROPERTIES:
;; :ID:       5ab47c35-53bd-460f-ba41-6f3075bd1222
;; :END:
;; =recentf= is a built-in program that tracks the files you've opened recently
;; persistently.  This is a great idea because these are the files you'll likely
;; revisit.  In practice, I look at this list of files in addition to the buffers I
;; already have open using a [[id:f26bedb3-a172-4543-afd0-4c47f5872d15][completion-framework]].  Because of this I rarely
;; have to set out to look for a file with =dired=.
(oo-add-hook 'emacs-startup-hook #'recentf-mode)

(set! recentf-max-menu-items 0)
(set! recentf-max-saved-items 700)
(set! recentf-save-file (concat oo-cache-dir "recentf"))
(set! recentf-auto-cleanup (* 60 10))
(set! recentf-filename-handlers '(file-truename))

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(--each (list #'recentf-mode #'recentf-cleanup #'recentf-save-list)
  (oo-add-advice it :around #'oo-funcall-silently))
;; *** setup marginalia
;; :PROPERTIES:
;; :ID:       79072a17-38a0-4e19-adcf-50326e1e858b
;; :END:
(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
(oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)
;; *** use comma as separator for =completing-read-multiple=
;; :PROPERTIES:
;; :ID:       20230731T162149.740545
;; :END:
;; When using =completing-read-multiple= to select multiple candidates, you need some
;; character that indicates that you've finished the current candidate and intend
;; on selecting a new one.  This is what this is.  I specify here that I want that
;; delineating character to be a comma.
(set! crm-separator ",")
;; *** use any letter from the alphabet for vertico quick
;; :PROPERTIES:
;; :ID:       20231013T110047.067173
;; :END:
;; Set vertico-quick so that I will always have to deal with only the homerow keys.
;; I used to just set [[][vertico-quick1]] to all the letters of the alphabet, but
;; I [[][realized]] this was not the best way.
(set! vertico-quick1 "asdf")
(set! vertico-quick2 "jkl;")
;; *** vertico
;; :PROPERTIES:
;; :ID:       c2209797-2522-464c-9ee5-bf3eae370531
;; :END:
(set! vertico-count-format '("%-6s " . "%2$s"))
(set! vertico-count 15)

(oo-add-hook 'emacs-startup-hook #'vertico-mode)
;; *** treat vertico as a buffer
;; :PROPERTIES:
;; :ID:       20230824T082944.473052
;; :END:
;; By default vertico uses the minibuffer.  With this I use the extension
;; =vertico-buffer= which takes the contents of the minibuffer and puts it in a
;; buffer.  Why would I want to do this? Well, it is nice to.
(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)
;; *** open the vertico buffer on the bottom
;; :PROPERTIES:
;; :ID:       20230825T213210.261609
;; :END:
(oo-popup-at-bottom "\\*Vertico")
;; *** orderless
;; :PROPERTIES:
;; :ID:       4e336184-c7f3-44ae-bc35-b992a71c67b9
;; :END:
(defhook! enable-orderless (vertico-mode-hook :expire t)
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))
    (alet '(orderless-strict-leading-initialism orderless-initialism orderless-regexp)
      (set! orderless-matching-styles it))))
;; *** abbrevs
;; :PROPERTIES:
;; :ID:       20230802T172218.869756
;; :END:
;; The abbrev system for emacs.
;; **** don't save my abbrevs to a file; I'd rather have them as lisp
;; :PROPERTIES:
;; :ID:       20230803T055416.342537
;; :END:
(set! abbrev-file-name null-device)
;; **** capitalize
;; :PROPERTIES:
;; :ID:       20230909T153643.051946
;; :END:
(define-global-abbrev "i" "I")
(define-global-abbrev "luis" "Luis")
;; **** imho
;; :PROPERTIES:
;; :ID:       20230802T192737.209043
;; :END:
(define-global-abbrev "imho" "in my humble opinion")
;; **** imo
;; :PROPERTIES:
;; :ID:       20230802T192722.138540
;; :END:
(define-global-abbrev "imo" "in my opinion")
;; **** fyi
;; :PROPERTIES:
;; :ID:       20230802T192700.148484
;; :END:
(define-global-abbrev "fyi" "for your information")
;; **** lmk
;; :PROPERTIES:
;; :ID:       20230802T171239.604183
;; :END:
(define-global-abbrev "lmk" "let me know")
;; **** afaik
;; :PROPERTIES:
;; :ID:       20230802T163541.823256
;; :END:
(define-global-abbrev "afaik" "as far as I know")
;; **** idk
;; :PROPERTIES:
;; :ID:       20230802T171437.846231
;; :END:
(define-global-abbrev "idk" "I do not know")
;; **** abbreviate =im= to =I'm=
;; :PROPERTIES:
;; :ID:       20230909T153859.786986
;; :END:
;; To press =I'm= on a QWERTY keyboard is too hard.  It's much easier to just type =im=.
(define-global-abbrev "im" "I am")
;; **** tbh
(define-global-abbrev "tbh" "to be honest")
;; **** abbreviate =qwerty= to =QWERTY=
;; :PROPERTIES:
;; :ID:       20230909T153936.924067
;; :END:
;; Haha.  As I was typing my commentary for [[][]] I realized how troublesome it is to type QWERTY and
;; I immediately made an abbrev for it.
(define-global-abbrev "qwerty" "QWERTY")
;; **** spell out all english abbreviation with ='=
;; :PROPERTIES:
;; :ID:       20230909T154712.166953
;; :END:
;; A continuation of my war on ='=.  I started this war with [[][I'm]].
(define-global-abbrev "dont" "do not")
;; **** convert =ive= to =I've=
;; :PROPERTIES:
;; :ID:       20230911T124829.255887
;; :END:
(define-global-abbrev "ive" "I've")
;; **** stop myself from misspelling =surprise= as =surprise=
;; I don't know why but I often tend to mispell =surprise= as =surprise=.  This
;; abbrev will fix this typo for me should I ever make this mistake in the
;; future.
(define-global-abbrev "suprise" "surprise")
;; **** another common misspell is for me to mix up the =i= and =o=
(define-global-abbrev "functoin" "function")
;; **** =gonna= - going to
;; :PROPERTIES:
;; :ID:       20230918T150556.925877
;; :END:
(define-global-abbrev "gonna" "going to")
;; **** =ngl= - not going to lie
;; :PROPERTIES:
;; :ID:       20230918T150535.294257
;; :END:
(define-global-abbrev "ngl" "not going to lie")
;; **** =tbf= - to be frank
;; :PROPERTIES:
;; :ID:       20230918T150442.391086
;; :END:
(define-global-abbrev "tfb" "to be frank")
;; **** more abbrevs
;; :PROPERTIES:
;; :ID:       20231010T112351.552472
;; :END:
(define-global-abbrev "ndo" "window")
(define-global-abbrev "tis" "it is")
(define-global-abbrev "amly" "automatically")
(define-global-abbrev "rn" "right now")
(define-global-abbrev "bly" "probably")
(define-global-abbrev "lk" "like")
;; **** abbreviate =configuration= with =config=
;; :PROPERTIES:
;; :ID:       20231006T144138.256297
;; :END:
;; I need to figure out how to only expand an abbrev based on certain condition
;; because it clashes with some things such as the =~/.config= directory.
(define-global-abbrev "config" "configuration")
;; **** abbreviate =obviously= with =obv=
;; :PROPERTIES:
;; :ID:       20231006T123058.805780
;; :END:
(define-global-abbrev "obv" "obviously")
;; **** abbreviate =appropriate= with =appr=
;; :PROPERTIES:
;; :ID:       20231006T123259.411709
;; :END:
(define-global-abbrev "appr" "appropriate")
;; **** abbreviate =don't= with =dnt=
;; :PROPERTIES:
;; :ID:       20231006T102442.829910
;; :END:
;; Prior to this I had =dont= expand to =don't=.  Why not remove one more vowel?
(define-global-abbrev "dnt" "don't")
;; **** abbreviate =of-course= with =ofc=
;; :PROPERTIES:
;; :ID:       20231006T102026.537852
;; :END:
;; I'll likely end up changing this.  I don't think I want this to be a global
;; abbrev.  Maybe I should make it just for =text-mode=.
(define-global-abbrev "ofc" "of course")
;; **** abbreviate =O.K.= with =ok=
;; :PROPERTIES:
;; :ID:       20231006T132227.798421
;; :END:
;; I would always just write =ok= in text but I think it's proper to actually write
;; out =O.K.=.
(define-global-abbrev "ok" "O.K.")
;; **** abbreviate =it's= with =iis=
;; :PROPERTIES:
;; :ID:       20231008T061714.575772
;; :END:
;; I don't like writing the ='=.  Naturally, I'm inclined to abbreviate =it's= with =its=
;; but its is actually a different word which I use often.  That's why I settled on
;; =iis=.
(define-global-abbrev "iis" "it's")
;; **** abbreviate =you'll= with =ul=
;; :PROPERTIES:
;; :ID:       20231008T161416.323520
;; :END:
;; This is continuation of my war against ='=.
(define-global-abbrev "ul" "you'll")
;; **** replace =bc= with =because=
;; :PROPERTIES:
;; :ID:       20231006T043735.171481
;; :END:
;; This is just a useful abbreviation.  I use the word "because" often.
(define-global-abbrev "bc" "because")
;; **** abbreviate =in other words= with =iow=
;; :PROPERTIES:
;; :ID:       20231007T131818.928017
;; :END:
(define-global-abbrev "iow" "in other words")
;; **** abbreviate =up until now= with =uun=
;; :PROPERTIES:
;; :ID:       20231008T061259.034293
;; :END:
(define-global-abbrev "uun" "up until now")
;; **** abbreviate =EXWM= with =exwm=
;; :PROPERTIES:
;; :ID:       20231009T142712.079778
;; :END:
(define-global-abbrev "exwm" "EXWM")
;; **** abbreviate "for example" with =fe=
;; :PROPERTIES:
;; :ID:       20231007T110531.380588
;; :END:
(define-global-abbrev "fe" "for example")
;; **** abbreviate =evaluated= with =evaled=
;; :PROPERTIES:
;; :ID:       20231006T120812.742130
;; :END:
(define-global-abbrev "evaled" "evaluated")
;; **** abbreviate =I'd= with =id=
;; :PROPERTIES:
;; :ID:       20231008T055429.648521
;; :END:
;; +I think =id= is [[https://www.verywellmind.com/what-is-the-id-2795275][actually a word]] but I never use it; however, I use =I'd= all the+
;; +time.  It's a worthwhile trade-off then to abbreviate =I'd= as =I'd=.+
(define-global-abbrev "idd" "I would")
;; **** abbreviate =that is= with =thats=
;; :PROPERTIES:
;; :ID:       20231014T193558.633337
;; :END:
(define-global-abbrev "thats" "that is")
;; **** abbreviate =I will= with =illl=
;; :PROPERTIES:
;; :ID:       20231014T195733.115935
;; :END:
(define-global-abbrev "illl" "I will")
;; **** abbreviate =doc-string= with =doc-string= and =dstr=
;; :PROPERTIES:
;; :ID:       20231015T125840.552335
;; :END:
(define-global-abbrev "docstring" "doc-string")
(define-global-abbrev "docstrings" "doc-strings")
(define-global-abbrev "dstr" "doc-string")
(define-global-abbrev "dstrs" "doc-strings")
;; **** abbreviate =I know= with =ik=
;; :PROPERTIES:
;; :ID:       20231015T151329.190932
;; :END:
(define-global-abbrev "ik" "I know")
;; **** abbreviate =describe= with =ribe=
;; :PROPERTIES:
;; :ID:       20231015T151617.394097
;; :END:
(define-global-abbrev "ribe" "describe")
;; **** abbreviate "if and only if" with =iff=
;; :PROPERTIES:
;; :ID:       20231021T154406.306354
;; :END:
;; I want to be precise when it comes to.
;; #+begin_src elisp
;; (define-global-abbrev "iff" "if and only if")
;; #+end_src
;; *** choose corfu candidates with avy
;; :PROPERTIES:
;; :ID:       20230906T201423.268025
;; :END:
(oo-bind 'corfu-map "<tab>" #'corfu-next)
(oo-bind 'corfu-map [backtab] #'corfu-previous)
(oo-bind 'corfu-map "C-;" #'corfu-quick-complete)
(oo-bind 'corfu-map "C-j" #'corfu-next)
;; Works
(oo-bind 'corfu-map "M-k" #'corfu-previous)
;; For some reason I can't bind these.
(oo-bind 'corfu-map :ieg "C-k" #'corfu-previous)
(oo-bind 'corfu-map :ieg "C-p" #'corfu-previous)
;; *** give me a wider range of quick keys
;; :PROPERTIES:
;; :ID:       20230906T202456.005093
;; :END:
;; By default =corfu= only comes with.
(set! corfu-quick1 "abcdefghijklmnopqrstuvxyz")
;; *** prefer recently used corfu candidates
;; :PROPERTIES:
;; :ID:       20230906T201310.034887
;; :END:
;; The package =corfu-history= puts items that.
(oo-add-hook 'corfu-mode #'corfu-history-mode)
;; *** enable snippet expansion
;; :PROPERTIES:
;; :ID:       20230830T081839.288591
;; :HEADER-ARGS: :tangle no
;; :END:
;; In this section I will show how I setup snippets.  With how much text is written
;; and how much repetitiveness exists in programming languages--and I think lisp is
;; one of the least repetitive because of macros--snippets are essential for
;; productive programming.  By "snippets" I mean particular text that you can insert.
;; **** abbreaviate =does not= with =doesnt=
;; :PROPERTIES:
;; :ID:       20231015T084901.180770
;; :END:
;; Man, `completion-at-point-functions' is such a long variable name huh?
;; I definitely do not recommend writing all that out yourself.
(define-global-abbrev "capfs" "completion-at-point-functions")
;; **** deleting!
;; :PROPERTIES:
;; :ID:       20231015T092629.296737
;; :END:
(cl-defmacro removing! (place item &key test test-not key (setter 'setf))
  "Set PLACE to the value of `cl-remove'.
SETTER is the same as in `appending!'."
  `(,setter ,place (cl-remove ,item ,place :test ,test :test-not ,test-not :key ,key)))
;; **** don't insert candidates on exact match
;; :PROPERTIES:
;; :ID:       20230901T122628.447860
;; :END:
;; By default =corfu= inserts a candidate on the exact match.  I don't want it to do this because there
;; might be something I want to do instead of inserting.  I'd rather insert
;; manually via the =return= key.
(set! corfu-on-exact-match nil)
;; **** define a minor mode for enabling snippet completion
;; :PROPERTIES:
;; :ID:       20231015T105510.482138
;; :END:
;; See the [[][manual]] on [[][defining a minor mode]].  To be honest, it was
;; [[][systemcrafters]] that gave me an example I could understand.
(define-minor-mode oo-snippet-mode
  "Mode to enable snippets for completion."
  nil
  :global nil
  (cond (oo-snippet-mode
         (adjoining! tempel-template-sources #'oo-active-snippets :setter setq-local)
         (adjoining! completion-at-point-functions #'tempel-complete :setter setq-local))
        (t
         (removing! tempel-template-sources #'oo-active-snippets :setter setq-local)
         (removing! completion-at-point-functions #'tempel-complete :setter setq-local))))

;; **** add tempel to =completion-at-point-functions=
;; :PROPERTIES:
;; :ID:       20230830T182833.937689
;; :END:
;; =Tempel= provides two alternative functions you can add to for a complete-at-point
;; function: [[tempel-expand]] and tempel-complete.  In the docs =tempel= uses
;; =tempel-expand=. However, as I found out the function =tempel-expand= triggers only
;; on exact matches.
(oo-add-hook 'prog-mode-hook 'oo-snippet-mode)
(oo-add-hook 'text-mode-hook 'oo-snippet-mode)
;; **** add bindings for traversing template placeholders
;; :PROPERTIES:
;; :ID:       20230901T074135.947015
;; :END:
;; I want to make sure I can traverse template placeholders in the same way as I do
;; everything else.  The =backtab= key (invoked with =Shift= =Tab=) is hard to press
;; quickly which is why I add an alternative: =C-k=.  And, for completeness I include =C-j=.
(oo-bind 'tempel-map :ie "C-j" #'tempel-next)
(oo-bind 'tempel-map :ie "C-k" #'tempel-previous)
(oo-bind 'tempel-map :ie "TAB" #'tempel-next)
(oo-bind 'tempel-map :ie [backtab] #'tempel-previous)
;; **** create an alist of snippets
;; :PROPERTIES:
;; :ID:       20231014T193736.749971
;; :END:
;; This is where I will store my modal snippets.
(defvar oo-snippet-alist nil
  "An alist of (MODES . SNIPPET).
MODE-OR-MODES is either a mode symbol or a list of modes when snippet should be
enabled.  SNIPPET is a valid `tempel' snippet.")
;; **** add to the global value of [[][completion-at-point-functions]]?
;; :PROPERTIES:
;; :ID:       20231015T083655.890255
;; :END:
;; At first I was creating a hook for each pertinent mode and adding
;; [[][tempel-complete]] to the local variable value of
;; completion-at-point-functions= but why not do it globally?  I'm tempted to do
;; that because its much simpler than doing it individually via local variables.
;; According to the [[][documentation]] it should contain functions that are
;; efficient.  After reading that I had second thoughts I do not want to slow down
;; emacs by having it resolve for templates in modes do not use them.  I think I'm
;; in need of a mode to add to a hook like =snippet-mode=.  =Tempel= does not provide a
;; =tempel-mode= (maybe it should?).  Then again, maybe a mode is overkill if all I
;; am using it for is to add and remove from a =completion-at-point-functions=.  I
;; don't know but still maybe I should just make it a minor mode because it has the
;; qualities of being a mode.
;; **** snippets
;; :PROPERTIES:
;; :ID:       20230920T152708.904400
;; :END:
;; These are snippets I have using [[][tempel]].
;; ***** org-link-snippet
;; :PROPERTIES:
;; :ID:       20230920T152720.914621
;; :END:
(adjoining! oo-snippet-alist (cons 'org-mode '(shot "[[" p "]" "[" p "]]")))
;; **** oo-minor-modes
;; :PROPERTIES:
;; :ID:       20231015T123532.079136
;; :END:
;; This function was inspired and adapted from [[][this question]] on how to see
;; the list of enabled minor modes.

;; Note that you need the =ignore-errors= because =symbol-value= will raise an error if
;; the symbol is checking is not bound.  In other words, there is a distinction
;; between a symbol that is bound to nil and one that is not bound at all.
(defun! oo-minor-modes ()
  "Return a list of enabled minor modes."
  (interactive)
  (dolist (mode minor-mode-list)
    (when (and (symbolp mode) (ignore-errors (symbol-value mode)))
      (adjoining! active-modes mode)))
  active-modes)

(defun oo-modes ()
  "Return the a list of all modes enabled.
The CAR of the list is the major mode.  The CDR is a list of minor modes."
  (cons major-mode (oo-minor-modes)))
;; **** open the =*Backtrace*= buffer at the bottom
;; :PROPERTIES:
;; :ID:       20231015T142118.024087
;; :END:
(oo-popup-at-bottom "*Backtrace")
;; **** don't expand elisp snippets in the docstring
;; :PROPERTIES:
;; :ID:       20231015T125130.496472
;; :END:
;; After having the snippets from [[][tempel-collection]] enabled, and writing the
;; doc-string for an elisp function I was being prompted to complete elisp
;; snippets.  Obviously, I'm not going to use elisp snippets in a doc-string.
;; Instead, it could prompt me for text mode snippets.  This is also why I want
;; context-aware abbrevs as well: my text-mode abbrevs would be useful in elisp
;; doc-strings.
;; **** define an alist that populates mode variables
;; :PROPERTIES:
;; :ID:       20230920T115104.895695
;; :END:
;; The idea is we have an alist of the modes a snippet should be enabled in and the
;; snippet itself.  The function =oo-active-snippets= returns all the snippets in
;; [[][oo-snippets-alist]] whose modes are active.  I place this function in =tempel-template-sources=.
(defun! oo-active-snippets ()
  "Return the list of snippets that should be enabled.
Specifically, return any snippets whose modes match any of the current modes."
  nil
  ;; (for! ((modes . snippet) oo-snippet-alist)
  ;;   (when (-difference (cons major-mode (oo-minor-modes)))
  ;;     (adjoining! snippets snippet :test #'equal :key #'car)))
  ;; snippets
  )
;; **** automate enabling snippets for a certain mode
;; :PROPERTIES:
;; :ID:       20230830T183446.161023
;; :HEADER-ARGS: :tangle no
;; :END:
;; Defining snippets for particular modes always amounts to the same thing.  I
;; define a mode-specific variable for storing the snippets for that mode if it
;; does not exist.  Then I add the snippet to that mode variable.  Finally, I
;; create a hook that appends the stored snippets to the local variable
;; =oo-local-templates= (if it does not exist already).  I automate this process with
;; the =defsnippet!= macro.
(defun oo-add-snippet (key mode-or-modes snippet)
  "Register SNIPPET to be trigged by KEY when any MODE-OR-MODES are enabled."
  (pushing! oo-snippet-alist (cons mode-or-modes (cons key snippet))))
;; ** appearance
;; :PROPERTIES:
;; :ID:       20230822T164825.678238
;; :END:
;; *** dashboard
;; :PROPERTIES:
;; :ID:       20230824T100150.191845
;; :END:
;; **** display the startup time in the dashboard
;; :PROPERTIES:
;; :ID:       20230822T092717.304272
;; :END:
;; Often I will end up calling =emacs-init-time= myself because I want to see how
;; much time my config took to startup.
(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))
(set! dashboard-init-info #'oo-dashboard-init-info)
;; **** dashboard
;; :PROPERTIES:
;; :ID:       20230822T085232.556049
;; :END:
(set! dashboard-banner-logo-title "Welcome!")
(set! dashboard-set-footer nil)
(set! dashboard-items nil)
(set! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))
(set! dashboard-center-content t)
;; **** initialize the dashboard screen as the initial buffer
;; :PROPERTIES:
;; :ID:       73d00f99-4b70-44d1-8359-01bd2c94b330
;; :END:
(defhook! create-dashboard (oo-initial-buffer-choice-hook)
  (when (require 'dashboard nil t)
    (aprog1 (get-buffer-create dashboard-buffer-name)
      (with-current-buffer it
        (dashboard-insert-startupify-lists)))))
;; *** preserve window divider face
;; :PROPERTIES:
;; :ID:       20230827T190126.412472
;; :END:
;; I set =window-divider-face= the first time when we boot Emacs. Then we set it again just after a
;; theme is loaded. Finally, I want to make sure the face is set to black before the window divider is
;; actually rendered which is why I specify the depths of both hooks.
(defun oo-set-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

(oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)
(oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)

(oo-add-advice #'load-theme :after #'oo-set-window-divider-face)
;; *** place window dividers on bottom and sides of each window
;; :PROPERTIES:
;; :ID:       20230815T203000.102130
;; :END:
;; You can either use =right-only= to place window dividers on the right of each
;; window.  Or =bottom-only= to place them just on the bottom.
(set! window-divider-default-places t)
;; *** load my preferred theme, =modus-operandi=
;; :PROPERTIES:
;; :ID:       20230731T180227.888840
;; :END:
(oo-add-hook 'after-init-hook #'load-theme :args '(modus-operandi))
;; *** add font lock for defun-like macros
;; :PROPERTIES:
;; :ID:       20230807T135107.795873
;; :END:
;; This lets =defun!= and =defmacro!= display with the standard syntax.
(defun! emacs-lisp-mode-hook&add-defun-like-font-lock ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-mode-hook&add-defun-like-font-lock)
;; *** don't use italics so much
;; :PROPERTIES:
;; :ID:       20230808T101112.321414
;; :END:
(set! lambda-themes-set-italic-comments nil)
(set! lambda-themes-set-italic-keywords nil)
;; *** don't use variable pitch
;; :PROPERTIES:
;; :ID:       20230808T101205.749373
;; :END:
(set! lambda-themes-set-variable-pitch nil)
;; *** make =modus-theme= headline text thin and large (like =minimal-theme=)
;; :PROPERTIES:
;; :ID:       20230802T064521.798481
;; :END:
;; For the customizations of headings in =modus-themes=.
(set! modus-themes-headings '((t . (light 1.0))))
;; ** applications
;; :PROPERTIES:
;; :ID:       20230826T194944.139088
;; :END:
;; *** mu4e
;; :PROPERTIES:
;; :ID:       20230826T201603.966234
;; :END:
;; **** mu4e-drafts
;; :PROPERTIES:
;; :ID:       5de3eca7-968b-4228-a1d9-ca872f18f58b
;; :END:
(set! mu4e-compose-signature-auto-include t)
(set! mu4e-compose-format-flowed t)
;; **** automatically update headers if an index operation shows changes
;; :PROPERTIES:
;; :ID:       2f0a3a6b-1f68-41f7-b001-c0aadf1de8de
;; :END:
(set! mu4e-headers-auto-update t)
;; *** dirvish
;; :PROPERTIES:
;; :ID:       20230826T185300.703070
;; :END:
;; **** use =h= for up directory
;; :PROPERTIES:
;; :ID:       20230826T192130.887256
;; :END:
(oo-bind 'dired-mode-map :nm "h" #'dired-up-directory)
;; **** don't show =..= and =.=
;; :PROPERTIES:
;; :ID:       20230826T190737.064673
;; :END:
(oo-add-hook 'dired-mode-hook #'dired-omit-mode)
;; **** always delete and copy recursively
;; :PROPERTIES:
;; :ID:       20230826T191729.964267
;; :END:
;; If you don't set [[helpvar:dired-recursive-deletes][dired-recursive-deletes]], emacs will prompt you every time you
;; try to delete a directory asking you whether you want to recursively delete its
;; contents.  If you're actually permenently deleting it this is a good idea to
;; prevent accidental deletion, but I set [[helpvar:delete-by-moving-to-trash][delete-by-moving-to-trash]] to =t=.  So
;; worst case I'll accidentally move a directory to the trash folder.
(set! dired-recursive-copies 'always)
(set! dired-recursive-deletes 'always)
;; **** use dirvish instead of dired
;; :PROPERTIES:
;; :ID:       20230826T190441.285748
;; :END:
(oo-call-after-load 'dirvish #'dirvish-override-dired-mode 1)
;; **** don't show the modeline
;; :PROPERTIES:
;; :ID:       20230826T190625.068604
;; :END:
(set! dirvish-use-mode-line nil)
;; **** set dirivsh attributes
;; :PROPERTIES:
;; :ID:       20230826T190023.059429
;; :END:
(set! dirvish-attributes '(file-size all-the-icons subtree-state))
;; **** invoke dired in the app map
;; :PROPERTIES:
;; :ID:       20230826T185646.361365
;; :END:
(oo-bind 'oo-app-map "d" #'dired)
;; **** invoke dirvish instead of dired
;; :PROPERTIES:
;; :ID:       20230826T185555.814219
;; :END:
(oo-bind :alt #'dired #'dirvish)
;; **** don't use the default layout
;; :PROPERTIES:
;; :ID:       20230826T185507.733989
;; :END:
(set! dirvish-default-layout nil)
;; *** eshell
;; :PROPERTIES:
;; :ID:       20230826T040106.144359
;; :END:
;; **** load eshell libraries during idle time
;; :PROPERTIES:
;; :ID:       20230827T134725.627088
;; :END:
;; These are features that are loaded for use in eshell.  They create a slight pause when opening
;; eshell.  I'm loading these features pre-emptively so during idle time so that eshell will be even
;; snappier.
(set! idle-require-symbols (append '(em-alias em-banner em-basic em-cmpl em-glob em-hist em-ls em-prompt em-term em-unix)
                                   idle-require-symbols))
;; **** open eshell at bottom
;; :PROPERTIES:
;; :ID:       20230826T040110.191113
;; :END:
(oo-popup-at-bottom "\\*eshell")
;; **** specify directories
;; :PROPERTIES:
;; :ID:       20230826T040324.734683
;; :END:
(set! eshell-directory-name (concat oo-cache-dir "eshell/"))
(set! eshell-history-file-name (concat eshell-directory-name "history"))
;; **** don't display a banner message
;; :PROPERTIES:
;; :ID:       20230826T040343.022042
;; :END:
(set! eshell-banner-message "")
;; **** don't duplicate history items
;; :PROPERTIES:
;; :ID:       20230826T040418.854381
;; :END:
(set! eshell-hist-ignoredups t)
;; *** emms
;; :PROPERTIES:
;; :ID:       20230519T170037.652577
;; :END:
(set! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(set! emms-directory (expand-file-name "emms/" oo-cache-dir))

(oo-call-after-load 'emms #'require 'emms-player-mpv)
(set! emms-player-list '(emms-player-mpv))
;; *** open =*help= buffers at the bottom
;; :PROPERTIES:
;; :ID:       20230731T162249.412629
;; :END:
(oo-popup-at-bottom "\\*[Hh]elp")
;; ** org
;; :PROPERTIES:
;; :ID:       20230801T194833.489566
;; :END:
;; *** source blocks
;; **** hide header-line shown on source blocks
;; :PROPERTIES:
;; :ID:       20230731T162238.682555
;; :END:
;; By default source blocks are displayed with a header line that shows the
;; keybindings for entering and exiting a source block.  Maybe this is useful at
;; first, but it quickly becomes redundant--and it consumes a line of space.  This
;; disables it.
(set! org-edit-src-persistent-message nil)
;; **** add "emacs-lisp" to known languages
;; :PROPERTIES:
;; :ID:       05fb0490-a407-4ae7-bd0f-3e1e9ad7b209
;; :END:
;; Suprisingly, =emacs-lisp= is not in =org-src-lang-modes=; instead =elisp= is what's registered.
(adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))
;; **** add "lua" to known languages
;; :PROPERTIES:
;; :ID:       20230919T200900.753689
;; :END:
;; When I create a source block =org-src-lang-modes= is where I get the languages
;; to prompt me from.
(adjoin! org-src-lang-modes '("lua" . lua))
;; **** edit paragraphs below a headline similar to source block
;; :PROPERTIES:
;; :ID:       20230826T084951.436354
;; :END:
;; The idea is to edit descriptions under headlines like a source block.  To me these descriptions
;; should be edited in text mode.  Trying to use =org-mode= as text mode--even though org-mode derives
;; from text mode--is just too much.
;; ***** add a newline to the modified paragraph contents if needed
;; :PROPERTIES:
;; :ID:       20230829T123808.647098
;; :END:
;; If you don't have a description and you create one the description will not have the newline
;; at the end.  The description should always end in a newline.  Right before committing I check to see
;; if there's a newline and if not I add it.
(defun edit-indirect-before-commit-hook&insert-newline-maybe (&rest _)
  "Add a newline to edit-indirect buffers if they don't have one."
  (alet (buffer-string)
    (unless (or (string-empty-p it) (string-match-p (rx (1+ anything) "\n" eos) it))
      (insert (prog1 (concat (buffer-string) "\n") (erase-buffer))))))
;; ***** create a function that narrows to the heading--not the subtree
;; :PROPERTIES:
;; :ID:       20230829T123253.751293
;; :END:
;; Most of the time.
(defun oo-narrow-to-heading ()
  "Narrow region to the heading at point.
By heading I mean the heading contents but not its subtree."
  (save-excursion
    (org-back-to-heading t)
    (let! narrow-beg (point))
    (let! narrow-end (save-excursion
                       (or (outline-next-heading) (end-of-buffer))
                       (point))))
  (narrow-to-region narrow-beg narrow-end))
;; ***** make a command to edit paragraphs
;; :PROPERTIES:
;; :ID:       20230826T101458.754392
;; :END:
(defun! oo-dwim-edit-paragraph ()
  "Edit consecutive paragraphs after a headline."
  (interactive)
  (let! edit-indirect-guess-mode-function (lambda (&rest _) (org-mode)
                                            (when (bound-and-true-p evil-mode)
                                              (evil-insert-state 1))))
  (let! (beg end) (oo-get-paragraph-bounds))
  (let! edit-indirect-after-creation-hook edit-indirect-after-creation-hook)
  (alet (lambda () (add-hook 'edit-indirect-before-commit-hook #'edit-indirect-before-commit-hook&insert-newline-maybe 100 t))
    (add-hook 'edit-indirect-after-creation-hook it))
  (edit-indirect-region beg end t))
;; ***** make bindings in =edit-indirect= similar to =org-src=
;; :PROPERTIES:
;; :ID:       6f50fb7f-12e4-4273-b283-7c8736c8f027
;; :END:
(oo-bind 'edit-indirect-mode-map "," #'edit-indirect-commit :localleader t)
(oo-bind 'edit-indirect-mode-map "c" #'edit-indirect-commit :localleader t)
(oo-bind 'edit-indirect-mode-map "a" #'edit-indirect-abort :localleader t)
(oo-bind 'edit-indirect-mode-map "s" #'edit-indirect-save :localleader t)

(oo-bind 'edit-indirect-mode-map [remap save-buffer] #'edit-indirect-save)
;; ***** create a hook to guess the mode of a buffer
;; :PROPERTIES:
;; :ID:       20230826T101506.003225
;; :END:
;; Each source block has to be in its correct mode.  This function will be called in
;; the edit-buffer (after I rename it) and it will call the appropriate edit
;; function depending on the buffer type.
(set! edit-indirect-guess-mode-function #'oo-edit-indirect-run-guess-mode-hook)

(defvar oo-edit-indirect-guess-mode-hook nil
  "Hooks run until a hook returns a function.
The function returned is called with no arguments and should set the mode of the
edit-indirect buffer.")

(defun! oo-edit-indirect-run-guess-mode-hook (parent-buffer beg end)
  (with-current-buffer parent-buffer
    (let! mode-fn (run-hook-with-args-until-success 'oo-edit-indirect-guess-mode-hook beg end)))
  (funcall (or mode-fn #'fundamental-mode)))
;; ***** make a minor mode for editing paragraphs
;; :PROPERTIES:
;; :ID:       20230919T092654.685368
;; :END:
;; A minor mode would make editing org paragraphs a "thing".  I want it to be a
;; "thing" because there are certain things I want to enable specially for.
;; ***** ensure that =edit-indirect= blocks open at the bottom
;; :PROPERTIES:
;; :ID:       026f9aa2-cf80-4b2e-9107-55b2c115014c
;; :END:
(oo-popup-at-bottom "\\*edit-indirect[^z-a]+")
;; ***** oo-edit-indirect-guess-mode-from-src-block
;; :PROPERTIES:
;; :ID:       251a6a01-0d97-4454-af77-33060e4d5633
;; :END:
;; This is what's reponsible for actually setting the proper mode of a source
;; block.
(defun! oo-edit-indirect-guess-mode-from-src-block (beg _)
  "Call the mode the edit-indirect buffer should be in."
  (save-excursion
    (goto-char beg)
    (and (let! element (org-element-at-point))
         (equal 'src-block (org-element-type element))
         (let! lang (org-element-property :language element))
         (let! mode-fn (org-src-get-lang-mode lang))
         mode-fn)))

(oo-add-hook 'oo-edit-indirect-guess-mode-hook #'oo-edit-indirect-guess-mode-from-src-block)
;; **** don't confirm evaluation of source block
;; :PROPERTIES:
;; :ID:       20230825T105449.754147
;; :END:
;; By default org prompts you to ask if you're sure you want to evaluate a source block.  For some reason as well it narrows.
(set! org-confirm-babel-evaluate nil)
;; **** open src-code buffers with popup rules
;; :PROPERTIES:
;; :ID:       20230731T162312.439081
;; :END:
;; Let =display-buffer= be in charge of how.  I want to use =display-buffer= for everything.
(set! org-src-window-setup 'plain)
;; **** org-mode indents the contents of a source block
;; :PROPERTIES:
;; :ID:       20230731T162317.317979
;; :END:
;; You'll find that =org-mode= automatically indents the content of a source block.  Once I set.
(set! org-src-preserve-indentation t)
(set! org-edit-src-content-indentation 0)
;; **** replace source block boilerplate with pretty symbols
;; :PROPERTIES:
;; :ID:       20231201T170708.613516
;; :END:
;; Use a pretty symbol to replace the ugly =#+begin_src= and =#+end_src= lines.
;; #+begin_src elisp
;; (appending! prettify-symbols-alist
;;             '(("#+BEGIN_SRC" . "†")
;;               ("#+END_SRC" . "†")
;;               ("#+begin_src" . "†")
;;               ("#+end_src" . "†"))
;;             :setter setq-local)
;; #+end_src
;; **** default header args
;; :PROPERTIES:
;; :ID:       20230801T194653.865875
;; :END:
(set! org-babel-default-header-args
      '((:session . "none")
        (:results . "silent")
        (:exports . "code")
        (:mkdirp  . "yes")
        (:cache   .  "no")
        (:noweb   .  "no")
        (:hlines  .  "no")
        (:tangle  .  "no")))
;; **** open =*org-src= buffers at the bottom
;; :PROPERTIES:
;; :ID:       20230731T162243.509972
;; :END:
;; This in combination with [[][]] guarantees that source blocks will open at the
;; bottom.
(oo-popup-at-bottom "\\*Org Src")
;; **** don't ask; just return to edit buffer
;; :PROPERTIES:
;; :ID:       20230731T162254.904167
;; :END:
;; When a source block currently has an existing edit buffer, org will ask you if
;; you want to return to it.  I always do.  I'm not sure why I ever
;; wouldn't.  I'm not clear what the alternative is--perhaps creating a brand new
;; edit buffer and discarding the old one?
(set! org-src-ask-before-returning-to-edit-buffer nil)
;; **** oo-dwim-edit-src-code
;; :PROPERTIES:
;; :ID:       cb508c25-99ec-48de-bf84-93f68940e7c5
;; :END:
;; One notable addition is the convenience of having the command jump to the next
;; source block for me.  This is a no-brainer "do what I mean" type thing for me: if
;; I invoke this command and I'm not on a source block I mean go to the first
;; source block of this section.
(defun! oo-dwim-edit-src-code ()
  "Edit nearest source block."
  (interactive)
  (mapc #'require '(edit-indirect org-ml))
  (unless (org-in-src-block-p) (org-next-block 1))
  (let! (beg end) (org-src--contents-area (org-ml-parse-this-element)))
  (let! parent-buffer (current-buffer))
  (edit-indirect-region beg end t))
;; **** oo-org-dwim-eval-src-block
;; :PROPERTIES:
;; :ID:       1a7d5dde-ab85-4159-b3c7-9da093a30de0
;; :END:
(defun oo-dwim-eval-src-block ()
  "Eval block contents."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not in source block"))
  (save-window-excursion
    (org-babel-execute-subtree)))
;; **** oo-heading-has-source-block-p
;; :PROPERTIES:
;; :ID:       32216520-084d-4f42-8713-01765773aa99
;; :END:
(defun! oo-has-src-block-p ()
  "Return non-nil if current headline has a source block."
  (save-excursion
    (let! beg (point))
    (let! end (or (outline-next-heading) (point-max)))
    (goto-char beg)
    (and (save-match-data (re-search-forward "^#\\+begin_src" end t)) t)))
;; **** insert a source block
;; :PROPERTIES:
;; :ID:       20230427T062204.669000
;; :END:
(defun! oo-dwim-insert-src-block ()
  "Insert source block for the current headline if it does not already exist."
  (interactive)
  (let! lang (completing-read "Language: " (mapcar 'car org-src-lang-modes)))
  (let! headline (org-ml-parse-this-headline))
  (let! section (org-ml-headline-get-section headline))
  (when (--any-p (equal 'src-block (org-element-type it)) section) (return!))
  (let! src-block (org-ml-build-src-block :value "" :language lang))
  (snocing! (org-ml-headline-get-section headline) src-block)
  (org-ml-update-this-headline (-const headline)))
;; **** adding tangle header arguments painlessly
;; :PROPERTIES:
;; :ID:       20230801T080452.422830
;; :END:
;; I had a function which added the header arg, but it is more useful for me to
;; toggle it.  This way I can easily and freely choose which headlines should be
;; tangled and which ones shouldn't.

;; There is a built-in function that does close to what I want:
;; [[file:snapshots/helpful-function-org-babel-insert-header-arg.png][org-babel-insert-header-arg]].  It prompts for a header argument to insert and then
;; suggests common values.  For the =tangle= header argument for example it suggests
;; =yes=, =no= and =:any=.  I want to get suggestions based on the header arguments I've
;; already used (as it's likely I'll use similar values again).
(defun! oo-add-tangle-header-arg ()
  "Add header arguments to current headline."
  (interactive)
  ;; Find tangle header arguments from other source blocks.
  (let! files (directory-files org-directory t "\\.org\\'"))
  (let! regexp ":tangle \\([^[:space:]]+\\)")
  (flet! tangle-file ()
    (awhen (org-entry-get (point) "HEADER-ARGS")
      (-second-item (s-match regexp (or it "")))))
  (let! header-args (-non-nil (-uniq (org-map-entries #'tangle-file nil files))))
  (let! choosen (completing-read "files: " header-args))
  ;; Now replace based on what was selected.
  (let! header-args (org-entry-get (point) "HEADER-ARGS" ))
  (org-entry-put (point) "HEADER-ARGS"))
;; **** disable tangling a source block
;; :PROPERTIES:
;; :ID:       20230805T053305.879537
;; :END:
(defun oo-disable-tangling ()
  "Disable tangling source block at point."
  (interactive)
  (org-entry-put (point) "HEADER-ARGS" ":tangle no"))
;; **** auto tangle files with oo-auto-tangle-mode enabled
;; :PROPERTIES:
;; :ID:       20230827T100959.938858
;; :END:
;; By default the package =org-auto-tangle= only auto tangles org files that have.  I don't want.
(set! org-auto-tangle-default t)
;; *** tags
;; **** specify where tags column should go
;; :PROPERTIES:
;; :ID:       20230801T134551.817843
;; :END:
;; I want things to be as simple as possible.  And, in any case, I plan "rice" the
;; appearance of tags.  So I don't care greatly about where tags are indented.  The
;; value of =0= places tags just after the newline.
(set! org-tags-column 80)
;; **** remove tags from buffer
;; :PROPERTIES:
;; :ID:       20231203T063813.170326
;; :END:
;; #+begin_src elisp
;; (defun oo-remove-all-tags ()
;;   (interactive)
;;   (org-map-entries (-partial #'org-set-tags nil) nil nil))
;; #+end_src
;; **** make a better set tags command
;; :PROPERTIES:
;; :ID:       20230801T155618.010216
;; :END:
;;  The stock org command for setting tags is [[][org-set-tags-command]].
(defun! oo-update-tags ()
  "Update tags for the current entry."
  (interactive)
  (let->>! all
    (org-map-entries #'org-get-tags nil (oo-directory-files))
    (apply #'append)
    (-non-nil)
    (-uniq))
  (let! old (org-get-tags))
  (let! new (completing-read-multiple "Tags: " all))
  (let! updated (-uniq (append (-difference old new) (-difference new old))))
  (org-set-tags updated))
;; *** markup
;; **** enable =org-appear-mode= in org-mode
;; :PROPERTIES:
;; :ID:       20231002T095204.956570
;; :END:
;; The mode =org-appear-mode= toggles the display of the emphasis characters.
(oo-add-hook 'org-mode-hook #'org-appear-mode)
;; **** hide emphasis markers
;; :PROPERTIES:
;; :ID:       20230731T162307.681422
;; :END:
;; By default org displays markers used for emphasis; but to be honest it looks
;; cooler when they're hidden.
(set! org-hide-emphasis-markers t)
;; **** fontify emphasis markers
;; :PROPERTIES:
;; :ID:       20230731T162303.666860
;; :END:
;; This variable fontifies emphasized text.  So, text with the.
(set! org-fontify-emphasized-text t)
;; **** enable the toggling of links
;; :PROPERTIES:
;; :ID:       20231002T101734.633030
;; :END:
;; Links are similar to emphasis markers in that it is easier to edit their raw
;; link text but when you're not editing them it's better to look at their
;; abbreviated form. This links behave the same way as emphasis markers by default:
;; as in, when my cursor is on a link--meaning that I likely want to edit
;; it--display the raw link text.
(set! org-appear-autolinks t)

;; *** archiving
;; **** don't add any information to the headline
;; :PROPERTIES:
;; :ID:       20230801T194512.542617
;; :END:
(set! org-archive-save-context-info nil)
;; **** specify archive location
;; :PROPERTIES:
;; :ID:       20230801T194333.393056
;; :END:
;; This is the file where archived headings should go to.
(set! org-archive-location (concat org-directory "archive.org::"))
;; *** refiling
;; **** create a heading if necessary
;; :PROPERTIES:
;; :ID:       20230801T134417.318262
;; :END:
(set! org-refile-allow-creating-parent-nodes t)
;; **** set the refile targets to all my org files
;; :PROPERTIES:
;; :ID:       20230801T134421.975320
;; :END:
;; The variable =org-refile-targets= specifies the places from which information is
;; taken to create the list of possible refile targets.  So, for example,
(set! org-refile-targets '((oo-directory-files :maxlevel . 10)))
;; **** don't complete in steps; show me all the information at once
;; :PROPERTIES:
;; :ID:       20230825T130940.525933
;; :END:
;; [[https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html][Aaron Bieber's post]] explains this pretty well.  Since we're using a completion framework--in my
;; case, primarily =corfu= we don't need to complete in steps.  It's more expedient to just select from
;; all the candidates.
(setq org-outline-path-complete-in-steps nil)
;; **** don't use a cache for refiling
(set! org-refile-use-cache nil)
;; **** allow files as targets for refiling
;; :PROPERTIES:
;; :ID:       d85fd068-97bc-4291-99c9-30f716697ba5
;; :END:
;; Without this setting, you can't actually refile to a generic file with refiling;
;; you can only refile to existing headings within that file.  The way I use
;; refiling, I'm refiling to files most of the time.
(set! org-refile-use-outline-path 'file)
;; **** when refiling, only consider headlines without a source block
;; :PROPERTIES:
;; :ID:       20230825T145959.961216
;; :END:
;; I will never let a headline with a source block be the parent of another headline.  It is simply not my style.
(set! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;; *** headlines
;; **** enter insert state after inserting a headline
;; :PROPERTIES:
;; :ID:       20230801T175939.021467
;; :END:
;; Predominately, the first thing I want to do after creating a headline is to
;; start writing the title.  For that it's more convenient to start out in insert
;; state.
(oo-add-hook 'org-insert-heading-hook #'evil-append-line :args '(1) :mode 'evil-mode)
;; **** evil-headline-state
;; :PROPERTIES:
;; :ID:       20231013T165637.036896
;; :END:
;; I predominately navigate org-mode via headlines.  But there are cases where I do
;; not want to do this and prefer normal editing.  For example, when I'm editing
;; paragraph descriptions. This is why I've decided to create a specific state for
;; headline-based operations. This is a state I've designed to traverse
;; headlines. Most of the time I'm using =org-mode= I am operating by headlines.  I
;; bind =j= and =k= in =normal-state= to [[file:snapshots/_helpful_command__oo-dwim-next-visible-headline_.png][oo-dwim-next-visible-heading]] and
;; [[file:snapshots/_helpful_command__oo-dwim-previous-visible-heading_.png][oo-dwim-previous-visible-heading]] respectively.  And I think doing this does
;; actually save me lots of time.  But it is not always the best way to edit with
;; org=.  Often I do want to go line by line and have normal =normal-state=
;; editing.  Therefore, I decided to make this "headline-traversal" into a state;
;; that way I can toggle off the keybindings for traversing headlines when I want
;; to edit the contents of an individual headline.
;; ***** create a state traversing and performing operations on headlines
;; :PROPERTIES:
;; :ID:       20231002T072906.662878
;; :END:
;; Using a state has the advantage that I can toggle on and off bindings.
(after! (evil org)
  (evil-define-state headline
    "Navigate and perform operations on org headlines"
    :tag " <H> "
    :suppress-keymap t)
  (evil-set-initial-state 'org-mode 'headline)
  (set! evil-headline-state-cursor '(box "violet")))

;; I eventually want to automate this for the creation of new states.  I think
;; it is bound to happen multiple times.
(oo-bind 'oo-override-mode-map :h ";" #'execute-extended-command)
(oo-bind 'oo-override-mode-map :h oo-normal-leader-key #'oo-leader-prefix-command)
(oo-bind :h [escape] (lambda () (interactive) (@exit-everything)))
;; ***** use =n= to narrow to subtree
;; :PROPERTIES:
;; :ID:       20231015T194001.993245
;; :END:
;; Often I want to focus specifically one subtree and to try to make that subtree as
;; good as possible.  Therefore, I want a dedicated keybinding.
(oo-bind :h "w" #'widen)
(oo-bind :h "n" #'org-narrow-to-subtree)
;; ***** bail out option
;; :PROPERTIES:
;; :ID:       20231125T151746.986784
;; :END:
;; I am not certain whether =C-j= is actually the best keybinding for this bailout
;; binding.  But I will find out if it isn't.  I need a key that allows me to
;; switch from =evil-normal-state= to =evil-headline-state= and one that lets me go
;; back.  This is akin to =i= in normal state which lets me switch to insert state.
;; #+begin_src elisp
;; (oo-bind :h "x" #'evil-normal-state)
;; (oo-bind :h "C-j" #'evil-normal-state)
;; (oo-bind 'org-mode-map :n "C-j" #'evil-headline-state)
;; #+end_src
;; ***** don't fallback on =org-ret= on =RET=
;; :PROPERTIES:
;; :ID:       20231125T141244.354455
;; :END:
;; Because I used the =:suppress-keymap= option, =evil-headline-state= doesn't have
;; any bindings to insert keys setup.  But that does mean org takes up any bindings
;; that are missing.  And because of this I ended up invoking =org-ret= by pressing
;; =RETURN= accidentally.
;; #+begin_src elisp
;; (oo-bind :h "RET" #'ignore)
;; #+end_src
;; ***** set parent keymap to =evil-normal-state-map=
;; :PROPERTIES:
;; :ID:       20231126T184413.744314
;; :END:
;; [[https://www.gnu.org/software/emacs/manual/html_node/elisp/Inheritance-and-Keymaps.html][inheritance-and-keymaps]]
;; I want to default to the same keybindings in =evil-normal-state-map= for keys I
;; have not yet bound.  At first I tried using ~(set-keymap-parent
;; evil-headline-state-map evil-normal-state-map)~ but this doesn't work because
;; =evil-normal-state-map= inherits many of its bindings from =evil-motion-state-map=
;; and inheritence is not transitive as I initially thought.

;; To truly be thorough I may have to use the map composing idiom recommended in
;; the manual.

;; In regards to inheritance of bindings, I notice thatI notice
;; that even though I have bound =+= to [[file:snapshots/_helpful_function__text-scale-increase_.png][text-scale-increase]] in
;; =evil-motion-state-map= that binding is not passed on to =evil-headline-state-map=
;; even though I made it a child keymap.
;; #+begin_src elisp
;; ;; (after! (:keymap evil-headline-state-map)
;; ;;   )
;; (alet (lambda () (set-keymap-parent evil-headline-state-map (make-composed-keymap evil-motion-state-map evil-normal-state-map)))
;;   (oo-call-after-keymap 'evil-headline-state-map it))
;; #+end_src
;; ***** use =H= to go up a headline
;; :PROPERTIES:
;; :ID:       20230916T062323.349539
;; :END:
;; I need a key I can use to go to the parent of a subheading.  Additionally, the
;; command should clean up after me--by which I mean it should fold everything.
(alet (lambda () (interactive)
        (org-fold-hide-subtree)
        (awhen (org-up-heading-safe)
          (org-fold-hide-subtree)
          (org-show-children)
          (oo-org-heading-start)))
  (oo-bind :h "H" it))
;; (awhen (org-up-heading-safe)
;;   (org-fold-show-children))
;; ***** define an easymotion for org headings
;; :PROPERTIES:
;; :ID:       20230908T185923.791918
;; :END:
;; I only want one heading open at a time to minimize distractions.  Moreover, I
;; want my headline tree state to be "clean".
(after! evil-easymotion
  (defevilem! oo-goto-headline ()
    "Jump the beginning of the text at headline title."
    (:scope 'page)
    (:initial-point #'point-min)
    (org-next-visible-heading 1)
    (oo-org-heading-start)))

;; Select a headline to jump to.  Show the headline and hide all the siblings of
;; around that headline.
(defun! oo-goto-headline-and-open ()
  (interactive)
  (oo-goto-headline)
  (save-excursion
    (awhen (org-up-heading-safe)
      (org-fold-hide-subtree)
      (org-fold-show-children)))
  (org-show-children)
  (org-show-entry))

(oo-bind :h "f" #'oo-goto-headline-and-open)
;; ***** fold subtrees with =TAB=
;; :PROPERTIES:
;; :ID:       20230120T160800.110855
;; :END:
;; By default =org-mode= has the command =org-cycle= for folding subtrees.  I
;; personally don't like this command because I find it unusual.  It has three
;; states of folding and I am used to just two.  Furthermore, it is a command that
;; is used for many different things in different situations--kind of like a =dwim=
;; command.
(oo-bind :h "TAB" #'evil-toggle-fold)
;; ***** create a binding for refile copy
;; :PROPERTIES:
;; :ID:       20230825T181221.829533
;; :END:
;; I have copy of my org files in my trash folder which I will eventually delete that is when I've
;; reproduced it this file in its entirety.  It's read-only because I don't want to end up inadvertently
;; editing it.  So refiling without actually deleting the current headline is useful.
(oo-bind :h "C" #'org-refile-copy)
;; ***** binding for editing paragraph
;; :PROPERTIES:
;; :ID:       f19eb603-2668-457b-9a9c-64698d02ba98
;; :END:
;; Editing paragraphs is so common that it warrants having a short localleader
;; binding.  But for the sake of intuitive and easy key binding remembering, I use
;; "e d" for "edit description".
(oo-bind :h "ed" #'oo-dwim-edit-paragraph :wk "edit paragraph" :localleader t)
(oo-bind :h "d" #'oo-dwim-edit-paragraph :wk "edit paragraph" :localleader t)
;; ***** general local leader bindings
;; :PROPERTIES:
;; :ID:       a537e6b9-6e8b-40f1-8ed1-04a4a9b8542e
;; :END:
(oo-bind :h "r" #'org-refile :wk "refile")
(oo-bind :h "t" #'org-babel-tangle :localleader t :wk "tangle")
;; ***** use =j= and =k= keys to go up and down headings in =org-mode=
;; :PROPERTIES:
;; :ID:       20230803T081108.324242
;; :END:
(oo-bind :h "j" #'oo-dwim-next-visible-heading)
(oo-bind :h "k" #'oo-dwim-previous-visible-heading)
;; ***** make =Y=, =D=, =P= correspond to copy, cut, paste
;; :PROPERTIES:
;; :ID:       20230801T204755.542889
;; :END:
(oo-bind :h "Y" #'org-copy-subtree)
(oo-bind :h "D" #'org-cut-subtree)
(oo-bind :h "P" #'org-paste-subtree)
;; ***** make =R= refile a headline
;; :PROPERTIES:
;; :ID:       20230801T180754.341666
;; :END:
(oo-bind :h "r" #'org-refile)
(oo-bind :h "R" #'org-refile)
;; ***** make =b= insert a source block to a heading
;; :PROPERTIES:
;; :ID:       20230801T165535.711852
;; :END:
(oo-bind :h "b" #'oo-dwim-insert-src-block)
;; ***** have =O= and =o= open headlines below and above respectively
;; :PROPERTIES:
;; :ID:       20230801T094841.208254
;; :END:
;; In =evil= =o= and =O= open lines above and below respectively.  I want to achive
;; some level of parity with evil.
(oo-bind :h "o" #'org-insert-heading-after-current)
(oo-bind :h "O" (lambda () (interactive)
                  (block! nil
                    (noflet! org-move-subtree-down (&rest _))
                    (funcall-interactively
                     #'org-insert-heading-after-current))))
;; ***** evaluate source block with =e= =E=
;; :PROPERTIES:
;; :ID:       20230801T164555.544440
;; :END:
;; As I've mentioned, I want to have a sort of parity.  There needs to be an element
;; of caution because we're talking about evaluation here--and even if what you
;; evaluating may not be malicious, it's a pain to evaluate something you didn't
;; intend to.  I accidentally evaluated my whole config again and I locked emacs forcing me to kill it
;; using =htop=.  That's why I choose to make it =E= instead of =e=.
(oo-bind :alt #'org-babel-execute-subtree #'oo-dwim-eval-src-block)
(oo-bind :h "ee" #'org-babel-execute-subtree :localleader t :wk "eval subtree")
(oo-bind :h "eE" #'org-babel-execute-subtree :localleader t :wk "eval subtree")
(oo-bind :h "E" #'org-babel-execute-subtree :wk "eval subtree")
;; ***** promoting and demoting headlines with =<= and =>=
;; :PROPERTIES:
;; :ID:       20230801T160324.759227
;; :END:
;; I find these bindings intuitive.
(oo-bind :h ">" #'org-demote-subtree)
(oo-bind :h "<" #'org-promote-subtree)
;; ***** move headlines up and down with =K= and =J=
;; :PROPERTIES:
;; :ID:       20230801T182225.846535
;; :END:
(oo-bind :h "J" #'org-metadown)
(oo-bind :h "K" #'org-metaup)
;; ***** revert to =evil-headline-state= on escaping =evil-insert-state=
;; :PROPERTIES:
;; :ID:       20231203T063813.205192
;; :END:
;; After I finished I opened a new headline pressed escape I was left in normal state but I
;; would rather be in =evil-headline-state= in an org buffer.

;; I'll note that the call to [[file:snapshots/_helpful_function__@exit-everything_.png][@exit-everything]] /must/ happen after the call to
;; =evil-headline-state=.  I think that [[file:snapshots/_helpful_function__keyboard-quit_.png][keyboard-quit]] exits the body of the
;; current function as well like =cl-return= exits a =cl-block=.
;; #+begin_src elisp
;; (oo-bind 'org-mode-map :i [escape] (lambda () (interactive)
;;                                      (evil-headline-state 1)
;;                                      (@exit-everything)))
;; #+end_src

;; **** org-superstar
;; :PROPERTIES:
;; :ID:       03677091-ad9b-419c-9a55-de1acc76c5f0
;; :END:
(set! org-superstar-leading-bullet ?\s)
;; (set! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))
(oo-add-hook 'org-mode-hook #'org-superstar-mode)
;; **** oo-beginning-of-heading
;; :PROPERTIES:
;; :ID:       20231126T051858.753690
;; :END:
;; #+begin_src elisp
;; (defun! oo-org-heading-start ()
;;   (save-match-data
;;     (let! regexp (rx (seq bol (one-or-more "*")
;;                           (opt (one-or-more "/s")
;;                                (one-or-more upper))
;;                           (opt (one-or-more "/s")
;;                                "[#" nonl "]")
;;                           (group (1+ nonl)))))
;;     (looking-at regexp)
;;     (goto-char (1+ (match-beginning 1)))))
;; #+end_src
;; **** go to the last asterix of the previous visible heading
;; :PROPERTIES:
;; :ID:       20230803T121741.861596
;; :END:
;;  I got inspiration for the regexp from the variable [[~/dotfiles/snapshots/helpful-variable-org-complex-heading-regexp.png][org-complex-heading-regexp]].
;;  Something to note is that the regular expression didn't work at first because I
;;  had to set [[~/dotfiles/snapshots/helpful-variable-case-fold-search.png][case-fold-search]] to nil.
(defun! oo-dwim-previous-visible-heading (&optional next)
  (interactive)
  (let! case-fold-search nil)
  (if next
      (org-next-visible-heading 1)
    (org-previous-visible-heading 1))
  (org-back-to-heading)
  (oo-org-heading-start))
;; **** go to the last asterix of the next visible heading
;; :PROPERTIES:
;; :ID:       20230803T083436.804762
;; :END:
(defun! oo-dwim-next-visible-heading ()
  (interactive)
  (oo-dwim-previous-visible-heading t))
;; **** demote the children of the subtree
;; Sometimes I realize later that a parent heading I created is not appropriate. in
;; this situation I want to move all of the headline children out to the same level
;; of its parent and possibly delete the parent as well.
;; #+begin_src elisp
;; ;; Promote all of the children of current headline.  Then remove the current
;; ;; headline.
;; (defun oo-org-demote-children ()
;;   "Promote all the children of the current subtree."
;;   (interactive)
;;   (org-promote-subtree)
;;   (org-demote))
;; #+end_src
;; **** get bounds of paragraphs in a headline
;; :PROPERTIES:
;; :ID:       20230829T100701.290847
;; :END:
;; Note that this function assumes the headline structure is what I always do.
;; Headline, property-drawer, and source block.  I don't account for multiple source
;; blocks with descriptions interweaved between them as some people like to do.  My
;; style is to prefer at most one source block and at most one paragraph per
;; headline.
(defun! oo-get-paragraph-bounds ()
  "Return the beginning and end points of the paragraphs between the
property drawer and the source block in the current heading."
  (interactive)
  (save-excursion
    (save-restriction
      (org-back-to-heading t)
      (oo-narrow-to-heading)
      (let! beg (progn (org-end-of-meta-data t) (point)))
      (let! elt (org-element-at-point))
      (while (and elt (equal 'paragraph (org-element-type elt)))
        (goto-char (org-element-property :end elt))
        (let! elt (org-element-at-point)))
      (let! end (point))
      (list beg end))))
;; *** IDs
;; **** track ids across all org buffers
;; :PROPERTIES:
;; :ID:       20230731T162703.626679
;; :END:
;; If =org-id-track-globally= is nil, ids will only work within the current
;; buffer.  I'd rather have.
(set! org-id-track-globally t)
(set! org-id-locations-file (concat oo-cache-dir "org-id-locations"))
;; **** use timestamps ids for org mode
;; :PROPERTIES:
;; :ID:       20230731T161949.539252
;; :END:
;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(set! org-id-method 'ts)
;; **** always use ids to link to headlines, creating one if necessary
;; :PROPERTIES:
;; :ID:       20230731T162922.883964
;; :END:
;; When storing a link to a headline always use its org id (as opposed to doing a
;; text search); and if it doesn't have an id, create one for it.  Text searches for
;; the headline by name are not reliable as headline names may not be unique.  All
;; of my headlines should have a UUID to identify them.
(set! org-id-link-to-org-use-id t)
;; **** add ids to all headlines
;; :PROPERTIES:
;; :ID:       20231203T063813.182105
;; :END:
;; #+begin_src elisp
;; (defun oo-add-ids ()
;;   (interactive)
;;   (org-map-entries #'org-id-get-create))
;; #+end_src
;; *** miscellaneous
;; **** load =org-bookmark-heading=
;; :PROPERTIES:
;; :ID:       20230906T124415.922390
;; :END:
;; Load bookmark heading only after =org= and =bookmark= have been loaded.
(oo-call-after-load '(org bookmark) #'require 'org-bookmark-heading)
;; **** declare the org directory 
;; :PROPERTIES:
;; :ID:       20231203T063813.195802
;; :END:
;; This is the default location =org-capture= and =org-agenda= look for.  This
;; variable must be non-nil, or you'll get an error.
(set! org-directory (expand-file-name "~/dotfiles"))
;; **** increase line-spacing
;; :PROPERTIES:
;; :ID:       20231127T200707.767471
;; :END:
;; The text in =org-mode= is too bunched up together.
;; #+begin_src elisp
;; (add-hook 'org-mode-hook (lambda () (setq-local line-spacing 4)))
;; #+end_src
;; **** don't use a default notes file
;; :PROPERTIES:
;; :ID:       20221011T130124.440288
;; :END:
;; Similar to [[][org-directory-files]], =org-mode= needs this variable to be
;; defined.  This is used as a fallback for =org-capture= templates that do not
;; specify a default target file.
(set! org-default-notes-file null-device)
;; **** don't fontify line-based search commands
;; :PROPERTIES:
;; :ID:       20230906T170035.841929
;; :END:
;; By default =consult= preserves.

;; Not preserving fontification for line searching commands makes the text more legible to me,
;; particularly with org-mode which can have variety of colors and text sizes. Also not fontifying is
;; probably more performant too.
(set! consult-fontify-preserve nil)
;; **** get the a list of the org files in =org-directory=
;; :PROPERTIES:
;; :ID:       20230801T192659.537968
;; :END:
(defun oo-directory-files ()
  "Return a list of org files in the `org-directory'."
  (directory-files org-directory t "\\.org\\'"))
;; **** don't indent headline contents
;; :PROPERTIES:
;; :ID:       20230731T162259.752446
;; :END:
;; When this is non-nil, contents of a heading such as property drawer are indented.
(set! org-adapt-indentation nil)
;; **** make property drawers completely invisible
;; :PROPERTIES:
;; :ID:       20231016T093952.428931
;; :END:
(set! org-tidy-properties-style 'invisible)
(oo-add-hook 'org-mode-hook #'org-tidy-mode)
;; **** setter for =org-ml-headline-get-section=
;; :PROPERTIES:
;; :ID:       20221223T062710.161713
;; :END:
(gv-define-expander org-ml-headline-get-section
  (lambda (do place)
    (gv-letplace (getter setter) place
      (funcall do `(org-ml-headline-get-section ,getter)
               (lambda (v)
                 (macroexp-let2 nil v v
                   `(progn
                      ,(funcall setter `(org-ml-headline-set-section ,v ,getter)))))))))
;; **** use pretty characters for TODO items
;; :PROPERTIES:
;; :ID:       20231202T082806.700085
;; :END:
;; #+begin_src elisp
;; (set! org-superstar-special-todo-items t)
;; #+end_src
;; ** editing
;; :PROPERTIES:
;; :ID:       20230822T164243.090910
;; :END:
;; *** highlight quoted symbols
;; :PROPERTIES:
;; :ID:       20230827T193245.740779
;; :END:
(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
;; *** map =leader k l= to =consult-bookmark=
;; :PROPERTIES:
;; :ID:       20230826T213620.654543
;; :END:
(oo-bind 'oo-miscellany-map "l" #'consult-bookmark)
;; *** save bookmarks whenever I make a bookmark
;; :PROPERTIES:
;; :ID:       20230826T213345.461005
;; :END:
;; By default this variable is set to =t= which means, save bookmarks whenever emacs is killed.  An
;; integer mans to save my bookmarks whenever I set a bookmark.  That way I minimize the chances of
;; losing them if emacs crashes.
(set! bookmark-save-flag 1)
;; *** don't pair quote
;; :PROPERTIES:
;; :ID:       20230826T081813.883798
;; :END:
(defafter! dont-pair-quotes (smartparens)
  (sp-local-pair sp-lisp-modes "'" nil :actions nil))
;; *** when in a comment or string pair =`= with ='=
;; :PROPERTIES:
;; :ID:       20230825T204350.864470
;; :END:
(defafter! pair-backquote-quote (smartparens)
  (sp-local-pair sp-lisp-modes "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
;; *** avy
;; :PROPERTIES:
;; :ID:       78064d92-2dde-4067-8a10-208ca6e88852
;; :END:
(set! avy-style 'pre)
(set! avy-keys (number-sequence 97 122))
;; *** auto capitalize words in programming modes
;; I use [[][captain]] to determine whether words should be capitalized.  To capitalize
;; words it uses the functions stored in the local variables
;; [[][captain-predicate]] and [[][captain-sentence-start-function]].  The
;; variable =captain-predicate= is what's called; whereas, the variable ==.

;; The package captain recommends.

;; These recommendations works well in =text-mode= but as well in =prog-mode=.  The
;; default sentence start function didn't work perfectly for =prog-mode=. Namely,
;; I've found the following problems:

;; - It fails to capitalize words at the beginning of a sentence in a comment.
;; - It fails to capitalize the word at the beginning of a docstring.
;; **** why does it fail to insert
;; I don't think that =bounds-of-thing-at-point= knows about the sentence being
;; in a comment.  Initially I tried to fix this using =comment-start= but that
;; doesn't work here.  First I want to see what are actually the bounds of
;; ~~.  And then I will decide what to do from there.
;; **** determine whether point is in a docstring
;; By "docstring" I mean the docstring of an elisp function.  Though it would be
;; nice if it could work for docstrings in other programming languages.
;; The idiom ~(nth 8 (syntax-pps (point)))~ tells me whether I'm in a
;; string and a docstring is a string.  But I don't thing I should
;; auto-capitalize words in strings because usually they are exact.  Smartparens
;; has a function that checks for a docstring.
;; **** determine if point is in a comment or string
;; :PROPERTIES:
;; :ID:       20230830T200829.309997
;; :END:
(defsubst +captain-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss (point))))
;; **** return the point at the start of the last sentence
;; :PROPERTIES:
;; :ID:       20230830T191359.459288
;; :END:
;; This is the function that determines whether a word should be capitalized.
;; Captain checks whether a word should be capitalized when you press space
;; after typing a word.  Luckily, much of the work is done by using the "bound of
;; thing" functions in Emacs.
(defun! +captain-prog-mode-sentence-start-function ()
  "Return point at the start of the last sentence.
Mean to be used as the value of `captain-predicate'."
  (cl-assert (require 'smartparens nil 'noerror))
  (awhen (car (bounds-of-thing-at-point 'sentence))
    (pushing! points it))
  (apply #'max points))
;; **** auto-capitalize sentences doc-string and comments
;; :PROPERTIES:
;; :ID:       20230830T190639.625324
;; :END:
(oo-add-hook 'prog-mode-hook #'captain-mode)

(defhook! auto-capitalize-sentences-in-docstrings-and-comments (prog-mode-hook)
  (setq-local captain-predicate #'+captain-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'+captain-prog-mode-sentence-start-function))
;; *** auto-capitalize sentences
;; :PROPERTIES:
;; :ID:       80781e44-dba1-4262-8337-4a9052d9ff4b
;; :END:
(oo-add-hook 'text-mode-hook #'captain-mode)

(defhook! auto-capitalize-sentences (text-mode-hook)
  (setq-local captain-predicate (lambda () t)))
;; *** aggressive-indent
;; :PROPERTIES:
;; :ID:       af75dbcb-5c65-4aef-9e87-09fc1f847747
;; :END:
;; [[https://github.com/Malabarba/aggressive-indent-mode][aggressive-indent]] indents portions of the text your working on as your typing
;; it.  It's pretty smart and very convenient.
(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
;; *** rainbow-delimiters
;; :PROPERTIES:
;; :ID:       5b58bb1c-5d3c-4f04-b4fb-c55f1588839e
;; :END:
;; [[https://github.com/Fanael/rainbow-delimiters][rainbow-delimiters]] colors parentheses different colors based on level.  This is a
;; great idea! It makes it really easy to see which parentheses go together.
(set! rainbow-delimiters-max-face-count 9)
(oo-add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(oo-add-hook 'reb-mode-hook #'rainbow-delimiters-mode)
;; *** enable code folding in programming modes
;; :PROPERTIES:
;; :ID:       76d1a702-2031-4bb4-a1b8-b4439d70684c
;; :END:
(oo-add-hook 'prog-mode-hook #'hs-minor-mode)
;; *** savehist
;; :PROPERTIES:
;; :ID:       54183df6-b4f5-4b01-9ddb-4054ef0583b0
;; :END:
;; =savehist= is a built-in feature for saving the minibuffer-history to a file--the
;; [[helpvar:savehist][savehist]] file.  Additionally, it provides the ability to save additional
;; variables which may or may not be related to minibuffer history.  You add the
;; ones you want to save to [[helpvar:savehist-additional-variables][savehist-additional-variables]].
(set! savehist-save-minibuffer-history t)
(set! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(set! savehist-autosave-interval (* 60 5))
(set! savehist-file (concat oo-cache-dir "savehist"))

(oo-add-hook 'on-first-input-hook #'savehist-mode)
;; *** saveplace
;; :PROPERTIES:
;; :ID:       63b04114-bcb9-4a2e-ad45-be4db8d4a269
;; :END:
;; As its name suggests, =save-place= is a built-in package that stores the buffer
;; location you left off at in a particular buffer.  When you visit that buffer
;; again, you are taken to the location you left off.  This is very convenient.
(set! save-place-file (concat oo-cache-dir "saveplace"))
(set! save-place-limit nil)

(oo-add-hook 'on-first-file-hook #'save-place-mode)
;; *** super-save
;; :PROPERTIES:
;; :ID:       684e788c-6db9-4e6e-826b-d4871c0a3f90
;; :END:
;; The default auto-saving feature in emacs saves after a certain number of
;; characters are typed (see [[helpvar:auto-save-interval][auto-save-interval]]).  The problem is that if you're in
;; the middle of typing and you've just hit the number of characters that trigger a
;; save, you could experience a lag, particularly if you are dealing with a large
;; file being saved.  Instead of doing this, [[https://github.com/bbatsov/super-save][super-save]] saves buffers during idle
;; time and after certain commands like [[helpfn:switch-to-buffer][switch-to-buffer]] (see [[helpvar:super-save-triggers][super-save-triggers]]).
;; Note that this is the same strategy employed by [[id:c550f82a-9608-47e6-972b-eca460015e3c][idle-require]] to load packages.
;; Saving files like this reduces the likelihood of user delays.
(set! super-save-auto-save-when-idle t)
(set! super-save-idle-duration 5)

(oo-add-hook 'on-first-file-hook #'super-save-mode)
;; *** disable lisp pairs in minibuffer
;; :PROPERTIES:
;; :ID:       0cba1d65-d5f2-462b-aa91-c6b5fa8818d7
;; :END:
;; "You're likely writing lisp in the minibuffer, therefore, disable these quote
;; pairs, which lisps doesn't use for strings." - Doom Emacs

;; Indeed. When using =eval-expression= I often want to quote a symbol. For example, when I'm trying to
;; load a feature by evaluating ~(require 'feature)~. With smartparens enabled without this setting
;; you'd get ~(require '')~ when you're typing the quote. Then you have to delete the completed quote
;; afterwards.
(defafter! disable-lisp-pairs-in-minibuffer (smartparens)
  "Disable auto-pairing for text used in minibuffer."
  (sp-local-pair 'minibuffer-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-mode "`" nil :actions nil))
;; *** enable adaptive filling
;; :PROPERTIES:
;; :ID:       0a498e8c-6397-4607-8ad1-0596df75aa98
;; :END:
(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)
;; *** evil
;; :PROPERTIES:
;; :ID:       20230822T083515.407412
;; :END:
;; **** stop at buffer boundaries at the end of a search
;; :PROPERTIES:
;; :ID:       20230824T075017.720370
;; :END:
(set! evil-search-wrap nil)
;; **** switch to insert state in minibuffer
;; :PROPERTIES:
;; :ID:       a23137c5-62a0-4e77-9e51-6a7372dac703
;; :END:
;; Before I just used ~(evil-change-state evil-previous-state)~ to revert the
;; state back to what it last was.  But this fails with ~evil-force-normal-state~
;; which is what I'm currently using to exit the minibuffer because then the
;; last state is normal state if the minibuffer is aborted.  Using a
;; =oo-evil-state-before-minibuffer= ensures that the state will be reverted to
;; the correct one.
(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! preserve-prior-evil-state (minibuffer-setup-hook)
  "Save state before entering the minibuffer and enter insert state."
  ;; :on evil-mode
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! restore-prior-evil-state (minibuffer-exit-hook)
  "Restore state after minibuffer."
  ;; :on evil-mode
  (when (bound-and-true-p evil-mode)
    (evil-change-state oo-evil-state-before-minibuffer)
    (setq oo-evil-state-before-minibuffer nil)))
;; **** don't message the current state
;; :PROPERTIES:
;; :ID:       20230815T213317.354181
;; :END:
;; **** colors and shapes
;; :PROPERTIES:
;; :ID:       b9d4490c-ef9f-4a04-809c-35e213f2029a
;; :END:
;; **** use =escape= to quit everything
;; :PROPERTIES:
;; :ID:       ea9378de-e5c5-482c-b53b-743a81e3bc8e
;; :END:
;; We want escape to be a "quit everything" keybinding.
(defvar oo-escape-hook nil
  "Hook run after escaping.")

(defun @exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((run-hook-with-args-until-success 'oo-escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        (t (keyboard-quit))))

(oo-bind :ie [escape] #'evil-force-normal-state)

(oo-add-advice #'evil-force-normal-state :after #'@exit-everything)
(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)
;; **** use =C-u= to invoke the prefix argument
;; :PROPERTIES:
;; :ID:       20230815T213609.427527
;; :HEADER-ARGS: :tangle no
;; :END:
(set! evil-want-C-u-delete nil)
;; *** enable =evil-surround= in =text-mode= and =prog-mode=
;; :PROPERTIES:
;; :ID:       20230801T152330.619190
;; :END:
(oo-add-hook 'prog-mode-hook #'evil-surround-mode)
(oo-add-hook 'text-mode-hook #'evil-surround-mode)
;; *** auto-pairing during =eval-expression= and =evil-ex=
;; :PROPERTIES:
;; :ID:       5173d780-c2ce-4e78-b1cc-0f4ffa2fde7d
;; :END:
(defhook! enable-smartparens-maybe (minibuffer-setup-hook)
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))
;; *** easymotion
;; :PROPERTIES:
;; :ID:       20230829T094012.443846
;; :END:
;; **** defevilem! - a declarative easymotion definer
;; :PROPERTIES:
;; :ID:       20230826T212539.034211
;; :END:
;; :PROPERTIES:
;; :ID:       305b254a-687c-4373-bd49-1010b1be4257
;; :END:
;; This macro provides me a familiar =defun= syntax for defining an interactive
;; function.  The interacive function
(defmacro! defevilem! (&rest args)
  "Convenience macro for defining an `evil-easymotion' motion."
  (declare (indent defun))
  (let! (name arglist metadata keys body) (oo-destructure-defun-plus args))
  (let! lambda `(lambda ,arglist (interactive) ,@metadata (block! nil ,@body)))
  `(progn (oo-autoload-fn ',name 'evil-easymotion)
          (after! evil-easymotion (evilem-make-motion ,name ,lambda ,@keys))))
;; **** oo-goto-beginning-of-word
;; :PROPERTIES:
;; :ID:       20230724T080532.739889
;; :END:
(defevilem! oo-goto-beginning-of-word ()
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
;; **** oo-goto-end-of-word
;; :PROPERTIES:
;; :ID:       20230724T080538.981325
;; :END:
(defevilem! oo-goto-end-of-word ()
  "Jump to the end of word in the current buffer."
  (:scope 'page)
  (:initial-point #'point-min)
  (let! regexp (rx (1+ alnum)))
  (save-match-data
    (awhen (save-excursion (goto-char (1+ (point)))
                           (re-search-forward regexp nil t nil))
      (goto-char (1- (match-end 0))))))
;; **** oo-goto-char
;; :PROPERTIES:
;; :ID:       20230724T080544.188502
;; :END:
(defevilem! oo-goto-char ()
  "Jump to the character in current buffer."
  (:initial-point #'point-min)
  (:scope 'page)
  (:bind ((char (read-char "Char: "))))
  (with! (save-match-data))
  (when (save-excursion (goto-char (1+ (point)))
                        (re-search-forward (rx-to-string (char-to-string char)) nil t nil))
    (goto-char (1- (match-end 0)))))
;; **** evil-easymotion
;; :PROPERTIES:
;; :ID:       9be10754-3975-49d4-afa7-08a122161c19
;; :END:
;; Here I expand the number of =evil-easymotion= keys used for jumping.  I also set
;; the style to 'at.
(set! evilem-keys (number-sequence ?a ?z))
(set! evilem-style 'at)
;; **** easymotion bindings
;; :PROPERTIES:
;; :ID:       20230726T061512.349723
;; :END:
(oo-bind :nv "w" #'oo-goto-beginning-of-word)
(oo-bind :nv "e" #'oo-goto-end-of-word)
(oo-bind :nv "f" #'oo-goto-char)
;; *** don't remember places in dashboard
;; :PROPERTIES:
;; :ID:       20230827T192814.749400
;; :END:
(adjoin! dogears-ignore-modes 'dashboard-mode)
;; *** bind =dogears-go= to quick binding
;; :PROPERTIES:
;; :ID:       20230827T033723.692060
;; :END:
(oo-bind 'oo-leader-map "ll" #'dogears-go)
;; *** don't remember org source buffers
;; :PROPERTIES:
;; :ID:       20230827T033226.903128
;; :END:
(defun oo-org-src-buffer-p () (bound-and-true-p org-src-mode))
(adjoin! dogears-ignore-places-functions #'oo-org-src-buffer-p)
;; *** persist dogears list
;; :PROPERTIES:
;; :ID:       20230827T012442.840068
;; :END:
(adjoin! savehist-additional-variables 'dogears-list)
;; *** add registers to persisted variables
;; :PROPERTIES:
;; :ID:       20230827T012048.421913
;; :END:
;; Registers are very useful; don't see why they can't be persisted like bookmarks.
(adjoin! savehist-additional-variables 'register-alist)
;; *** enable dogears as soon as possible
;; :PROPERTIES:
;; :ID:       20230827T013649.225217
;; :END:
(oo-add-hook 'on-first-input-hook #'dogears-mode)
;; ** uncategorized
;; :PROPERTIES:
;; :ID:       20230827T140431.419127
;; :END:
;; *** enable abbrev mode in programming modes
;; :PROPERTIES:
;; :ID:       20230916T062330.080077
;; :END:
(oo-add-hook 'prog-mode-hook #'abbrev-mode)
;; *** add specific bookmarks for outshine buffers
;; If I've structured my buffer in an outshine friendly way and I am using
;; property ids, it means that I can more accurately locate specific text
;; because that text will have an id.
;; *** pairs org markup indicators in outshine when inside a comment
;; :PROPERTIES:
;; :ID:       20230915T134425.749031
;; :END:
;; I want =smartparens= to help me complete org markup in comments of buffers
;; where I have =outshine-mode= enabled.  Note that the first argument
;; =sp-local-pair= is called =modes= but it means specifically major modes; it
;; doesn't work with minor modes.  And =outshine-mode= is a minor
;; mode. According to the [[file:snapshots/pair-management.rst.png][documentation]] we can specify an arbitrary symbol
;; and update the local pairs.
(after! smartparens
  (sp-local-pair 'outshine-pairs "=" "=" :when '(sp-in-comment-p))
  (sp-local-pair 'outshine-pairs "~" "~" :when '(sp-in-comment-p))
  (sp-local-pair 'outshine-pairs "*" "*" :when '(sp-in-comment-p))
  (sp-local-pair 'outshine-pairs "/" "/" :when '(sp-in-comment-p)))

(oo-add-hook 'outshine-mode-hook #'sp-update-local-pairs :args '(outshine-pairs))
;; *** bind =outline-toggle-children= to =TAB= in =outshine-mode=
;; :PROPERTIES:
;; :ID:       20230915T075630.975188
;; :END:
;; Let =TAB= trigger =outshine-cycle= in normal state.

;; I'll note that =outshine-cycle= behaves differently from =org-cycle.  In the
;; current version of org, =org-cycle=.
(oo-bind 'outshine-mode-map "TAB" #'outline-toggle-children)
;; *** don't use background for spacemacs comments
;; By default spacemacs uses a bluish background for comments.  I find it
;; visually jarring.
(set! spacemacs-theme-comment-bg nil)
;; *** silence flyspell message about setting up dictionary
;; :PROPERTIES:
;; :ID:       20230830T201512.810823
;; :END:
;; When flyspell is enabled, it outputs a message about.
(oo-add-advice #'flyspell-mode :around #'oo-funcall-silently)
;; *** enable spell-checking in =text-mode=
;; :PROPERTIES:
;; :ID:       20230830T081417.409762
;; :HEADER-ARGS: :tangle no
;; :END:
(oo-add-hook 'text-mode-hook #'flyspell-mode)
;; *** save bookmarks in my cache
;; :PROPERTIES:
;; :ID:       20230827T140443.085098
;; :END:
;; You have to use =setq-default= here.
(setq-default bookmark-default-file (expand-file-name "bookmarks" oo-cache-dir))
;; *** oo-set-font-face
;; :PROPERTIES:
;; :ID:       800c92cf-01f6-45d2-99e3-ae86bdaae9d6
;; :END:
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (let! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))
;; *** fix elisp indentation :text-editing:
;; :PROPERTIES:
;; :ID:       834eff05-1b96-4295-a46c-d14f81b43ad6
;; :HEADER-ARGS: :tangle no
;; :END:
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
;; *** prevent =transient= from creating files everywhere
;; :PROPERTIES:
;; :ID:       1805bd2d-b190-47b6-9c7c-2a200f9cf6a1
;; :END:
(set! transient-levels-file (concat oo-cache-dir "transient/levels"))
(set! transient-values-file (concat oo-cache-dir "transient/values"))
(set! transient-history-file (concat oo-cache-dir "transient/history"))
;; *** replace =$HOME= with =~= in recentf files
;; :PROPERTIES:
;; :ID:       20230803T162034.660750
;; :END:
;; Replace $HOME with ~, which is more portable, and reduces how much
;; horizontal space the recentf listing uses to list recent files.  --noctuid
(adjoin! recentf-filename-handlers #'abbreviate-file-name)
;; *** remove text properties recentf files
;; :PROPERTIES:
;; :ID:       20230803T154635.054163
;; :END:
;; Text properties inflate the size of recentf's files, and there is
;; no purpose in persisting them, so we strip them out.  --noctuid
(adjoin! recentf-filename-handlers #'substring-no-properties)
;; *** show me spelling suggestion in code comments/docstrings
;; :PROPERTIES:
;; :ID:       20230829T190254.177548
;; :END:
(oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; *** declare the program used for spell checking
;; :PROPERTIES:
;; :ID:       20230829T182604.675296
;; :END:
(set! ispell-program-name (or (executable-find "hunspell") (executable-find "ispell")))
;; *** oo-split-window-right-and-focus
;; :PROPERTIES:
;; :ID:       6cb60d94-723b-48e5-850a-3483e78f6647
;; :END:
(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))
;; *** oo-split-window-below-and-focus
;; :PROPERTIES:
;; :ID:       d6a4a81f-007d-4b7e-97a3-e0bba3ff97a4
;; :END:
(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;; *** url
;; :PROPERTIES:
;; :ID:       d4c0b77e-7c6c-4ab3-bf20-fb0d335eb771
;; :END:
(set! url-cache-directory (concat oo-cache-dir "url/"))
(set! url-cookie-file (concat oo-cache-dir "url/cookies.el"))
(set! url-history-file (concat oo-cache-dir "url/history.el"))
(set! url-configuration-directory (concat oo-cache-dir "url/configuration/"))
;; *** remove text properties from =kill-ring= before saving with savehist
;; :PROPERTIES:
;; :ID:       20230907T092216.085893
;; :END:
(defadvice! remove-properties-from-kill-ring (before savehist-save)
  (setq kill-ring (-map-when #'stringp #'substring-no-properties kill-ring)))
;; *** open =*Occur*= buffer at bottom
;; :PROPERTIES:
;; :ID:       20230907T115440.516809
;; :END:
(oo-popup-at-bottom "*Occur")
;; *** magit
;; :PROPERTIES:
;; :ID:       820ce76e-53e4-4be8-9cad-3bf35efeefd7
;; :END:
;; Open magit at the bottom.
(oo-popup-at-bottom "magit: ")

;; Use `completing-read'.
(set! magit-completing-read-function #'completing-read)
(set! magit-diff-refine-hunk t)
(set! magit-auto-revert-mode t)
;; *** query me asking me whether I want to kill emacs
;; :PROPERTIES:
;; :ID:       20230907T150303.739180
;; :END:
;; I want to avoid accidentally quitting Emacs.  I've done it before.  That is, unless I explicitly ask
;; not to be prompted.
(defhook! ask-before-quitting (kill-emacs-query-functions)
  "Prompt the user, asking them to confirm quitting Emacs."
  (y-or-n-p "Are you sure you want to quit Emacs?"))
;; *** define a capture template for scratch notes
;; :PROPERTIES:
;; :ID:       20230907T202653.524254
;; :END:
;; The Emacs template provides an on-the-fly way of adding a snippet of code to my
;; configuration.  However, I'd like a way of creating a snippet which is not fleshed out yet. This
;; capture template lets me add to the scratch buffer, which does not tangle to anything.
(defcapture! scratch ()
  :file (expand-file-name "~/dotfiles/scratch.org")
  :prepend t
  :template (-partial (function oo-src-block-headline) nil "emacs-lisp"))
;; *** show an indicator in the minibuffer
;; :PROPERTIES:
;; :ID:       20230803T154422.642011
;; :END:
(oo-add-hook 'on-first-input-hook #'minibuffer-depth-indicate-mode)
;; *** whether a symbol is
;; :PROPERTIES:
;; :ID:       20230908T102609.816121
;; :END:
(defun oo-symbol-p (symbol)
  "Return non-nil if symbol is an oo symbol."
  (oo-symbol-match-p "\\`oo[/-]" symbol))
;; *** enable =evil-magit= after =evil-magit-init=
;; :PROPERTIES:
;; :ID:       20230920T064900.423061
;; :END:
;; I need to sit down and figure out how exactly I'm going to use magit.  I might
;; end up replacing it with Emacs's vc alternatives which are in general less
;; flashy and featureful than =magit=, but much lighter.  For now though I'm used to
;; the evil magit bindings provided by =evil-magit-init=.
(oo-call-after-load 'magit #'evil-magit-init)
;; *** alias =pushing!= as =consing!=
;; :PROPERTIES:
;; :ID:       20230920T090737.794492
;; :END:
;; I don't know.  Sometimes I feel that =consing!= makes more sense to me than =pushing!=.
(defalias 'consing! 'pushing!)
;; *** load =smartparens-config=
;; :PROPERTIES:
;; :ID:       20231001T211903.342267
;; :END:
;; The feature =smartparens-config= contains general settings that are useful for a
;; variety of modes, notably =org-mode= and =emacs-lisp-mode=.  Furthermore, the
;; settings are properly loaded lazily (via =eval-after-load=) so =smartparens-config=
;; it's O.K. to load it this way.
(oo-call-after-load 'smartparens #'require 'smartparens-config)
;; *** enable =smartparens-mode= for =text-mode=
;; :PROPERTIES:
;; :ID:       20231001T212028.550989
;; :END:
;; I actually wrote this wanting to enable =smartparens= for =org-mode=, but I
;; remembered =org= is built on top of =text-mode=.
(oo-add-hook 'text-mode-hook 'smartparens-mode)
;; *** buffer snapshots - creating images of the contents of a buffer
;; :PROPERTIES:
;; :ID:       20230802T153110.062543
;; :END:
;; Since learning about =org-mode= I've wanted to use org links to refer to function
;; and variable documentation. And at first I thought that I'd just write a custom
;; link--something like =[[helpfn:foo][foo]]=--which would invoke helpful-function=
;; (or =describe-function= if helpful wasn't available). But then I realized that
;; people looking at my org file wouldn't be able use these links unless they
;; evaluated my implementation for the custom link.  Also custom links wouldn't
;; work on =github= either.
;; **** directory where snapshots will be saved
;; :PROPERTIES:
;; :ID:       20230802T163333.538937
;; :END:
;; +To be honest I am unsure where this directory should go.+ All snapshots will go
;; in the aptly named directory =snapshots= at the top level of my dotfiles directory.
(defvar oo-snapshot-dir (expand-file-name "~/dotfiles/snapshots/"))
;; **** automatically generate a nice file name based on buffer name
;; :PROPERTIES:
;; :ID:       20231003T121724.239312
;; :END:
;; The easiest thing to do is to just use the buffer name for the file name when
;; possible.  However I've noticed that org links have trouble reading some buffer
;; names.  Take the buffer name =*helpful command: htmlize-buffer*= as an example.  I
;; attempted to follow a link with a file named the same as this buffer, but it
;; didn't work--at least not properly.  I think it has to do with the name of the
;; file and that it has spaces in it.  It seems like in general it [[https://superuser.com/questions/29111/what-technical-reasons-exist-for-not-using-space-characters-in-file-names][isn't a good
;; idea to have file names with spaces in it]] anyway.

;; I told =chatgpt= to write me a function that "takes a string and returns the
;; string as a legal filename".  Fortunately, this is a kind of quantitative
;; question that chatgpt does well in answering.  I didn't ask it for the
;; =max-filename-length= thing, but that's a nice touch.
(defun! oo-legal-filename (string)
  "Convert a string into a legal filename.
Replace any illegal characters in string with \"_\". Also don't allow filename
to exceed 255 characters long."
  (let! illegal-chars-regexp "[/\\?%*:|\"<>. ]")
  (let! safe-char ?_)
  (let! max-filename-length 255)
  ;; Replace illegal characters with safe character
  (let! legal-filename (replace-regexp-in-string illegal-chars-regexp (char-to-string safe-char) string))
  ;; Ensure the filename length is within the allowed limit
  (when (> (length legal-filename) max-filename-length)
    (setq legal-filename (substring legal-filename 0 max-filename-length)))
  legal-filename)
;; *** change my wallpaper every 5 minutes when =wallpaper-cycle-mode= is enabled
;; :PROPERTIES:
;; :ID:       20231006T025343.452570
;; :END:
;; This variable controls how often (in seconds) to change your wallpaper when you
;; have =wallpaper-cycle-mode= enabled. The default is every =15= seconds but in my
;; opinion that's way too distracting and disorienting.  Therefore, I set it to =5=
;; minutes.  For me 5 minutes provides a good balance of not being too distracting,
;; but also not letting the current wallpaper get too boring. Note that if you
;; change this interval while =wallpaper-cycle-mode= is active it will not take
;; effect because the timer is already running; you have to disable and re-enable
;; wallpaper-cycle-mode= which would create a new timer.
(set! wallpaper-cycle-interval (* 5 60))
;; *** set directory where my wallpapers should go
;; :PROPERTIES:
;; :ID:       20231006T031131.263677
;; :END:
;; This is the symbol that provides the path for getting the set of wallpapers.  I
;; would have just name this =wallpaper-directory= or =wallpaper-dir= (probably the
;; former so that its more predictable) because this directory is used for more
;; than just =wallpaper-cycle-mode=.  For example, its also used in
;; [[file:snapshots/_helpful_command__wallpaper-set-wallpaper_.png][wallpaper-set-wallpaper]].
(set! wallpaper-cycle-directory (expand-file-name "~/dotfiles/wallpapers"))
;; *** when auto-committing add any new files
;; :PROPERTIES:
;; :ID:       20231006T122900.601589
;; :END:
;; The variable [[file:snapshots/_helpful_variable__gac-automatically-add-new-files-p_.png][gac-automatically-add-new-files-p]] automatically adds any untracked
;; files in the branch.  Obviously, this isn't perfect for all scenarios but I
;; think in general I do want to automatically add new files.  When it's not
;; appropriate, I can always just toggle this variable.
(set! gac-automatically-add-new-files-p t)
;; *** don't output to the messages buffer when committing
;; :PROPERTIES:
;; :ID:       20231007T105549.950291
;; :END:
;; By default, =git-auto-commit-mode= [[file:snapshots/example-of-gac-commit-message.png][outputs]] to the message buffer when you
;; make a commit.  Since I'm committing alot I find this message unnecessary and
;; distracting and therefore disable this.
(set! gac-silent-message-p t)
;; *** prompt me to sign ssh for auto-pushing
;; :PROPERTIES:
;; :ID:       20231007T191834.333252
;; :END:
;; I've been manually requiring =ssh-agency= and calling =ssh-agency-ensure=.  Instead,
;; I want Emacs to just prompt me when I need to.  The hook function should remove itself
;; after it has been called once because I'm already signed in at that point.  Then
;; again, not sure; I could just leave the hook there incase I'm signed out at some
;; point somehow.
(defun! gac-push@sign-into-ssh (orig-fn &rest args)
  "Prompt me for signing into ssh."
  (ssh-agency-ensure))

(advice-add #'gac-push :before #'gac-push@sign-into-ssh)
;; *** automatically push to github if possible
;; :PROPERTIES:
;; :ID:       20231007T105416.756421
;; :END:
;; It seems like [[][gac-automatically-push-p]] is a local variable.
(after! git-auto-commit-mode (setq-default gac-automatically-push-p t))
;; *** open browsers in new windows
;; :PROPERTIES:
;; :ID:       20231007T190523.840624
;; :END:
;; Getting this to work can be a bit tricky.  The first step is to set
;; [[file:snapshots/_helpful_variable__browse-url-new-window-flag_.png][browse-url-new-window-flag]] to a non-nil value.  However, as I found this might
;; still not work.  I realized that in the body of [[file:snapshots/_helpful_command__browse_url_.png][browse-url]] the function that's
;; used to decide what opens the browser is the value of
;; [[file:snapshots/_helpful_variable__browse-url-browser-function_.png][browse-url-browser-function]] which is the function [[file:snapshots/_helpful_function__browse-url-default-browser_.png][browse-url-default-browser]].
;; If you examine the [[file:snapshots/_helpful_function__browse-url-default-browser_.png][body]] of this function you'll see there's a condition
;; ~((browse-url-can-use-xdg-open) 'browse-url-xdg-open)~ /before/ it checks whether to
;; use browser programs.  Thus, if the function =browse-url-can-use-xdg-open= returns
;; non-nil =browse-url-xdg-open= will be used and this function does not respect
;; =browse-url-new-window-flag=[fn:2].  So, I disable the use of "xdg open" by
;; advising =browse-url-can-use-xdg-open= to always return nil.
(set! browse-url-new-window-flag t)
(advice-add #'browse-url-can-use-xdg-open :around #'ignore)
;; *** use ros as the [[file:snapshots/_helpful_variable__inferior-lisp-program_.png][inferior-lisp-program]]
;; :PROPERTIES:
;; :ID:       20231003T153239.859857
;; :END:
;; I'm not sure, but my thinking is that ros recommends to set it to ~ros -Q run~
;; because this will handle the cases where you switch implementations.
(set! inferior-lisp-program "ros -Q run")
;; *** start the picom daemon
;; :PROPERTIES:
;; :ID:       20230702T133823.552540
;; :END:
;; This command is designed to allow me to start [[][picom]], a popular compositor,
;; to change the opacity of my applications.
(defun! oo-start-picom ()
  (interactive)
  (let! executable (executable-find "picom"))
  (let! dotfile (f-full "~/.config/picom/picom.conf"))
  (let! buffer (generate-new-buffer "*picom*"))
  (let! process (start-process "picom" buffer executable))
  (if process
      (progn (lgr-info oo-lgr "Picom has started...")
             (kill-buffer buffer))
    (message "Picom has failed to start...")))
;; *** make a =display-buffer-base-action= =edwina=
;; :PROPERTIES:
;; :ID:       20231010T123403.492097
;; :END:
;; The package [[https://gitlab.com/ajgrf/edwina][edwina]] seems to be the closest to doing [[id:20231009T185113.102581][what I want]].  It
;; involves =display-buffer= and it does mantain windows well.  But it doesn't seem to
;; respect =display-buffer-alist=.  For example, I have [[file:screenshots/_helpful_command__vertico-buffer-mode_.png][vertico-buffer-mode]] enabled and I
;; have vertico buffers [[file:snapshots/vertico-buffers-in-display-buffer-alist.png][registered]] in =display-buffer-alist= to display on the bottom but
;; =edwina= still displays it on the right as if it were a slave.  I don't
;; want it influenced by =edwina= but =edwina= still thinks it is a slave.  I expected
;; it to ignore buffers that are already managed by =display-buffer-alist=.  +I
;; suspect that+ This is because =edwina= takes the wrong approach by advising =display-buffer=.
;; Instead, it should be providing a function for =display-buffer-base-action=.

;; The function =oo-display-buffer-base-action= I have here is adapted straight from
;; [[file:snapshots/_helpful_function__edwina--display-buffer_.png][edwina--display-buffer]], the advice =edwina= adds to =display-buffer=.  I found it
;; looking at the definition of [[file:snapshots/_helpful_command__edwina-mode_.png][edwina-mode]].  Instead of using it as an advice, I
;; make it into its own function and set =display-buffer-base-action= to ~(cons
;; #'oo-display-buffer-base-action nil)~.  Now as I suspected =display-buffer-alist=
;; is respected.
(defun! oo-display-buffer-base-action (&rest args)
  "Open new window and ensure all windows are arranged as master-slave."
  (require 'edwina)
  (aprog1 (with-demoted-errors "Error: %S" (apply #'display-buffer-at-bottom args))
    (edwina--respective-window it
      (edwina-arrange))))

(set! display-buffer-base-action (cons #'oo-display-buffer-base-action nil))
;; *** create a variant =display-buffer= which includes "virtual" buffers
;; :PROPERTIES:
;; :ID:       20231011T153852.519343
;; :END:
;; Instead of using [[file:snapshots/_helpful_command__switch-to-buffer_.png][switch-to-buffer]] I use [[file:snapshots/_helpful_command__consult-buffer_.png][consult-buffer]] which is
;; enhanced with "virtual" buffers.  Virtual buffers are [[https://www.emacswiki.org/emacs/RecentFiles][recentf]] files, project
;; files, and [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html#:~:text=Bookmarks%20are%20somewhat%20like%20registers%20in%20that%20they,record%20where%20you%20were%20reading%20in%20various%20files.][bookmarks]] (I do not know what project files are yet).  I want to do the
;; same for =display-buffer= but there doesn't seem to  be a consult equivalent.
;; Fortunately, I can make one myself by temporarily setting
;; [[file:snapshots/_helpful_variable__consult--buffer-display_.png][consult--buffer-display]] to =display-buffer=.
(defun! oo-display-buffer ()
  (interactive)
  (require 'consult)
  (let! consult--buffer-display #'display-buffer)
  (call-interactively #'consult-buffer))

(oo-bind :alt #'display-buffer #'oo-display-buffer)
;; *** map the info command to =I= in =oo-package-map=
;; :PROPERTIES:
;; :ID:       20230906T105108.723015
;; :HEADER-ARGS: :tangle no
;; :END:
;; I am using the info command frequently to get information of packages that I install so that I can
;; write their recipe down. It merits its own function.
(oo-bind 'oo-package-map "I" #'elpaca-info :wk "info")
