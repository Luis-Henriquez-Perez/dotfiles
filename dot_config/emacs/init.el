;; -*- lexical-binding: t -*-

;; This variable controls how often.  Setting it to =most-positive-fixnum=, a very big
;; number, essentially disables garbage collection.  The garbage collection is later
;; reset to a reasonable value.
(setq gc-cons-threshold most-positive-fixnum)

;; The variable `oo-debug-p' is snatched from [[https://github.com/hlissner/doom-emacs][Doom's]] [[https://github.com/hlissner/doom-emacs/blob/develop/core/core.el][doom-debug-mode]].  The point of this variable is
;; to serve as an indicator of whether the current Emacs instance is run for
;; debugging.  When Emacs is set up for debugging it prints out many messages about
;; what its doing via [[hfn:void-log][oo-log]].
(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")

;; It's useful to store directories which I reference frequently in variables and
;; functions.  This way I can reference the full path.  Certain directories are
;; important; and I end up referencing them alot.  One of these is my
;; cache directory.
(defvar oo-cache-dir (concat user-emacs-directory "cache/")
  "Directory containing files used for caching information.")

;; Suppress file handlers operations at startup. The value of `file-name-handler-alist' is
;; consulted on each call to `require' and `load'. Here I disable it (set it to nil) and schedule it
;; to be re-enabled after startup. I got this from centaur emacs.
(when (not (or (daemonp) noninteractive init-file-debug))
  (defvar original-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (defun emacs-startup-hook&restore-file-name-handler-alist ()
    (setq file-name-handler-alist original-file-name-handler-alist)
    (makunbound 'original-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'emacs-startup-hook&restore-file-name-handler-alist))

;; Put the base directory into the `load-path', making sure it's at the front.
(push (expand-file-name "base" user-emacs-directory) load-path)

;; Add the base directory to the load-path.
(let (font)
  (setq font (or (font-spec :name "Iosevka Comfy Wide"
			                :weight 'normal
			                :slant 'normal
			                :size 15)
	             (font-spec :name "SpaceMono Nerd Font"
			                :weight 'normal
			                :slant 'normal
			                :size 15)
		         (font-spec :name "iMWritingMono Nerd Font Mono"
			                :weight 'normal
			                :slant 'normal
			                :size 15)))
  (set-face-attribute 'default nil :font font))

;; Not =cl= which is depreciated.  Loading =cl= instead of =cl-lib= will create an annoying warning message
;; during startup that says package cl is depreciated.
;; https://emacs.stackexchange.com/questions/66758/package-cl-is-deprecated-is-there-any-easy-fix-for-it
(require 'cl-lib)

(require 'oo-settings)

(require 'oo-packages)

;; If there are any incomplete queues, complete them and restart emacs.
(if-let ((queues (reverse elpaca--queues))
	     ((mapc #'elpaca--maybe-reset-queue queues))
	     (incomplete (cl-find 'incomplete queues :key #'elpaca-q<-status)))
    (progn (elpaca-process-queues)
	       (add-hook 'elpaca-after-init-hook #'restart-emacs))
  (run-hooks 'elpaca--post-queues-hook))

(require 'on)

(require 'log4e)

(log4e:deflogger "oo" "%t [%l] %m" "%H:%M:%S")

(defalias 'oo-log 'oo--log-info)
(defalias 'oo-log-info 'oo--log-info)
(defalias 'oo-log-warn 'oo--log-warn)
(defalias 'oo-log-debug 'oo--log-debug)
(defalias 'oo-log-error 'oo--log-error)
(defalias 'oo-log-fatal 'oo--log-fatal)
(defalias 'oo/open-log 'oo--log-open-log)

(oo--log-set-level 'trace)

(oo--log-enable-logging)

(require 's)

(require 'oo-base-utils)
(require 'oo-block-macro)

(defun oo-defun-components (arglist)
  "Return the components of defun.
ARGLIST is the arglist of `defun' or similar macro.
The components returned are in the form of (name args (docstring declaration interactive-form) body)."
  (block! nil
    (let! (name args . body) arglist)
    (let! docstring (when (stringp (car body)) (pop body)))
    (let! declarations (when (equal 'declare (car-safe (car body))) (pop body)))
    (let! interactive-form (when (equal 'interactive (car-safe (car body))) (pop body)))
    (list name args (list docstring declarations interactive-form) body)))

(defun oo-destructure-defun-plus (arglist)
  "Return list (name args (docstring declarations) plist body) from DEFUN-ARGS."
  (block! nil
    (let! (name args (docstring declarations) body) (oo-defun-components arglist))
    (let! (args plist) (-split-with (-not #'keywordp) args))
    (let! (raw body) (-split-with (-compose #'keywordp #'car-safe) body))
    (for! ((elt &as k v) raw)
      (let! (k . v) elt)
      (pushing! alist (cons k (if (cdr v) v (car v)))))
    (let! new (if (and plist alist)
		          (map-merge 'plist plist alist)
		        (map-into (or alist plist) 'plist)))
    (list name args (list docstring declarations) new body)))

(defmacro lambda! (args &rest body)
  "Wrapper around lambda that uses `block!'"
  (declare (indent defun))
  `(lambda (,@args) (block! nil ,@body)))

(defmacro defmacro! (&rest args)
  "Wrapper around `defmacro!'."
  (declare (indent defun) (doc-string 3))
  (-let [(name arglist metadata body) (oo-defun-components args)]
    `(defmacro ,name ,arglist
       ,@(-non-nil metadata)
       (block! ,name
	     (excluding! ,@(-remove #'oo-ampersand-symbol-p (-flatten arglist)))
	     (catch-autoloads! ,@body)))))

(defmacro! defun! (&rest args)
  "Wrapper around `defun'."
  (declare (indent defun) (doc-string 3))
  (-let [(name arglist metadata body) (oo-defun-components args)]
    `(defun ,name ,arglist
       ,@(-non-nil metadata)
       (block! ,name
	     (excluding! ,@(-remove #'oo-ampersand-symbol-p (flatten-list arglist)))
	     (catch-autoloads! ,@body)))))

(add-to-list 'log4e-log-level-alist '(setting . 3))
(log4e--def-level-logger "oo" "log-setting" 'setting)
(defalias 'oo-log-setting 'oo--log-advice)
(defun! oo-log-setting (var value)
  "Log a setting."
  (let! svalue (s-truncate 40 (format "%S" value)))
  (oo--log-setting "%s -> %s" var svalue))

(defvar oo-unbound-symbol-alist nil
  "An alist mapping an unbound symbol to an expression.
This alist is checked by the hook `after-load-functions&set-bound-symbols' for
any symbols that are now bound.")

(defmacro! set! (symbol value)
  "A \"do-it-all\" setter for configuring variables."
  (let! value-var (make-symbol "value"))
  `(if (not (boundp ',symbol))
       (push (cons ',symbol ',value) oo-unbound-symbol-alist)
     (let ((,value-var ,value))
       (oo-log-setting ',symbol ,value-var)
       (aif (get ',symbol 'custom-set)
	       (funcall it ',symbol ,value-var)
	     (setq ,symbol ,value-var)))))

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

(defun! oo-try-load-feature (fn)
  "Try to load feature that could contain FN."
  (unless (fboundp fn)
    (dolist (feature (oo-candidate-features fn))
      (require feature)
      (when (fboundp fn)
        (return! feature)))))

(adding-to-list! log4e-log-level-alist '(after-load . 3))
(log4e--def-level-logger "oo" "log-after-load" 'after-load)
(defalias 'oo-log-after-load 'oo--log-after-load)
(defun oo-log-after-load (fn args)
  "Log an `after-load!' form."
  (cond ((symbolp fn)
	     (oo--log-after-load (s-truncate 70 (format "%S" (cons fn args)))))
	    (t
	     (oo--log-after-load "(anonymous-after-block)"))))

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

(defun! oo-call-after-load-fn (fn args)
  "Return function to be used for `oo-call-after-load' based on FN.
The returned function calls FN with args ignoring any resulting errors.
Additionally, it does nothing and returns nil on any calls after its first call.
Finally, logs into the log buffer using `oo-log-after-load'."
  (let! fname (gensym "eval-after-load-fn-"))
  (let! closure (eval `(let ((first-call-p t))
			             (lambda (&rest _)
			               (when first-call-p
			                 (setq first-call-p nil)
			                 (oo-log-after-load #',fn ',args)
			                 (ignore-errors (apply #',fn ',args)))))
		              t))
  (fset fname closure)
  fname)

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

(defmacro! after! (condition &rest body)
  "Eval BODY after CONDITION is met.
For what CONDITION is see `oo-call-after-load'."
  (declare (indent 1))
  `(oo-call-after-load ',condition (lambda () ,@body)))

(defmacro! defafter! (&rest args)
  "A wrapper around `after!' With same syntax as `defun'."
  (declare (indent defun))
  (let! (_ args _ body) (oo-defun-components args))
  `(after! ,args ,@body))

(adding-to-list! log4e-log-level-alist '(hook . 3))
(log4e--def-level-logger "oo" "log-hook" 'hook)
(defun oo-log-hook (_ function &rest _)
  (oo--log-hook "%s" function))

(defun! oo-generate-hook-name (hook function args)
  "Return a hook name based on HOOK, FUNCTION and ARGS."
  (let! prefix hook)
  (alet (pcase function
	      ((pred listp)
	       (gensym "anonymous-hook-"))
	      ((guard (aand args (-all-p (-orfn #'stringp #'symbolp #'numberp) args)))
	       ;; Combine function and args into a name if it's reasonable to do so.
	       (string-join (mapcar #'oo-args-to-string (cons function args)) "-"))
	      (_
	       function))
    (intern (format "%s&%s" prefix it))))

(defun oo-add-hook (symbols function &rest arglist)
  "Add new hooks generated from FUNCTIONS to SYMBOLS.
Unlike `add-hook', SYMBOLS and FUNCTIONS can be single items or lists.  EXPIRE is
the same as in `oo-hook-create'.  When EXPIRE is non-nil, each
function will remove itself from the hook it is in after it is run once.  If
EXPIRE is a function, call it on the return value in order to determine
whether to remove a function from the hook."
  (dolist (symbol (-list symbols))
    (apply #'oo-create-hook symbol function arglist)))

(defmacro! defhook! (&rest rest)
  "Define a hook and add it to SYMBOL and SYMBOLS.
DOCSTRING and BODY are the docstring and body (respectively) of the defined
hook.  Optionally, this macro takes key-value pairs passed in as either.

Optional keyword arguments:

- `:args' the arguments of the hook, `\(&rest)' by default.
- `:depth' - same as `depth' in `add-hook'.
- `:append' - same as `:depth'.
- `:local' - same as `local' in `add-hook'.
- `:expire' - a function that returns non-nil when the advice should be removed.

\(fn NAME (SYMBOL [SYMBOLS] [KEY VALUE]...) [DOCSTRING] [[KEY VALUE]...] [BODY])"
  (declare (indent defun) (doc-string 3))
  (let! (name symbols metadata plist body) (oo-destructure-defun-plus rest))
  (dolist (symbol symbols)
    (let! args (or (plist-get plist :args) '(&rest _)))
    (let! lambda `(lambda ,args ,@metadata (block! nil ,@body)))
    (let! fname (intern (format "%s+" name)))
    (collecting! forms `(oo-add-hook ',symbol #',lambda ,@plist :name ',fname)))
  (macroexp-progn forms))

(defun! oo-create-hook (symbol fn &rest arglist)
  "Add a hook at SYMBOL that behaves like FUNCTION.
DEPTH and LOCAL are the same as in `add-hook'.  When EXPIRE is non-nil, each
the new hook will remove itself from SYMBOL after it is run once.  If
EXPIRE is a function, call it on the return value of the new hook (which is will
be the same as the return value for FUNCTION) in order to determine whether to
remove a function from the hook."
  (let! args (take! (-not #'keywordp) arglist))
  (let! (&plist :append :depth :local :expire) arglist)
  (let! hook (oo-generate-hook-name symbol fn args))
  (fset hook
	`(lambda (&rest _)
	   ,@(when (symbolp fn) `((oo-try-load-feature #',fn)))
	   (oo-log-hook nil ',hook)
	   (prog1 (condition-case-unless-debug err
		      (apply #',fn ',args)
		    (error (oo-log-error "hook `%s' failed because %s -> %s" ',hook (car err) (cdr err))))
	     (when ,expire (remove-hook ',symbol ',hook)))))
  (add-hook symbol hook (or depth append) local))

;; (defalias 'oo-generate-hook 'oo-create-hook)
;; (defalias 'oo-gen-hook 'oo-create-hook)

(defun! oo-advice-create (&rest args)
  "Return a function that advises SYMBOL at PLACE.
PROPS is the same as in `advice-add'.  When EXPIRE is non-nil, each function will
remove itself as advice after it is run once.  If EXPIRE is a function, call it
on the return value in order to determine whether to remove a function as advice."
  (let! (symbol place function (&plist :name :expire :props)) args)
  (let! name (or name (if (symbolp function) function (gensym "anonymous-advice-"))))
  (let! new-advice (oo-args-to-symbol symbol '@ name))
  (fset new-advice
	    `(lambda (&rest args)
	       ,@(when (symbolp function) `((oo-try-load-feature #',function)))
	       (aprog1 (apply #',function args)
	         (when (and ',expire (or (not (functionp ',expire)) (funcall ',expire it)))
	           (advice-remove ',symbol ',new-advice)))))
  (advice-add symbol place new-advice props)
  new-advice)

(cl-defun oo-add-advice (symbols place function &key expire props)
  "SYMBOLS, WHERE, FUNCTIONS, and PROPS correspond to the arguments for
advice-add.  Unlike advice-add, SYMBOLS and FUNCTIONS can be single items or
lists.  When EXPIRE is non-nil, each function will remove itself as advice
after it is run once.  If EXPIRE is a function, call it on the return value in
order to determine whether to remove a function as advice."
  (dolist (symbol (-list symbols))
    (oo-advice-create symbol place function :expire expire :props props)))

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
    (pushing! forms `(oo-advice-create ',symbol ,(oo-args-to-keyword place) ,lambda ,@kargs)))
  (macroexp-progn (nreverse forms)))

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

(defconst oo-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst oo-normal-localleader-key "SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-normal-localleader-short-key ","
  "A shorter alternative `oo-localleader-key'.")

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

(defvar oo-initial-evil-key-bindings nil
  "A list of binding forms to be run after evil is loaded.")

(defvar oo-initial-evil-key-bindings-enabled-p nil
  "Whether forms in `oo-initial-evil-key-bindings-' have been evaluated.")

(defafter! evaluate-initial-evil-key-bindings (evil)
  "Evaluate forms in `oo-initial-evil-key-bindings'."
  (oo-log "Evaluating initial bindings...")
  (funcall `(lambda () (ignore-errors ,@oo-initial-evil-key-bindings)))
  (setq oo-initial-evil-key-bindings-enabled-p t))

(defhook! enable-deferred-key-binding-evaluation (emacs-startup-hook :depth 80)
  "Evaluate any deferred bindings.
Also, setup deferred binding evaluation in `after-load-functions'."
  (oo-eval-deferred-key-bindings-maybe)
  (oo-add-hook 'after-load-functions #'oo-eval-deferred-key-bindings-maybe))

(defvar oo-override-mode-map (make-sparse-keymap))

(define-minor-mode oo-override-mode
  "Global minor mode for higher precedence evil keybindings."
  :keymap oo-override-mode-map
  :global t)

(oo-add-hook 'after-init-hook #'oo-override-mode :depth -100)

(defun evil-mode-hook&make-intercept-map ()
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map oo-override-mode-map 'all t))

(oo-add-hook 'evil-mode-hook #'evil-mode-hook&make-intercept-map)

(pushing! emulation-mode-map-alists '((oo-override-mode . oo-override-mode-map)))

(defvar oo-deferred-key-bindings nil
  "An alist with elements of (KEYMAP . FORMS).
KEYMAP is the name of a keymap.  FORMS are a list of lisp forms that should be
evaluated when KEYMAP is bound.")

(defun! oo-eval-deferred-key-bindings-maybe (&rest _)
  "Evaluate any binding forms whose keymap has been loaded.
Evaluate forms from all elements of `oo-deferred-key-bindings' whose
KEYMAP is bound \(the elements of the form \(KEYMAP . FORMS\)\).  Additionally,
remove those elements from `oo-deferred-key-bindings'."
  (for! ((item &as map . forms) oo-deferred-key-bindings)
    (cond ((boundp map)
	   (pushing! forms `(oo--log-info "Evaluating %s bindings..." ',map))
	   (appending! body forms))
	  (t
	   (pushing! to-keep item))))
  (setq oo-deferred-key-bindings to-keep)
  (funcall `(lambda () ,@body)))

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo/leader-prefix-command 'oo-leader-map)

(defun! oo-bind-macro-handle-evil-bind (token interpreters)
  "Return form that performs evil binding."
  (let! state (map-elt token :state))
  (let! key (map-elt token :key))
  (let! map (map-elt token :map))
  (let! def (map-elt token :def))
  (let! minor-mode (map-elt token :minor-mode))
  (cond ((or (member state '(nil global)) (not key) (not (map-contains-key token :def)))
	 (oo-bind-macro-handle-token token interpreters))
	(minor-mode
	 `((evil-define-minor-mode-key ',state ',minor-mode ,key ,def)
	   ,@(oo-bind-macro-handle-token token interpreters)))
	(t
	 `((evil-define-key* ',state ,map ,key ,def)
	   (oo-log-keybinding ',map ,key ,def ',state)
	   ,@(oo-bind-macro-handle-token token interpreters)))))

(defvar oo-bind-macro-state-to-localleaders
  `((oo-normal-localleader-short-key . normal)
    (oo-normal-localleader-key . normal)
    (oo-insert-localleader-key . insert)
    (oo-insert-localleader-short-key . insert)
    (oo-emacs-localleader-key . emacs)
    (oo-emacs-localleader-short-key . emacs)
    (oo-emacs-localleader-key . global)
    (oo-emacs-localleader-short-key . global))
  "List of localleaders to state.")

(defun! oo-bind-macro-handle-init-evil-binds (token interpreters)
  (let! state (map-elt token :state))
  (let! forms (oo-bind-macro-handle-token token interpreters))
  (if (member state '(nil global))
      forms
    `((cond ((bound-and-true-p oo-initial-evil-key-bindings-enabled-p)
	     ,@forms)
	    (t
	     (appending! oo-initial-evil-key-bindings ',forms))))))

(defun! oo-bind-macro-handle-after-evil-maybe (token interpreters)
  (let! state (map-elt token :state))
  (if (not (member state '(global nil)))
      `((after! evil ,@(oo-bind-macro-handle-token token interpreters)))
    (oo-bind-macro-handle-token token interpreters)))

(defun! oo-bind-macro-handle-alt (token interpreters)
  "Remap uses of command to alternate command."
  (let! (command . alt) (map-elt token :alt))
  (let! condition (or (map-elt token :when) t))
  (cond ((and alt condition command)
	 (let! hook (oo-args-to-symbol 'oo-bind-hook-for- command))
	 (let! filter-fn (oo-args-to-symbol 'oo-bind-filter-for- command))
	 (let! key-var (gensym "key-"))
	 (let! def-var (gensym "def-"))
	 (setq token (map-insert token :def def-var))
	 (setq token (map-insert token :key key-var))
	 (unless (map-elt token :map)
	   (setq token (map-insert token :map 'global-map)))
	 `((setq ,def-var '(menu-item "" nil :filter ,filter-fn))
	   (setq ,key-var [remap ,command])
	   (unless (boundp ',hook)
	     (defvar ,hook '((t ,command)))
	     (defun ,filter-fn (&rest _)
	       (run-hook-with-args-until-success ',hook))
	     (defhook! ,(oo-args-to-symbol 'run- command '-maybe) (,hook)
	       (when t #',command)))
	   (defhook! ,(oo-args-to-symbol 'run- alt '-maybe) (,hook)
	     (when ,condition #',alt))
	   ,@(oo-bind-macro-handle-token token interpreters)))
	(t
	 (oo-bind-macro-handle-token token interpreters))))

(defvar oo-bind-macro-state-alist
  '((?n . normal) (?i . insert) (?e . emacs)
    (?v . visual) (?m . motion) (?o . operator)
    (?r . replace) (?g . global))
  "An alist mapping the first character of an evil state to the state.")

(defun! oo-bind-macro-evil-keyword-to-states (evil-keyword)
  "Return list of evil states specified by EVIL-KEYWORD.
EVIL-KEYWORD is a keyword whose letters correspond to the first letters of evil
states (e.g. :i \"i\" would correspond to \"insert\" state).
If any letter in EVIL-KEYWORD does not correspond to an evil state, return nil."
  (let! state 'dummy)
  (let! string (seq-rest (symbol-name evil-keyword)))
  (while (and (not (string-empty-p string)) state)
    (let! char (seq-first string))
    (updating! string #'seq-rest)
    (let! state (alist-get char oo-bind-macro-state-alist))
    (adjoining! states state))
  (and state (nreverse states)))

(defun! oo-bind-macro-handle-concat-prefix (token interpreters)
  "Return form that concatenates prefix to key."
  (let! prefix (map-elt token :prefix))
  (let! key (map-elt token :key))
  (cond (prefix
	 `((setq ,key (concat ,prefix "\s" ,key))
	   ,@(oo-bind-macro-handle-token token interpreters)))
	(t
	 (oo-bind-macro-handle-token token interpreters))))

(defvar oo-bind-macro-handlers
  '(oo-bind-macro-handle-localleader
    oo-bind-macro-handle-defer-if-map-unbound
    oo-bind-macro-handle-init-evil-binds
    oo-bind-macro-handle-after-evil-maybe
    oo-bind-macro-handle-stop-var-leaks
    oo-bind-macro-handle-alt
    oo-bind-macro-handle-autoload
    oo-bind-macro-handle-concat-prefix
    oo-bind-macro-handle-which-key
    oo-bind-macro-handle-kbd
    oo-bind-macro-handle-evil-bind
    oo-bind-macro-handle-bind)
  "List of INTERPRETERS that convert a token to lisp forms.
A INTERPRETER is a function with two arguments, a token and a list of
interpreters.  A INTERPRETER should return a list of forms.
See `bind!'.")

(defun! oo-bind-macro-handle-which-key (token interpreters)
  "Return form that registers binding specified by TOKEN with `which-!key'."
  (let! key (map-elt token :key))
  (let! wk (or (map-elt token :wk) (map-elt token :which-key)))
  (let! state (map-elt token :state))
  (let! map (alet (map-elt token :map)
	      (if (not (member state '(nil global)))
		  `(evil-get-auxiliary-keymap ,it ',state)
		it)))
  (let! wk-fn 'which-key-add-keymap-based-replacements)
  (let! forms (oo-bind-macro-handle-token token interpreters))
  (let! wk-prefix (map-elt token :wk-prefix))
  (let! prefix (map-elt token :prefix))
  (cond ((and map key wk)
	 (pushing! forms `(oo-call-after-load '(which-key evil) #',wk-fn ,map ,key ,wk)))
	((and map key wk)
	 (pushing! forms `(oo-call-after-load 'which-key #',wk-fn ,map ,key ,wk))))
  (when (and prefix wk-prefix)
    (pushing! forms `(oo-call-after-load 'which-key #',wk-fn ,map ,prefix ,wk-prefix)))
  forms)

(defun! oo-bind-macro-merge-token (token)
  "Join necessary values for token."
  (let! done nil)
  (for! ((key . values) token)
    (pcase key
      (:prefix
       (updating! (map-elt done key) (lambda (x) (list (string-join (append x values) "\s")))))
      (_
       (appending! (map-elt done key) values))))
  (nreverse done))

(defun! oo-bind-macro-handle-bind (token interpreters)
  "Define a binding using `define-key'."
  (let! map (map-elt token :map))
  (let! key (map-elt token :key))
  (let! def (map-elt token :def))
  (let! state (map-elt token :state))
  (if (and map key (map-contains-key token :def) (member state '(nil global)))
      `((oo-log-keybinding ',map ,key ,def)
	(define-key ,map ,key ,def)
	,@(oo-bind-macro-handle-token token interpreters))
    (oo-bind-macro-handle-token token interpreters)))

(defun! oo-bind-macro-handle-kbd (token interpreters)
  "Apply kbd to key if key is a string."
  (let! key (map-elt token :key))
  (let! forms (oo-bind-macro-handle-token token interpreters))
  (if key
      (cons `(setq ,key (if (stringp ,key) (kbd ,key) ,key)) forms)
    forms))

(defun! oo-bind-macro-handle-stop-var-leaks (token interpreters)
  "Prevent variable leakage from evaluation save variables."
  (let! key (map-elt token :key))
  (when key
    (let! key-var (gensym "key-"))
    (collecting! let-binds (list key-var key))
    (setq token (map-insert token :key key-var)))
  (let! def (map-elt token :def))
  (when def
    (let! def-var (gensym "def-"))
    (collecting! let-binds (list def-var def))
    (setq token (map-insert token :def def-var)))
  (if let-binds
      `((let ,let-binds ,@(oo-bind-macro-handle-token token interpreters)))
    (oo-bind-macro-handle-token token interpreters)))

(defun! oo-bind-macro-handle-token (token interpreters)
  "Return a list of forms created from applying INTERPRETERS to TOKEN.
INTERPRETERS are a list of function symbols that are applied to TOKEN."
  (let! (interpreter-fn . rest) interpreters)
  (funcall (or interpreter-fn #'ignore) token rest))

(defun! oo-bind-macro-handle-autoload (token interpreters)
  "Autoload binding definition."
  (let! def (map-elt token :def))
  (let! autoload (map-elt token :autoload))
  (cond ((and def autoload)
	 `((when (and (symbolp ,def) (not (member ,def '(t nil))))
	     (setq ,def (oo-autoload-fn ,def ',autoload)))
	   ,@(oo-bind-macro-handle-token token interpreters)))
	(t
	 (oo-bind-macro-handle-token token interpreters))))

(defun! oo-bind-macro-handle-defer-if-map-unbound (token interpreters)
  "Return a form that defers keybinding forms until keymap is unbound."
  (when (and (not (map-contains-key token :map)) (map-contains-key token :key))
    (setq token (map-insert token :map 'global-map)))
  (let! map (map-elt token :map))
  (let! forms (oo-bind-macro-handle-token token interpreters))
  (if (and map (symbolp map) (not (member map '(global-map))))
      `((if (boundp ',map)
	    (progn ,@forms)
	  (appending! (map-elt oo-deferred-key-bindings ',map) ',forms)))
    forms))

(defun! oo-bind-macro-tokenize-clause (clause)
  "Return a list of tokens generated from CLAUSES."
  (dolist (raw (oo-bind-macro-generate-raw-tokens clause))
    (let! merged (oo-bind-macro-merge-token raw))
    (appending! tokens (oo-bind-macro-separate merged)))
  tokens)

(defun! oo-bind-macro-separate (raw-tokens)
  "Divide tokens such that each token has only one."
  (let! heads (list nil))
  (while raw-tokens
    ;; (let! (pair &as key . values) (pop raw-tokens))
    (let! pair (pop raw-tokens))
    (let! (key . values) pair)
    (let! new-heads nil)
    (for! (value values)
      (for! (head heads)
	(pushing! new-heads (cons (cons key value) head))))
    (let! heads new-heads))
  (mapcar #'reverse heads))

(defun! oo-bind-macro-get-tokenites (clause)
  "Return \(TOKENITES CLAUSE) from CLAUSE.
Tokenites is a list of token attributes."
  (let! tokenites
	(pcase clause
	  (`(:minor-mode ,(pred symbolp) . ,(guard t))
	   (list (list (pop clause) (pop clause))))
	  (`(:alt ,(pred symbolp) ,(pred symbolp) . ,(guard t))
	   (list (list (pop clause) (cons (pop clause) (pop clause)))))
	  (`(:state ,(pred oo-non-keyword-symbol-p) . ,(guard t))
	   (list (cons (pop clause) (pop! clause #'oo-non-keyword-symbol-p))))
	  (`(,(or :map :localleader) ,(pred oo-non-keyword-symbol-p) . ,(guard t))
	   (list (cons (pop clause) (pop! clause #'oo-non-keyword-symbol-p))))
	  (`(,(pred stringp) . nil)
	   (list (list :key (pop clause)) (list :def nil)))
	  (`(:wk ,(pred stringp) :prefix ,(pred stringp) . nil)
	   (pop clause)
	   (list (list :wk-prefix (pop clause))
		 (list (pop clause) (pop clause))))
	  (`(:wk ,(pred stringp) . ,(guard t))
	   (list (list (pop clause) (pop clause))))
	  (`(,(or :when :def :prefix) ,_ . ,(guard t))
	   (list (pop! clause 2)))
	  (`(,(or (pred stringp) (pred vectorp) (pred symbolp)) (function ,(pred symbolp)) . ,(guard t))
	   `((:key ,(pop clause)) (:def ,(pop clause))))
	  (`(,(or (pred stringp) (pred vectorp) (pred symbolp)) ,(pred symbolp) . nil)
	   `((:key ,(pop clause)) (:def ,(pop clause))))
	  (`(:key ,_ . ,(guard t))
	   (let! tokenites (list (pop! clause 2)))
	   (cond ((equal :def (car clause))
		  (affixing! tokenite (list (pop clause) (pop clause))))
		 ((keywordp (car clause))
		  (affixing! tokenites (list :def nil)))
		 (t
		  (affixing! tokenites (list :def (pop clause)))))
	   tokenites)
	  (`(,(and (pred keywordp) (app oo-bind-macro-evil-keyword-to-states states)) ,_ ,_ . ,(guard t))
	   (pop clause)
	   (list (cons :state states) (list :key (pop clause)) (list :def (pop clause))))
	  (`(,(or (pred stringp) (pred vectorp)) :wk . ,(guard t))
	   (list (list :key (pop clause)) (list :def nil) (list :ignore t)))
	  (_
	   (error "No matcher for `%S' of `%S'" (car clause) clause))))
  (list tokenites clause))

(defun oo-bind-macro-keybinding-p (token)
  "Return non-nil if TOKEN is a binding token."
  (or (and (assoc :def token) (assoc :key token)) (assoc :alt token)))

(defun! oo-bind-macro-generate-raw-tokens (clause)
  "Return list of tokens specified by CLAUSE."
  (let! stack (list (list clause nil 0)))
  (while stack
    (pcase (car stack)
      (`(nil nil ,level)
       (let! tokenites (mapcar #'-second-item (pop! leaves (lambda (x) (= (1+ level) (car x))))))
       (let! (bindings settings) (-separate #'oo-bind-macro-keybinding-p tokenites))
       (let! settings (reverse (apply #'append settings)))
       (let! new-leaves (mapcar (lambda (x) (list level (append settings x))) bindings))
       (prepending! leaves new-leaves)
       (pop stack))
      (`(nil ,tokenites ,level)
       (pushing! leaves (list level tokenites))
       (pop stack))
      (`((,(and (pred listp) car) . ,(and (guard t) cdr)) ,_ ,_)
       (setf (nth 0 (car stack)) cdr)
       (pushing! stack (list car nil (length stack))))
      (`(,(and (pred listp) clause) ,_ ,_)
       (let! (tokenites updated) (oo-bind-macro-get-tokenites clause))
       (setf (nth 0 (car stack)) updated)
       (appending! (nth 1 (car stack)) tokenites))
      (_
       (error "Unknown case `%S'" (car stack)))))
  (nreverse (mapcar #'cadr leaves)))

(adjoining! log4e-log-level-alist '(keybinding . 3))
(log4e--def-level-logger "oo" "log-keybinding" 'keybinding)
(defalias 'oo-log-keybinding 'oo--log-keybinding)
(defun oo-log-keybinding (key map def &optional state)
  (cond (state
	 (oo--log-keybinding "%s %s %s @ %s" key map def state))
	(t
	 (oo--log-keybinding "%s %s %s" key map def))))

(defun! oo-bind-macro-handle-localleader (token interpreters)
  "Return form that processes localleader."
  (let! localleader (map-elt token :localleader))
  (let! prefix (map-elt token :prefix))
  (cond (localleader
	 (for! ((leader . state) oo-bind-macro-state-to-localleaders)
	   (let ((token token))
	     (let! new-prefix (if prefix `(concat ,leader "\s" ,prefix) leader))
	     (updating! token #'map-insert :prefix new-prefix)
	     (updating! token #'map-insert :map localleader)
	     (updating! token #'map-insert :state state)
	     (appending! forms (oo-bind-macro-handle-token token interpreters))))
	 forms)
	(t
	 (oo-bind-macro-handle-token token interpreters))))

(defmacro! bind! (&rest clause)
  "An extensible macro for binding keys."
  (for! (token (oo-bind-macro-tokenize-clause clause))
    (appending! forms (oo-bind-macro-handle-token token oo-bind-macro-handlers)))
  (macroexp-progn forms))

(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo/window-prefix-command 'oo-window-map)
(bind! (:map oo-leader-map)
       (:wk "window" "w" #'oo/window-prefix-command))

(bind! (:map oo-window-map)
       ("M" #'maximize-window)
       ("m" #'minimize-window)
       ("b" #'balance-windows)
       ("z" #'zoom-window-zoom))

(bind! (:map oo-window-map)
       (:wk "left"         "h" #'windmove-left)
       (:wk "down"         "j" #'windmove-down)
       (:wk "up"           "k" #'windmove-up)
       (:wk "right"        "l" #'windmove-right)
       (:wk "ace"          "o" #'ace-window)
       (:wk "delete"       "d" #'delete-window)
       (:wk "delete others""D" #'delete-other-windows)
       (:wk "vsplit"       "v" #'split-window-horizontally)
       (:wk "split"        "s" #'split-window-vertically)
       (:wk "vsplit+focus" "V" #'oo/split-window-right-and-focus)
       (:wk "split+focus"  "v" #'oo/split-window-below-and-focus)
       (:wk "transpose"    "t" #'transpose-frame))

(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo/buffer-prefix-command 'oo-buffer-map)
(bind! (:map oo-leader-map)
       (:wk "buffer" "b" #'oo/buffer-prefix-command))

(bind! (:map oo-buffer-map)
       ("w" #'save-buffer)
       ("x" #'buffer-expose)
       ((:wk "kill" :prefix "k")
	("c" #'kill-current-buffer))
       ("j" #'pop-to-buffer)
       ("b" #'switch-to-buffer)
       ("n" #'next-buffer)
       ("p" #'previous-buffer)
       ("d" #'display-buffer))

(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo/quit-prefix-command 'oo-quit-map)
(bind! (:map oo-leader-map)
       (:wk "quit" "q" #'oo/quit-prefix-command))

(defvar oo-quick-map (make-sparse-keymap))
(define-prefix-command 'oo/quick-prefix-command 'oo-quick-map)
(bind! (:map oo-leader-map)
       (:wk "quick" "j" #'oo/quick-prefix-command))

(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo/app-prefix-command 'oo-app-map)
(bind! (:map oo-leader-map)
       (:wk "app" "a" #'oo/app-prefix-command))

(defvar oo-toggle-map (make-sparse-keymap))
(define-prefix-command 'oo/toggle-prefix-command 'oo-toggle-map)
(bind! (:map oo-leader-map "t" #'oo/toggle-prefix-command))

(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo/help-prefix-command 'oo-help-map)
(bind! (:map oo-leader-map "h" #'oo/help-prefix-command))

(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(bind! (:map oo-leader-map)
       (:wk "package" "p" #'oo/package-prefix-command))

(bind! (:map oo-quit-map)
       (:wk "quit emacs" "q" #'save-buffers-kill-emacs))

(bind! (:map oo-quit-map)
       (:wk "quit and restart" "r" #'restart-emacs))

;; Note that this can't work with `on-first-input-hook' because which-key
;; doesn't happen on first keypress.  It needs to be in the startup hook.
(oo-add-hook 'emacs-startup-hook #'which-key-mode)

(set! which-key-sort-uppercase-first nil)
(set! which-key-max-display-columns nil)
(set! which-key-add-column-padding 1)
(set! which-key-min-display-lines 6)
(set! which-key-side-window-slot -10)
(set! which-key-sort-order #'which-key-prefix-then-key-order)
(set! which-key-popup-type 'side-window)
(set! which-key-idle-delay 0.8)

;; (set! line-spacing 3 :hook which-key-init-buffer-hook :local t)

(set! which-key-show-transient-maps t)
(set! which-key-show-operator-state-maps t)

(bind! (:map oo-override-mode-map)
       (:g   oo-emacs-leader-key  #'oo/leader-prefix-command)
       (:i   oo-insert-leader-key #'oo/leader-prefix-command)
       (:nmv oo-normal-leader-key #'oo/leader-prefix-command))

(oo-add-hook 'emacs-startup-hook #'idle-require-mode :append t)

(set! idle-require-load-break 5)
(set! idle-require-idle-delay 10)

(set! gcmh-high-cons-threshold (* 8 1024 1024))
(set! gcmh-low-cons-threshold (* 4 1024 1024))

(set! gcmh-idle-delay 'auto)

(oo-add-hook 'emacs-startup-hook #'gcmh-mode :depth 91)

(defvar oo-initial-buffer-choice-hook nil
  "Hook run to choose initial buffer.
Each hook should return either a buffer to be displayed or a boolean.
For what buffer is displayed in the case of a boolean see
`initial-buffer-choice'.")

(defun oo-run-initial-buffer-choice-hook ()
  "Run `oo-initial-buffer-choice-hook'."
  (aprog1 (or (run-hook-with-args-until-success 'oo-initial-buffer-choice-hook)
	      (get-buffer-create "*scratch*"))
    (oo-log-info "set initial buffer to %s" (buffer-name))))

(setq initial-buffer-choice #'oo-run-initial-buffer-choice-hook)

(defhook! minibuffer-setup-hook&boost-garbage-collection ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold"
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defhook! minibuffer-exit-hook&defer-garbage-collection (minibuffer-exit-hook :append t)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold gcmh-low-cons-threshold))

(set! helm-candidate-number-limit 50)

(oo-popup-at-bottom "\\*Helm")

(bind! (:map helm-map)
       (:ie "TAB" #'helm-next-line)
       (:ie "C-j" #'helm-next-line)
       (:ie "C-k" #'helm-previous-line))

(bind! (:map helm-map)
       (:ie "C-a" #'helm-select-action)
       (:ie "C-m" #'helm-toggle-visible-mark-forward)
       (:ie "RET" (lambda () (interactive) (funcall #'helm-select-nth-action 0)))
       ;; This binding has a problem.  (:ie "C-i" #'helm-toggle-visible-mark-backward)
       (:ie "S-TAB" #'helm-mark-current-line))

(bind! (:when (bound-and-true-p helm-mode))
       (:alt oo/set-font-face helm-select-xfont)
       (:alt apropos                  helm-apropos)
       (:alt find-library             helm-locate-library)
       (:alt execute-extended-command helm-M-x)
       (:alt find-file                helm-find-files)
       (:alt locate                   helm-locate)
       (:alt imenu                    helm-semantic-or-imenu)
       (:alt noop-show-kill-ring      helm-show-kill-ring)
       (:alt recentf                  helm-recentf)
       (:alt switch-to-buffer         helm-mini))

(set! consult-preview-key nil)

(oo-add-hook 'emacs-startup-hook #'recentf-mode)

(set! recentf-max-menu-items 0)
(set! recentf-max-saved-items 700)
(set! recentf-save-file (concat oo-cache-dir "recentf"))
(set! recentf-auto-cleanup (* 60 10))
(set! recentf-filename-handlers '(file-truename))

(oo-add-advice #'recentf-save-list :before #'recentf-cleanup)
(oo-add-hook 'kill-emacs-hook #'recentf-save-list)

(oo-silence #'recentf-mode #'recentf-cleanup #'recentf-save-list)

(oo-add-hook 'vertico-mode-hook #'marginalia-mode)
(when (display-graphic-p)
  (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode))
;; Declaratively add hooks.
;; (oo-add-hook 'marginalia-mode-hook #'all-the-icons-completion-mode :when #'display-graphic-p)

(set! crm-separator ",")

(set! vertico-quick1 "abcdefghijklmnopqrstuvwxyz")

(set! vertico-count-format '("%-6s " . "%2$s"))
(set! vertico-count 15)

(oo-add-hook 'on-first-input-hook #'vertico-mode)

(oo-add-hook 'vertico-mode-hook #'vertico-buffer-mode)

(oo-popup-at-bottom "\\*Vertico")

(defhook! enable-orderless (vertico-mode-hook :expire t)
  (when (require 'orderless nil t)
    (setq completion-styles '(orderless))
    (setq completion-category-defaults nil)
    (setq completion-category-overrides '((file (styles partial-completion))))
    (alet '(orderless-strict-leading-initialism orderless-initialism orderless-regexp)
      (set! orderless-matching-styles it))))

(defun oo-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))
(set! dashboard-init-info #'oo-dashboard-init-info)

(set! dashboard-banner-logo-title "Welcome!")
(set! dashboard-set-footer nil)
(set! dashboard-items nil)
(set! dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))
(set! dashboard-center-content t)
;; (set! dashboard-banner-logo-title "Welcome!")

(defhook! create-dashboard (oo-initial-buffer-choice-hook)
  (when (require 'dashboard nil t)
    (aprog1 (get-buffer-create dashboard-buffer-name)
      (with-current-buffer it
	(dashboard-insert-startupify-lists)))))

(defun oo-set-window-divider-face (&rest _)
  "Set the window divider face."
  (set-face-foreground 'window-divider "black"))

(oo-add-hook 'after-init-hook #'oo-set-window-divider-face :depth 11)

(oo-add-advice #'load-theme :after #'oo-set-window-divider-face)

(oo-add-hook 'after-init-hook #'window-divider-mode :depth 12)

(set! window-divider-default-bottom-width 7)
(set! window-divider-default-right-width 7)

(setq window-divider-default-places t)

(oo-add-hook 'after-init-hook #'load-theme 'modus-operandi)

(set! lambda-themes-set-italic-comments nil)
(set! lambda-themes-set-italic-keywords nil)

(set! lambda-themes-set-variable-pitch nil)

(set! modus-themes-headings '((1 . (rainbow light 1.4))
			                  (2 . (rainbow light 1.3))
			                  (3 . (rainbow light 1.2))
			                  (4 . (rainbow light 1.1))
			                  (t . (rainbow light 1.0))))

(set! mu4e-compose-signature-auto-include t)
(set! mu4e-compose-format-flowed t)

(set! mu4e-headers-auto-update t)

(bind! (:map dired-mode-map)
       (:nm "h" #'dired-up-directory))

(oo-add-hook 'dired-mode-hook #'dired-omit-mode)

(set! dired-recursive-copies 'always)
(set! dired-recursive-deletes 'always)

(oo-call-after-load 'dirvish #'dirvish-override-dired-mode 1)

(set! dirvish-use-mode-line nil)

(set! dirvish-attributes '(file-size all-the-icons subtree-state))

(bind! (:map oo-app-map "d" #'dired))

(bind! (:alt dired dirvish))

(set! dirvish-default-layout nil)

(set! idle-require-symbols (append '(em-alias em-banner em-basic em-cmpl em-glob em-hist em-ls em-prompt em-term em-unix)
                                   idle-require-symbols))

(oo-popup-at-bottom "\\*eshell")

(set! eshell-directory-name (concat oo-cache-dir "eshell/"))
(set! eshell-history-file-name (concat eshell-directory-name "history"))

(set! eshell-banner-message "")

(set! eshell-hist-ignoredups t)

(set! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(set! emms-directory (expand-file-name "emms/" oo-cache-dir))

(oo-call-after-load 'emms #'require 'emms-player-mpv)
(set! emms-player-list '(emms-player-mpv))

(oo-popup-at-bottom "\\*[Hh]elp")

(set! org-auto-tangle-default t)

(defun! +org/choose-capture-template ()
  "Choose a capture template."
  (interactive)
  (dolist (template org-capture-templates)
    (let! (keys name) template)
    (collecting! alist (cons name keys)))
  (let! selected (completing-read "capture template: " alist nil t))
  (org-capture nil (alist-get selected alist nil nil #'string=)))

(oo-popup-at-bottom "CAPTURE[^z-a]+")

(oo-popup-at-bottom "\\*Org Src")

(gv-define-expander org-ml-headline-get-section
  (lambda (do place)
    (gv-letplace (getter setter) place
      (funcall do `(org-ml-headline-get-section ,getter)
               (lambda (v)
                 (macroexp-let2 nil v v
                   `(progn
                      ,(funcall setter `(org-ml-headline-set-section ,v ,getter)))))))))

(bind! (:map org-mode-map)
       (:n "C" #'org-refile-copy))

(set! org-refile-target-verify-function (lambda () (not (+org-has-src-block-p))))

(defun! +org-has-src-block-p ()
  "Return non-nil if current headline has a source block."
  (save-excursion
    (let! beg (point))
    (let! end (or (outline-next-heading) (point-max)))
    (goto-char beg)
    (and (save-match-data (re-search-forward "^#\\+begin_src" end t)) t)))

(set! org-superstar-leading-bullet ?\s)
(set! org-superstar-special-todo-items t)
(set! org-superstar-headline-bullets-list '("✖" "✚" "▶" "◉" "○"))
(oo-add-hook 'org-mode-hook #'org-superstar-mode)

(set! org-refile-use-outline-path 'file)

(defun +org-directory-files ()
  "Return a list of org files in the `org-directory'."
  (directory-files org-directory t "\\.org\\'"))

(set! org-directory (expand-file-name "~/dotfiles"))

(set! org-archive-location (concat org-directory "archive.org::"))

(set! org-default-notes-file null-device)

(set! org-refile-use-cache nil)

(setq org-outline-path-complete-in-steps nil)

(set! org-refile-targets '((+org-directory-files :maxlevel . 10)))

(set! org-archive-save-context-info nil)

(set! org-refile-allow-creating-parent-nodes t)

(set! org-src-preserve-indentation t)
(set! org-edit-src-content-indentation 0)

(set! org-src-ask-before-returning-to-edit-buffer nil)

(defun! +org/dwim-next-visible-heading ()
  (interactive)
  (+org/dwim-previous-visible-heading t))

(defun! +org/dwim-previous-visible-heading (&optional next)
  (interactive)
  (let! case-fold-search nil)
  (let! regexp "^\\*+\\(?: +[[:upper:]]+\\)?\\(?: +\\[#.]\\)?\\(?: +\\(.*?\\)\\)?")
  (if next
      (org-next-visible-heading 1)
    (org-previous-visible-heading 1))
  (org-back-to-heading)
  (save-match-data
    (looking-at regexp)
    (goto-char (match-beginning 1))))

(defun +org/disable-tangling ()
  "Disable tangling source block at point."
  (interactive)
  (org-entry-put (point) "HEADER-ARGS" ":tangle no"))

(set! org-babel-default-header-args
      '((:session . "none")
        (:results . "silent")
        (:exports . "code")
        (:mkdirp  . "yes")
        (:cache   .  "no")
        (:noweb   .  "no")
        (:hlines  .  "no")
        (:tangle  .  "no")))

(set! org-tags-column 80)

(defun! +org/open-heading-above (&optional below)
  "Insert a heading above the current heading."
  (interactive)
  (ignore-errors (org-back-to-heading))
  (let! level (if (org-at-heading-p)
		          (car (org-heading-components))
		        1))
  (when (and below (org-at-heading-p))
    (or (outline-next-heading) (org-end-of-subtree)))
  (let->>! headline
    (org-ml-build-headline! :level level)
	(org-ml-headline-set-node-property "ID" (org-id-new)))
  (org-ml-insert (point) headline)
  (run-hooks 'org-insert-heading-hook))

(defun +org/open-heading-below ()
  (interactive)
  (+org/open-heading-above t))

(defun! +org/dwim-insert-src-block ()
  "Insert source block for the current headline if it does not already exist."
  (interactive)
  (let! lang (completing-read "Language: " (mapcar 'car org-src-lang-modes)))
  (let! headline (org-ml-parse-this-headline))
  (let! section (org-ml-headline-get-section headline))
  (when (--any-p (equal 'src-block (org-element-type it)) section) (return!))
  (let! src-block (org-ml-build-src-block :value "" :language lang))
  (snocing! (org-ml-headline-get-section headline) src-block)
  (org-ml-update-this-headline (-const headline)))

(defun! +org/dwim-edit-src-code ()
  (interactive)
  (mapc #'require '(edit-indirect org-ml))
  (unless (org-in-src-block-p) (org-next-block 1))
  (let! (beg end) (org-src--contents-area (org-ml-parse-this-element)))
  (let! parent-buffer (current-buffer))
  (edit-indirect-region beg end t))

(set! org-hide-emphasis-markers t)

(set! org-fontify-emphasized-text t)

(set! org-adapt-indentation nil)

(set! org-edit-src-persistent-message nil)

(defun! +org/update-tags ()
  "Update tags for the current entry."
  (interactive)
  (let->>! all
    (org-map-entries #'org-get-tags nil (+org-directory-files))
    (apply #'append)
    (-non-nil)
    (-uniq))
  (let! old (org-get-tags))
  (let! new (completing-read-multiple "Tags: " all))
  (let! updated (-uniq (append (-difference old new) (-difference new old))))
  (org-set-tags updated))

(set! org-id-track-globally t)
(set! org-id-locations-file (concat oo-cache-dir "org-id-locations"))

(set! org-id-method 'ts)

(set! org-src-window-setup 'plain)

(set! org-id-link-to-org-use-id t)

(defun! +org/add-tangle-header-arg ()
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

(bind! (:map org-mode-map)
       (:n "TAB" #'oo/toggle-fold-subtree))

(defun! oo/toggle-fold-subtree ()
  "Toggle folding of current subtree."
  (interactive)
  (let! folded-p (outline-invisible-p (line-end-position)))
  (outline-toggle-children)
  (save-excursion
    (next-line 1)
    (when (org-at-drawer-p)
      (org-fold-hide-drawer-toggle))))

(defun +org/dwim-eval-src-block ()
  "Eval block contents."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not in source block"))
  (save-window-excursion
    (org-babel-execute-subtree)))

(set! org-src-lang-modes (add-to-list 'org-src-lang-modes '("emacs-lisp" . emacs-lisp)))

(oo-add-hook 'org-insert-heading-hook #'evil-append 1)

(bind! (:map oo-toggle-map)
       ("f" #'oo/set-font-face))

(bind! (:map oo-app-map)
       ("E" #'restart-emacs-start-new-emacs))

(bind! ((:map oo-quick-map)
        (:wk "capture" "j" #'org-capture))
       ((:map oo-app-map)
        (:wk "capture" "a" #'org-capture)
        (:wk "capture" "j" #'org-capture)))

(bind! (:localleader org-mode-map)
       (:wk "execute subtree" "E" #'org-babel-execute-subtree)
       ("w" #'widen)
       (:wk "narrow" "n" #'org-narrow-to-subtree)
       (:wk "refile" "r" #'org-refile)
       (:wk "tangle" "t" #'org-babel-tangle))

(bind! (:map oo-leader-map)
       (:wk "eval" :prefix "e")
       ("e" #'eval-expression))

(bind! (:localleader emacs-lisp-mode-map)
       (:wk "eval" :prefix "e")
       ("r" #'lispy-eval-and-replace)
       ("b" #'eval-buffer)
       ("d" #'eval-defun)
       ("e" #'eval-expression)
       ("l" #'eval-last-sexp)
       ("p" #'eval-print-last-sexp))

(bind! (:map oo-app-map "e" #'eshell))

(bind! (:nm "+" #'text-scale-increase)
       (:nm "-" #'text-scale-decrease))

(bind! (:map oo-toggle-map)
       ("r" #'read-only-mode)
       ("t" #'load-theme)
       ("c" #'caps-lock-mode)
       ("r" #'redacted-mode)
       ("s" #'smartparens-mode)
       ("d" #'toggle-debug-on-error))

(bind! (:map oo-miscellany-map "k" #'bookmark-set))

(defvar oo-miscellany-map (make-sparse-keymap))
(define-prefix-command 'oo/miscellany-prefix-command 'oo-miscellany-map)
(bind! (:map oo-leader-map)
       (:wk "miscellany" "k" #'oo/miscellany-prefix-command))

(bind! (:map oo-help-map)
       ("i" #'info)
       ("m" #'describe-mode)
       ("f" #'describe-function)
       ("v" #'describe-variable)
       ("h" #'describe-variable)
       ("C" #'describe-char)
       ("k" #'describe-key)
       ("a" #'apropos)
       ("w" #'woman))

(bind! (:when (require 'helpful nil t))
       (:alt describe-function helpful-callable)
       (:alt describe-command helpful-command)
       (:alt describe-variable helpful-variable)
       (:alt describe-key helpful-key))

(bind! (:map vertico-map)
       (:state insert)
       ("TAB" #'vertico-next)
       ("C-k" #'vertico-previous)
       ("C-j" #'vertico-next)
       (";" #'vertico-quick-exit)
       ("C-;" #'vertico-quick-exit)
       ([backtab] #'vertico-previous))

(bind! ((:map vertico-map)
	(:i "C-o" #'embark-act)
	(:i "C-," #'embark-collect))
       (:n "C-o" #'embark-act))

(bind! (:map oo-leader-map "SPC" #'execute-extended-command)
       (:map oo-override-mode-map :nmv ";" #'execute-extended-command)
       (:i "A-x" #'execute-extended-command)
       (:i "M-x" #'execute-extended-command))

(bind! (:map org-mode-map)
       (:n "j" #'+org/dwim-next-visible-heading)
       (:n "k" #'+org/dwim-previous-visible-heading))

(bind! (:map org-mode-map)
       (:n "p" #'ignore))

(bind! (:map org-mode-map)
       (:n "Y" #'org-copy-subtree)
       (:n "D" #'org-cut-subtree)
       (:n "P" #'org-paste-subtree))

(bind! (:map org-mode-map)
       (:n "R" #'org-refile))

(bind! (:map org-mode-map)
       (:n "b" #'+org/dwim-insert-src-block))

(bind! (:map org-mode-map)
       (:n "o" #'+org/open-heading-below)
       (:n "O" #'+org/open-heading-above))

(oo-add-hook 'prog-mode-hook #'smartparens-strict-mode)
(oo-add-hook 'prog-mode-hook #'corfu-mode)

(set! org-confirm-babel-evaluate nil)

(bind! (:map org-mode-map)
       (:n "e" #'+org/dwim-eval-src-block))

(bind! (:map org-mode-map)
       (:n "t" #'+org/update-tags))

(bind! (:map org-mode-map)
       (:n ">" #'org-demote-subtree)
       (:n "<" #'org-promote-subtree))

(bind! (:map org-mode-map)
       (:n "K" #'org-metaup)
       (:n "J" #'org-metadown))

(bind! (:map oo-window-map)
       (:wk "left"  "h" #'windmove-left)
       (:wk "down"  "j" #'windmove-down)
       (:wk "up"    "k" #'windmove-up)
       (:wk "right" "l" #'windmove-right))

(bind! (:map oo-window-map)
       (:wk "delete"        "d" #'delete-window)
       (:wk "delete others" "D" #'delete-other-windows))

(bind! (:map oo-window-map)
       (:wk "vsplit"       "v" #'split-window-horizontally)
       (:wk "split"        "s" #'split-window-vertically)
       (:wk "vsplit+focus" "V" #'oo/split-window-right-and-focus)
       (:wk "split+focus"  "v" #'oo/split-window-below-and-focus))

(bind! (:n "gr" 'evil-operator-eval))

(bind! (:when (require 'eros nil t))
       (:alt eval-last-sexp eros-eval-last-sexp))

(bind! (:map evil-inner-text-objects-map "f" #'evil-cp-inner-form)
       (:map evil-outer-text-objects-map "f" #'evil-cp-a-form))

(bind! (:minor-mode org-src-mode)
       (:localleader org-src-mode-map)
       ("," #'org-edit-src-exit)
       ("a" #'org-edit-src-abort)
       ("c" #'org-edit-src-exit))

(defun! +org/dwim-edit-src-code ()
  "Edit nearest source block."
  (interactive)
  (unless (org-in-src-block-p) (org-next-block 1))
  (call-interactively #'org-edit-src-code))

(bind! (:alt org-edit-src-code +org/dwim-edit-src-code))

(bind! (:localleader org-mode-map)
       ("," #'org-edit-src-code)
       ((:wk "edit" :prefix "e")
	(:wk "source block" "s" #'org-edit-src-code)))

(bind! (:localleader emacs-lisp-mode-map)
       (:wk "macrostep" :prefix "m")
       ("e" #'macrostep-expand)
       ("c" #'macrostep-collapse)
       ("C" #'macrostep-collapse-all))

(oo-add-hook 'prog-mode-hook #'lispyville-mode)

(bind! (:map lispyville-mode-map)
       (:i "SPC" #'lispy-space)
       (:i ";" #'lispy-comment))

(setq corfu-bar-width 0)

(set! sp-show-pair-delay 0.2)
;; fix paren highlighting in normal mode
;; (set! sp-show-pair-from-inside nil)
;; sp-cancel-autoskip-on-backward-movement nil

(set! corfu-quit-at-boundary nil)
(set! corfu-auto t)
(set! corfu-auto-delay 0.1)
(set! corfu-auto-prefix 2)

(setq-default fill-column 100)

(set! sp-highlight-wrap-tag-overlay nil)
(set! sp-highlight-pair-overlay nil)
(set! sp-highlight-wrap-overlay nil)

(defadvice! disable-old-themes (around load-theme)
  "Disable old themes before loading new ones."
  (:args orig-fn &rest args)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(bind! (:state visual)
       ("V" #'er/contract-region)
       ("v" #'er/expand-region))

(defvar oo-file-map (make-sparse-keymap))
(define-prefix-command 'oo/file-prefix-command 'oo-file-map)
(bind! (:map oo-leader-map)
       (:wk "file" "f" #'oo/file-prefix-command))

(bind! (:when (require 'consult nil t))
       (:alt switch-to-buffer consult-buffer)
       (:alt yank-pop consult-yank-pop)
       (:alt apropos consult-apropos)
       (:alt man consult-man))

(defvar oo-search-map (make-sparse-keymap))
(define-prefix-command 'oo/search-prefix-command 'oo-search-map)
(bind! (:map oo-leader-map)
       (:wk "search" "s" #'oo/search-prefix-command))

(bind! (:map oo-search-map)
       (:wk "line"        "f" #'consult-line)
       (:wk "line"        "s" #'consult-line)
       (:wk "line"        "l" #'consult-line)
       (:wk "outline"     "h" #'consult-outline)
       (:wk "org heading" "o" #'consult-org-heading))

(bind! (:prefix "g")
       (:n "," #'lispyville-comment-or-uncomment)
       (:n "c" #'lispyville-comment-and-clone-dwim)
       (:n "l" #'lispyville-comment-or-uncomment-line))

(bind! (:map oo-package-map)
       (:wk "browse" "b" #'elpaca-browse)
       (:wk "update all"     "U" #'elpaca-update-all)
       (:wk "update"         "u" #'elpaca-update)
       (:wk "visit"          "v" #'elpaca-visit)
       (:wk "try"            "i" #'elpaca-try)
       (:wk "rebuild"        "r" #'elpaca-rebuild)
       (:wk "delete"         "d" #'elpaca-delete)
       (:wk "log"            "l" #'elpaca-log)
       (:wk "write lockfile" "w" #'elpaca-write-lockfile)
       (:wk "manager"        "m" #'elpaca-manager))

(bind! (:alt org-capture +org/choose-capture-template))

(bind! (:v "E" #'lispy-eval-and-replace))

(bind! (:map oo-app-map)
       ("g" #'gumshoe-peruse-globally))

(bind! (:map org-mode-map)
       (:n "H" #'outline-up-heading))

(bind! (:when (require 'consult nil t))
       (:alt switch-to-buffer consult-buffer)
       (:alt yank-pop consult-yank-pop)
       (:alt apropos consult-apropos)
       (:alt man consult-man))

(bind! (:map oo-find-map)
       (:wk "line"        "s" #'consult-line)
       (:wk "line"        "l" #'consult-line)
       (:wk "outline"     "h" #'consult-outline)
       (:wk "org heading" "o" #'consult-org-heading))

(bind! (:map helm-map)
       (:ie "C-;" #'ace-jump-helm-line))

(oo-add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

(bind! (:map oo-miscellany-map)
       ("l" #'consult-bookmark))

(set! bookmark-save-flag 1)

(defafter! dont-pair-quotes (smartparens)
  (sp-local-pair sp-lisp-modes "'" nil :actions nil))

(defafter! pair-backquote-quote (smartparens)
  (sp-local-pair sp-lisp-modes "`" "'" :when '(sp-in-string-p sp-in-comment-p)))

(set! avy-style 'pre)
(set! avy-keys (number-sequence 97 122))

(defsubst +captain-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss (point))))

(defun! +captain-prog-mode-sentence-start-function ()
  "Return point at the start of the last sentence.
Mean to be used as the value of `captain-predicate'."
  (cl-assert (require 'smartparens nil 'noerror))
  (awhen (car (bounds-of-thing-at-point 'sentence))
    (pushing! points it))
  (acond ((save-excursion (and (comment-beginning) (point)))
          (pushing! points it))
         ((and (nth 8 (syntax-ppss (point))) (sp-in-docstring-p nil nil 'string))
          (pushing! points it)))
  (apply #'max points))

(defhook! auto-capitalize-sentences-in-docstrings-and-comments (prog-mode-hook)
  (captain-mode 1)
  (setq-local captain-predicate #'+captain-in-string-or-comment-p)
  (setq-local captain-sentence-start-function #'+captain-prog-mode-sentence-start-function))

(defhook! auto-capitalize-sentences (text-mode-hook)
  (captain-mode 1)
  (setq-local captain-predicate (lambda () t)))

(oo-add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(set! rainbow-delimiters-max-face-count 9)
(oo-add-hook '(prog-mode-hook reb-mode-hook) #'rainbow-delimiters-mode)

(oo-add-hook 'prog-mode-hook #'hs-minor-mode)

(set! savehist-save-minibuffer-history t)
(set! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(set! savehist-autosave-interval (* 60 5))
(set! savehist-file (concat oo-cache-dir "savehist"))

(oo-add-hook 'on-first-input-hook #'savehist-mode)

(set! save-place-file (concat oo-cache-dir "saveplace"))
(set! save-place-limit nil)

(oo-add-hook 'on-first-file-hook #'save-place-mode)

(set! super-save-auto-save-when-idle t)
(set! super-save-idle-duration 5)

(oo-add-hook 'on-first-file-hook #'super-save-mode)

(oo-add-hook 'auto-fill-mode-hook #'filladapt-mode)

(set! evil-search-wrap nil)

(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! preserve-prior-evil-state (minibuffer-setup-hook)
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! restore-prior-evil-state (minibuffer-exit-hook)
  "Restore state after minibuffer."
  (when (bound-and-true-p evil-mode)
    (evil-change-state oo-evil-state-before-minibuffer)
    (setq oo-evil-state-before-minibuffer nil)))

(oo-add-hook 'after-init-hook #'require 'evil :depth 10)

(oo-add-hook 'after-init-hook #'evil-mode :depth 90)

(set! evil-echo-state nil)

;; Whether the cursor is moved backwards when exiting insert state.
(set! evil-move-cursor-back nil)

(set! evil-move-beyond-eol nil)

(defun oo-set-default-evil-cursors (&rest _)
  "Set the evil cursors."
  (when (bound-and-true-p evil-mode)
    (set! evil-insert-state-cursor '((bar . 3) "chartreuse3"))
    (set! evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
    (set! evil-normal-state-cursor '(box "DarkGoldenrod2"))
    (set! evil-visual-state-cursor '((hollow) "dark gray"))
    (set! evil-operator-state-cursor '((hbar . 10) "hot pink"))
    (set! evil-replace-state-cursor '(box "chocolate"))
    (set! evil-motion-state-cursor '(box "plum3"))))

(oo-add-hook 'evil-mode-hook #'oo-set-default-evil-cursors)
(oo-add-advice #'load-theme :after #'oo-set-default-evil-cursors)

(defvar oo-escape-hook nil
  "Hook run after escaping.")

(defun @exit-everything (&rest _)
  "Exits out of whatever is happening after escape."
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 (abort-recursive-edit))
	((run-hook-with-args-until-success 'oo-escape-hook))
	((or defining-kbd-macro executing-kbd-macro) nil)
	(t (keyboard-quit))))

(bind! (:ie [escape] #'evil-force-normal-state))

(oo-add-advice #'evil-force-normal-state :after #'@exit-everything)
(oo-add-advice #'lispyville-normal-state :after #'@exit-everything)

(oo-add-hook '(prog-mode-hook text-mode-hook) #'evil-surround-mode)

(defhook! enable-smartparens-maybe (minibuffer-setup-hook)
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

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

(adding-to-list! dogears-ignore-modes 'dashboard-mode)

(bind! (:map oo-leader-map)
       (:prefix "l")
       ("l" #'dogears-go))

(defun oo-org-src-buffer-p () (bound-and-true-p org-src-mode))
(adding-to-list! dogears-ignore-places-functions #'oo-org-src-buffer-p)

(adding-to-list! savehist-additional-variables 'dogears-list)

(adding-to-list! savehist-additional-variables 'register-alist)

(oo-add-hook 'on-first-input-hook #'dogears-mode)

(oo-silence #'flyspell-mode)

(oo-add-hook 'text-mode-hook #'flyspell-mode)

(setq-default bookmark-default-file (expand-file-name "bookmarks" oo-cache-dir))

(defun! oo/set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (let! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))

(set! transient-levels-file (concat oo-cache-dir "transient/levels"))
(set! transient-values-file (concat oo-cache-dir "transient/values"))
(set! transient-history-file (concat oo-cache-dir "transient/history"))

(adding-to-list! recentf-filename-handlers #'abbreviate-file-name)

(adding-to-list! recentf-filename-handlers #'substring-no-properties)

(oo-add-hook 'prog-mode-hook #'flyspell-prog-mode)

(set! ispell-program-name (or (executable-find "hunspell") (executable-find "ispell")))
