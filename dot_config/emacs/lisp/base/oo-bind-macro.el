(require 'oo-base-utils)
(require 'oo-modification-macros)
(require 'oo-base-definers)
(require 'oo-base-hook)

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

(defvar oo-deferred-key-bindings nil
  "An alist with elements of (KEYMAP . FORMS).
KEYMAP is the name of a keymap.  FORMS are a list of lisp forms that should be
evaluated when KEYMAP is bound.")

(defun! oo-eval-deferred-key-bindings-maybe (&rest _)
  "Evaluate any binding forms whose keymap has been loaded.
Evaluate forms from all elements of `oo-deferred-key-bindings' whose
KEYMAP is bound (the elements of the form (KEYMAP . FORMS)).  Additionally,
remove those elements from `oo-deferred-key-bindings'."
  (for! ((item &as map . forms) oo-deferred-key-bindings)
    (cond ((boundp map)
	       (pushing! forms `(oo--log-info "Evaluating %s bindings..." ',map))
	       (appending! body forms))
	      (t
	       (pushing! to-keep item))))
  (setq oo-deferred-key-bindings to-keep)
  (funcall `(lambda () ,@body)))

;; 1. Replace handlers with a function.
;; 2. Use treepy for producing tokens
;; 3. Add a specific function for alternate bindings
(defun oo-bind-alt (def alt)
  )

;; Make a function variant of `evil-define-minor-mode-key'. It's the same implementation of
;; `evil-define-minor-mode-key' but I want it to be a function not a macro so that I can more
;; easily use it with `oo-bind-key'.
(defun! +evil-define-minor-mode-key (minor-mode states key def)
  (dolist (state (-list states))
    (let! keymap (evil-get-minor-mode-keymap st mode))
    (define-key keymap key def)))

;; Extension to evil that lets me figure out the state later. This is needed if we want to be
;; thorough. As in, if we want to ensure that we account for evil states that may be defined
;; later. What we need to do is check to see which.

;; In the future I should probably modularize this. Right now `oo-bind-key' is more of
;; hard-coded. But right now I don't have so many things that I want it to do. I think it's body is
;; still relatively small.
(defun! oo-bind-key (keymap key def &rest plist)
  "Generic do-what-I-mean bind key."
  (if (and (symbolp keymap) (not (bound-and-true-p keymap)))
      (pushing! oo-deferred-key-bindings (list keymap key def plist))
    (with-map! plist
      (when @prefix
        (setq @key (concat prefix "\n" key)))
      (when @kbd
        (setq key (kbd key)))
      (when @autoload
        (oo-autoload-fn @def @autoload))
      (when @which-key
        (setq def (cons @which-key @def)))
      (cond ((member @state '(nil global))
             (define-key keymap key def))
            ((and @state @minor-mode)
             (oo-call-after-load 'evil #'+evil-define-minor-mode-key @minor-mode @state @keymap @key @def))
            (t
             (oo-call-after-load 'evil #'evil-define-key* @state @keymap @key @def))))))

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

;; (defun! oo-bind-macro-handle-alt (token interpreters)
;;   "Remap uses of command to alternate command."
;;   (let! (command . alt) (map-elt token :alt))
;;   (let! condition (or (map-elt token :when) t))
;;   (cond ((and alt condition command)
;; 	 (let! hook (oo-args-to-symbol 'oo-bind-hook-for- command))
;; 	 (let! filter-fn (oo-args-to-symbol 'oo-bind-filter-for- command))
;; 	 (let! key-var (gensym "key-"))
;; 	 (let! def-var (gensym "def-"))
;; 	 (setq token (map-insert token :def def-var))
;; 	 (setq token (map-insert token :key key-var))
;; 	 (unless (map-elt token :map)
;; 	   (setq token (map-insert token :map 'global-map)))
;; 	 `((setq ,def-var '(menu-item "" nil :filter ,filter-fn))
;; 	   (setq ,key-var [remap ,command])
;; 	   (unless (boundp ',hook)
;; 	     (defvar ,hook '((t ,command)))
;; 	     (defun ,filter-fn (&rest _)
;; 	       (run-hook-with-args-until-success ',hook))
;; 	     (defhook! ,(oo-args-to-symbol 'run- command '-maybe) (,hook)
;; 	       (when t #',command)))
;; 	   (defhook! ,(oo-args-to-symbol 'run- alt '-maybe) (,hook)
;; 	     (when ,condition #',alt))
;; 	   ,@(oo-bind-macro-handle-token token interpreters)))
;; 	(t
;; 	 (oo-bind-macro-handle-token token interpreters))))

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

(defun! oo-bind-macro-handle-token (token interpreters)
  "Return a list of forms created from applying INTERPRETERS to TOKEN.
INTERPRETERS are a list of function symbols that are applied to TOKEN."
  (let! (interpreter-fn . rest) interpreters)
  (funcall (or interpreter-fn #'ignore) token rest))

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

(provide 'oo-bind-macro)
