(require 'treepy)
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

;; It's faster to have a hook specifically for evaluating deferred bindings than it is to check
;; individually with a lambda. Each keymap is only checked once and only one function call is used
;; to check everything.
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

(defun! oo-bind-alt (def alt &optional when-fn)
  "Call ALT instead of DEF in keybindings.
If WHEN-FN."
  (let! name (oo-args-to-symbol 'oo-alt-bind-for- command))
  (fset name (-partial #'run-hook-with-args-until-success))
  ;; '(menu-item "" nil :filter ,filter-fn)
  ;; [remap ,command]
  )

;; Make a function variant of `evil-define-minor-mode-key'. It's the same implementation of
;; `evil-define-minor-mode-key' but I want it to be a function not a macro so that I can more
;; easily use it with `oo-bind-key'.
(defun! +evil-define-minor-mode-key (minor-mode states key def)
  "Same as `evil-define-minor-mode-key' but as a function."
  (dolist (state (-list states))
    (let! keymap (evil-get-minor-mode-keymap st mode))
    (define-key keymap key def)))

;; TODO:
;; Extension to evil that lets me figure out the state later. This is needed if we want to be
;; thorough. As in, if we want to ensure that we account for evil states that may be defined
;; later. What we need to do is check to see which.

;; In the future I should probably modularize this. Right now `oo-bind-key' is more of
;; hard-coded. But right now I don't have so many things that I want it to do. I think it's body is
;; still relatively small.
(defun! oo-define-key (map)
  "Generic do-what-I-mean bind key."
  (with-map! map
    (:use-keywords t)
    (if (and (symbolp $keymap) (not (bound-and-true-p $keymap)))
        (pushing! (alist-get $keymap oo-deferred-key-bindings) map)
      (when $prefix
        (setq $key (concat $prefix "\s" key)))
      (when $kbd
        (setq key (kbd key)))
      (when $autoload
        (oo-autoload-fn $def (unless (eq t $autoload) $autoload)))
      (when $which-key
        (setq def (cons $which-key $def)))
      (cond ((member $state '(nil global))
             (define-key $keymap $key $def))
            ((and $state $minor-mode)
             (oo-call-after-load 'evil #'+evil-define-minor-mode-key $minor-mode $state $keymap $key $def))
            (t
             (oo-call-after-load 'evil #'evil-define-key* $state $keymap $key $def))))))

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

(defun! oo-bind-get-metadata (clause)
  (let! zipper (treepy-list-zip clause))
  (while (not (treepy-end-p zipper))
    (let! node (treepy-node (treepy-next zipper)))
    (message "node-> %S" node)
    (setq zipper (treepy-next zipper))))

(defun oo-bind-macro-keybinding-p (token)
  "Return non-nil if TOKEN is a binding token."
  (or (and (assoc :def token) (assoc :key token)) (assoc :alt token)))

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
  (flet! bind-form (plist) `())
  (macroexp-progn (mapcar #'bind-form (oo-bind-macro-tokenites clause))))

(provide 'oo-bind-macro)

;; (pcase node
;;   (`(:minor-mode ,(pred symbolp) . ,(guard t))
;;    (list (list (pop node) (pop node))))
;;   ;; (`(:alt ,(pred symbolp) ,(pred symbolp) . ,(guard t))
;;   ;;  (list (list (pop node) (cons (pop node) (pop node)))))
;;   (`(:state ,(pred oo-non-keyword-symbol-p) . ,(guard t))
;;    (list (cons (pop node) (pop! node #'oo-non-keyword-symbol-p))))
;;   (`(,(or :map :localleader) ,(pred oo-non-keyword-symbol-p) . ,(guard t))
;;    (list (cons (pop node) (pop! node #'oo-non-keyword-symbol-p))))
;;   (`(,(pred stringp) . nil)
;;    (list (list :key (pop node)) (list :def nil)))
;;   (`(:wk ,(pred stringp) :prefix ,(pred stringp) . nil)
;;    (pop node)
;;    (list (list :wk-prefix (pop node))
;; 	     (list (pop node) (pop node))))
;;   (`(:wk ,(pred stringp) . ,(guard t))
;;    (list (list (pop node) (pop node))))
;;   (`(,(or :when :def :prefix) ,_ . ,(guard t))
;;    (list (pop! node 2)))
;;   (`(,(or (pred stringp) (pred vectorp) (pred symbolp)) (function ,(pred symbolp)) . ,(guard t))
;;    `((:key ,(pop node)) (:def ,(pop node))))
;;   (`(,(or (pred stringp) (pred vectorp) (pred symbolp)) ,(pred symbolp) . nil)
;;    `((:key ,(pop node)) (:def ,(pop node))))
;;   (`(:key ,_ . ,(guard t))
;;    (let! tokenites (list (pop! node 2)))
;;    (cond ((equal :def (car node))
;; 	      (affixing! tokenite (list (pop node) (pop node))))
;; 	     ((keywordp (car node))
;; 	      (affixing! tokenites (list :def nil)))
;; 	     (t
;; 	      (affixing! tokenites (list :def (pop node)))))
;;    tokenites)
;;   (`(,(and (pred keywordp) (app oo-bind-macro-evil-keyword-to-states states)) ,_ ,_ . ,(guard t))
;;    (pop node)
;;    (list (cons :state states) (list :key (pop node)) (list :def (pop node))))
;;   (`(,(or (pred stringp) (pred vectorp)) :wk . ,(guard t))
;;    (list (list :key (pop node)) (list :def nil) (list :ignore t)))
;;   (_
;;    (error "No matcher for `%S' of `%S'" (car node) node)))
