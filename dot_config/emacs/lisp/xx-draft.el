;;;; set initial font
;; I do not want to be setting the font as I am now every single time.
;; https://www.reddit.com/r/emacs/comments/xybrtw/what_is_the_most_appropriate_way_to_set_fonts_in/
;; (set-frame-font)
;; (cond ((x-list-fonts "Courier Prime")
;;        (set-frame-font "Courier Prime"))
;;       (()
;;        (set-frame-font "Iosevka Extended 12" nil t)))

(defun oo-tree-map-nodes (pred fun tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fun tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fun (car tree))
               (oo-tree-map-nodes pred fun (cdr tree))))
        (t
         tree)))

(defsubst oo-non-nil-symbol-p (obj)
  (and obj (symbolp obj)))

;; Being able to distinguish between a non-keyword symbol is useful enough to merit
;; its own function.
(defun oo-non-keyword-symbol-p (object)
  "Return t if OBJECT is a symbol but not a keyword."
  (declare (pure t) (side-effect-free t))
  (and (symbolp object) (not (keywordp object))))

(defun oo-args-to-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defmacro dowhile! (condition &rest body)
  (declare (indent 2))
  `(when ,condition
     ,(car body)
     ,@(cdr body)
     (while ,condition ,@(cdr body))))

(defmacro take-while! (condition list)
  (cl-with-gensyms (old new)
    `(let ((,new nil))
       (while (and ,list ,condition)
         (push (pop ,list) ,new))
       (setq ,new (nreverse ,new)))))

;; This is an attempt to use seq-let to make a pcase-let variant but with better
;; syntax.  Note this is *NOT* a very efficient way of doing this.  But it is
;; not like I am binding hundreds of bindings.  Dash's =-let= and =-let*= in
;; contrast are written to be very efficient.  But it must be said, this is
;; quite a beautiful way of composing macros I have written before.
(defmacro let! (bindings &rest body)
  (let (wrappers)
    (while bindings
      (while (and bindings (sequencep (car-safe (car-safe bindings))))
        (push `(seq-let ,@(pop bindings)) wrappers))
      (awhen! (take-while! (oo-non-nil-symbol-p (car-safe (car-safe bindings))) bindings)
              (push `(let* ,it) wrappers)))
    (oo-wrap-forms (reverse wrappers) body)))
;; Simple symbols, using this function can save me having to provide a string for
;; =format=.
;; (defun oo-args-to-symbol (&rest args)
;;   "Return an interned symbol from ARGS."
;;   (declare (pure t) (side-effect-free t))
;;   (intern (apply #'oo-args-to-string args)))

;; Convert patterns to pcase patterns
;; Essentially, I am just mapping.
(defun oo-seq-map-nodes (pred fn tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fn tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fn (car tree))
               (oo-tree-map-nodes pred fn (cdr tree))))
        ((vectorp tree)
         (seq-into (oo-tree-map-nodes pred fn (seq-into tree 'list)) 'vector))
        (t
         tree)))

;;;; function transformers
;; (defun oo-notfn (fn)
;;   (lambda (&rest args) (not (apply fn args))))

;; (defun oo-andfn (fn &rest fns)
;;   )

;;;; advices
;; A general advice to message the.
;; (defun oo-message-arguments (fn &rest args)
;;   ())

(defmacro add! (key value)
  "Add VALUE to the list associated with KEY in ALIST."
  (cl-with-gensyms (existing)
    `(setq alist
           (let ((,existing (assoc ,key alist)))
             (if ,existing
                 (setcdr existing (cons ,value (cdr ,existing)))
               (push (list ,key ,value) alist))))))

(let ((alist '((a 1) (b 2))))
  (add! 'a 3)
  (should (equal '((a 1 3) (b 2)) alist))
  (add! 'b 9)
  (should (equal '((b 9) (a 1 3) (b 2)) alist))
  (add! 'b 8)
  (should (equal '((b 9 8) (a 1 3) (b 2)) alist))
  (add! 'c 0)
  (should (equal '((c 0) (b 9 8) (a 1 3) (b 2)) alist)))

;; For brevity I do not want to add the =!= for this macro.
(defmacro -- (&rest body)
  `(lambda (it) ,@body))

;; Does pcase destructuring but without pcase syntax.
(defmacro let! (bindings &rest body)
  "Like `pcase-let*' but all."
  (declare (indent 1))
  (let (wrappers bind)
    (while bindings
      (setq bind (car-safe (car-safe bindings)))
      (let (pcase-binds)
        (while (and bind (sequencep bind) (not (stringp bind)))
          (setf (caar bindings) (oo-pcase-pattern (caar bindings)))
          (push (pop bindings) pcase-binds)
          (setq bind (car-safe (car-safe bindings))))
        (when pcase-binds
          (push `(pcase-let* ,(reverse pcase-binds)) wrappers)))
      (let (let-binds)
        (while (and bind (symbolp bind))
          (push (pop bindings) let-binds)
          (setq bind (car-safe (car-safe bindings))))
        (when let-binds
          (push `(let* ,(reverse let-binds)) wrappers))))
    (oo-wrap-forms (reverse wrappers) body)))

;; (ert-deftest take-while! ()
;;   (should (let ((foo '(a b 1 2 3))) (list (take-while! (symbolp foo) foo) foo))
;;           '((a b) (1 2 3)))
;;   ;; (should ())
;;   )

;; (ert-deftest oo-non-keyword-symbol-p ()
;;   (should (oo-non-keyword-symbol-p 'foo))
;;   (should-not (oo-non-keyword-symbol-p :foo))
;;   (should-not (oo-non-keyword-symbol-p "foo"))
;;   (should-not (oo-non-keyword-symbol-p 1)))

;; (ert-deftest oo-args-to-string ()
;;   (should (equal "foo" (oo-args-to-string 'foo)))
;;   (should (equal ":foo" (oo-args-to-string :foo)))
;;   (should (equal "foo" (oo-args-to-string "foo")))
;;   (should (equal "1" (oo-args-to-string 1))))

;; (ert-deftest oo-args-to-symbol ()
;;   (should (equal 'foo (oo-args-to-symbol 'foo)))
;;   (should (equal :foo (oo-args-to-symbol :foo)))
;;   (should (equal 'foo (oo-args-to-symbol "foo")))
;;   (should (equal '1 (oo-args-to-symbol 1))))

(defmacro lambda! (match-form &rest body)
  "Same as lambda but provides destructuring."
  (if (oo-all-p #'symbolp match-form)
      `(lambda ,match-form ,@body)
    (cl-with-gensyms (args)
      `(lambda (&rest ,args)
         (let! ((,match-form ,args))
           ,@body)))))

(defalias 'l! 'lambda!)

(ert-deftest lambda! ()
  ;; How many lists need to nest can be a bit confusing, but I think that this
  ;; needs to be.
  ;; (should (equal '((1 . 2) 1 2) (funcall (lambda! ((&as foo (a . b))) (list foo a b)) (cons 1 2))))
  (should (= 10 (funcall (lambda! (a (b c) d) (+ a b c d)) 1 '(2 3) 4)))
  (should (= 3 (funcall (lambda! (a b) (+ a b)) 1 2))))

;;;; flet!
;; I have to say that I am not a fan of the =cl-flet= syntax where you specify
;; the function bodies in the bindings.  I am more O.K. with the seemingly less
;; convenient syntax of =cl-letf= because it is more idiomatic in my opinion.
;; And you do not have to deal with bad indentation.  Only problem is =cl-letf=
;; does not provide access to the original function and in my experience, most
;; of the time, that is the function you want to define.  Sometimes I want this kind
;; of thing because stubbing in =block!= does not provide access to the original
;; function and neither does letf!.
;; However this does require thought from the macro perspective on how.
;; (defmacro! flet! (bindings &rest body)
;;   ""
;;   (stub! )
;;   (for! ((symbol args . body) bindings)
;;     (collecting! originals `(,(cl-gensym "original-fn") ,symbol))
;;     (collecting! sets `(fset ))
;;     (collecting! resets `()))
;;   `(let ,originals
;;      (unwind-protect (progn ,@sets ,@body)
;;        ,@resets)))
;;;;; Suppressing unecessary messages
;; There are tons of compilation messages which is one of the complaints with
;; the package.el.  The idea is if we do not.

;; Funny thing is I actually do not know which packages were.
;; The idea is to use this macro to determine whether.
;; (defmacro with-suppressed-messages! (&rest body)
;;   "Execute BODY, suppressing messages and capturing them in a string."
;;   (let ((original-message-fn (symbol-function 'message)))
;;     `(let ((capture-buffer (generate-new-buffer " *captured-messages*")))
;;        (with-current-buffer capture-buffer
;;          (unwind-protect
;;              (progn
;;                (fset 'message (lambda (&rest args)
;;                                 (with-current-buffer ,captured-messages
;;                                   (goto-char (point-max))
;;                                   (insert (apply 'format args) "\n"))))
;;                ,@body)
;;            (fset 'message ,original-message-fn)
;;            (kill-buffer capture-buffer))))))
;;;;; Advice to get arguments
;; This is an advice to.  This will not always work perfectly because sometimes
;; the FN is not a symbol.
(defun oo-message-fn-and-args (fn &rest args)
  (message "%S" (cons 'vc-clone args))
  (apply fn args))
;; I need to know whether vc-clone is the problem or if the problem is the
;; arguments passed into vc-clone are incorrect.
(advice-add #'vc-clone :around #'oo-message-fn-and-args)
;;;;; message only fail pass
;; (defun oo-message-only-fail-pass (fn args)
;;   (with-suppressed-messages!
;;    (apply fn args)))
;; (advice-add #'package-vc-install :around #'oo-message-only-fail-pass)

;;; Base Library
;; This is an initiative to remove external dependencies from the base functions
;; and macros that I use for my configuration.  Let me explain my reasoning.
;; First, I do not think I generally needed the convinience functions of dash,
;; f, and s.  With just a bit more work, I could do without them.  But also, I
;; want this base library to be a separate file that I can load for any elisp
;; scripts I write.  Relying on external libraries when loading scripts can be
;; complicated because you have to be sure you're packages are all set; if
;; they're not your script could take much longer if it tries to install the
;; packages before running its code, or it could even fail.  And it is painful
;; to write scripts without the functions and macros I painstakingly wrote to
;; help me.  Another advantage is I can use my library of helpers anywhere even

(defun oo-compose (&rest fns)
  "Return a function that is a composition of FNS.
FNS are a list of functions.  The returned function accepts a variable number of
arguments and returns the result of applying the previous function to the
arguments (right to left)."
  (lambda (&rest args)
    (cl-reduce (lambda (outer inner) nil) fns)))

;; I'd like to have the signature be something like ~(function &rest args &key
;; ...)~; that way it would be truly analogous to =funcall=. However, then the
;; signature would ambiguous if =function= has arguments that are the same as
;; keys specified by =&key=.

(cl-defun oo-condition-case-fn (fn &key (handlers 'error) (action #'ignore))
  "Return a function that calls ACTION when errors matching HANDLERS are raised.
ACTION is a function with three arguments the error object, FN and the list of
arguments FN will be called with."
  ;; To be honest I'm not sure if I need to make a gensym for the variable
  ;; `err'.  I do it just in case.
  (cl-with-gensyms (err)
    `(lambda (&rest args)
       (condition-case ,err
           (apply #',fn args)
         (,handlers (funcall #',action ,err #',fn args))))))
(defalias 'oo-ccase-fn 'oo-condition-case-fn)

(ert-deftest oo-compose ()
  (should (= 6 (funcall (oo-compose '1+ '+) 2 3)))
  (should (= 1 (funcall (oo-compose '1+ -) 2 3))))

(ert-deftest oo-condition-case-fn ()
  (should (oo-condition-case-fn)))

;; This is a backend function for the macro.  It is useful so that I can keep
;; the
;; ;; Something I was always confused about was why adjoin instead of just using
;; ;; =push=.  The latter is more performant; however I don't think that's.  The best reason I could
;; ;; think of is that sometimes you want to re-evaluate parts of your configuration
;; ;; and in that case it is more convenient to have =adjoin= over =push=.
;; (cl-defmacro adjoin! (place value &key test key test-not)
;;   "Adjoin value to place.
;; Same as `adjoining!' but use `set!' as the setter.  Meant to be used for
;; customizing variables."
;;   `(adjoining! ,place ,value :test ,test :key ,key :test-not ,test-not :setter set!))
;;;; with-map!
;; This is similar to the idea of =let-alist= but it works for plists.  And it
;; has better symbols.
;; (defun oo-bang-symbol-p (obj)
;;   "Return non-nil if OBJ is a bang symbol."
;;   (and obj
;;        (symbolp obj)
;;        (string-match-p "\\`![^[:space:]]+" (symbol-name obj))))

;; (defmacro collect! (pred &rest body)
;;   "Same as `for!' but accumulate result of body into TARGET."
;;   `(for! ()
;;      (pushing! ,target (macroexp-progn body))))

;;;; buffer-contents
(defun oo-buffer-contents (buffer)
  (with-current-buffer (get-buffer buffer)
    (buffer-string)))
;;;; advice abbrev alist
(defvar oo-how-alist '(("AR" . :around)
                       ("AF" . :after)
                       ("BF" . :before)
                       ("BU" . :before-until)
                       ("OV" . :override)
                       ("BW" . :before-while)
                       ("FA" . :filter-args)
                       ("FR" . :filter-return)))
;;;; oo-advise-components
(def oo-advice-symbol-regexp ())
(defvar oo-hook-symbol-regexp ())
(defun oo-advice-components (fsym)
  "Return."
  (declare (pure t) (side-effect-free t))
  (s-match regexp (symbol-name fsym)))
;;;; oo-or-fn
(describe "oo-or-fn"
  (it "returns non-nil if any of functions match"
    (expect (funcall (oo-or-fn #'stringp #'integerp) "foo"))
    (expect (funcall (oo-or-fn #'stringp #'integerp) 4)))
  (it "return nil if none of the functions match"
    (expect (not (funcall (oo-or-fn #'stringp #'integerp) 'a)))
    (expect (not (funcall (oo-or-fn #'stringp #'integerp) 4.5)))))
;;; with-elapsed-time!
(require 'benchmark)
(defmacro with-elapsed-time! (&rest forms)
  (let* ((elapsed-time (cl-gensym "elapsed-time"))
         (forms (macroexp-progn forms)))
    `(let ((,elapsed-time (benchmark-run ,forms)))
       (message "Evaluation time of %S: %.06f seconds" ',forms (car ,elapsed-time)))))
;;; logsym!
(defmacro logsym! (sym)
  `(if (boundp ',sym)
       (message "%s -> %S" ',sym (symbol-value ',sym))
     (message "symbol %s is not bound" ',sym)))

;; This is really messy lol.  I am sure there is a better way to do this.
;; Basically, I am saying add a comma to all the symbols, but replace certain
;; patterns with and record these patterns for me.  I would rather do this with
;; an iterator.
(defun oo--match-form-wrappers (match-form)
  "Return the pcase syntax representation of MATCH-FORM.
MATCH-FORM is a nested form of lists, vectors, and symbols."
  (cl-labels ((fn (wrappers tree)
                (pcase tree
                  ((pred null)
                   (list wrappers nil))
                  ((and sym (pred symbolp))
                   (list wrappers (list '\, sym)))
                  (`(&as ,whole ,parts)
                   (alet! (cl-gensym "&as")
                     (list `((let! ((,whole ,it)
                                    (,parts ,it))))
                           (list '\, it))))
                  (`(,(or '&map '&alist '&plist '&hash) ,map)
                   (alet! (cl-gensym "&map")
                     (list `((let ((,it ,map))) (with-map! ,it))
                           (list '\, it))))
                  ((pred listp)
                   (pcase-let* ((`(,wrappers1 ,tree1) (fn nil (car tree)))
                                (`(,wrappers2 ,tree2) (fn nil (cdr tree))))
                     (list (append wrappers wrappers1 wrappers2)
                           (cons tree1 tree2))))
                  ((pred vectorp)
                   (alet! (mapcar (apply-partially #'fn wrappers) (append tree))
                     (list (apply #'append (mapcar #'car it))
                           (vconcat (mapcan #'cdr it))))))))
    (pcase-let* ((`(,wrappers ,pcase-mf) (fn nil match-form)))
      (pcase pcase-mf
        (`(,(and comma (guard (equal '\, comma))) ,(and symbol (pred symbolp)))
         (list wrappers symbol))
        (_
         (list wrappers (list '\` pcase-mf)))))))
;;;
(defun oo-proper-list-p (obj)
  "Return non-nil only if OBJ is a proper list.
A proper list is defined as a sequence of cons cells ending with nil."
  (declare (pure t) (side-effect-free t))
  (and (listp obj)
       (or (null obj)
           (null (cdr-safe (last obj))))))

(defun oo-improper-list-p (obj)
  "Return non-nil only if OBJ is not a proper list.
See `oo-proper-list-p'."
  (declare (pure t) (side-effect-free t))
  (and (listp obj)
       (cdr-safe (last obj))))

(defun oo--tree-do (alist tree accum)
  "Perform action on TREE according to ALIST."
  (list tree accum)
  ;; (while alist
  ;;   ())
  ;; (cl-find-if (pcase-lambda (`(,pred)) (funcall pred tree)) alist)
  )

(defun oo--tree-do-recurse-list (alist tree)
  (cons (oo--tree-do alist (car tree))
        (oo--tree-do alist (cdr tree))))

(defun oo--tree-do-recurse-vector (alist tree)
  `[,@(mapcar (apply-partially #'oo--tree-do alist tree)
              (append tree nil))])
;; (cons #'symbolp #'oo--tree-collect-symbol)
;; (cons #'vectorp #'oo--tree-do-recurse-vector)
;; (cons #'listp #'oo--tree-do-recurse-list)
;; (cons #'always #'identity)

;; I want to use this to implement `block!'.
(defun oo--traverse-tree (tree)
  (let ((stack (list (list tree)))
        (node nil)
        (nodes nil))
    (while stack
      (cond ((car stack)
             (setq node (pop (car stack)))
             (push node nodes)
             (when (listp node) (push node stack)))
            (t
             (pop stack))))
    (nreverse nodes)))

(oo--traverse-tree '(a (b c) d))

((a (b c) d) a (b c) b c d)

;; => ((a (b c) d) a (b c) b c d)

(oo--traverse-tree '(a (b c) d))

(defun -tree-map-nodes-iter (pred fun tree)
  "Call FUN on each node of TREE that satisfies PRED iteratively.

If PRED returns nil, continue descending down this node.  If PRED
returns non-nil, apply FUN to this node and do not descend
further."
  (let ((stack (list tree))
        (nodes nil))
    (while stack
      (let ((node (pop stack)))
        (if (funcall pred node)
            (push (funcall fun node) nodes)
          (when (listp node)
            (dolist (child node)
              (push child stack))))))
    (nreverse nodes)))
;;;; pcase-match
(defmacro pcase-match! (expr value)
  "Return non-nil if EXPR matches VALUE.
EXPR is a `pcase-style' expression."
  `(pcase ,value
     (,expr t)
     (_ nil)))
;;;; oo-matches-p
(defun oo-matches-p (pattern value)
  "Return non-nil if PATTERN matches VALUE.")
;;;; functional
(defun oo-if-fn (condition-fn if-fn else-fn)
  "Return a function that returns."
  (lambda (&rest args) (if (funcall condition-fn)
                           (apply if-fn args)
                         (apply else-fn args))))

(defun oo-when-fn (condition-fn body-fn)
  "Return a function that returns."
  (oo-if-fn condition-fn body-fn #'ignore))

(defun oo-after-fn (fn after-fn)
  "Return fn"
  (lambda (args) (prog1 (apply fn args) (apply after-fn args))))

(defun oo-before-fn (fn)
  "Return"
  (lambda (args) (funcall before-fn args) (apply fn args)))
;;;; quiet!
;; TODO: make it so that I can specify regular expressions of messages.
;; I copied much of the bod of this from the =shut-up= package.  I really wanted
;; to just use that package but the problem is that I need this macro
;; beforehand, specifically for package installation with =package.el=.  The
;; =shut-up= package does a bit more because it puts the messages in a different
;; buffer, but I won't go into that yet--not when and until I think I need it.
(defmacro quiet! (&rest body)
  "Suppress message output during BODY."
  `(let! ((standard-output #'ignore)
          (#'message #'ignore)
          (#'write-region
           ;; Wish there was a way not to have to specify all the arguments
           ;; twice.  Well see if I find one or one day thing of one.
           ;; complicating things is that some of the arguments are optional.
           (lambda (start end fname &optional append visit lockname mustbenew)
             (unless visit (setq visit 'no-message))
             (funcall this-fn start end fname append visit lockname mustbenew)))
          (#'load (lambda (fn file noerror nomsg nosuffix must-suffix)
                    (funcall this-fn file noerror t nosuffix must-suffix))))
     ,@body))
;;;; conversion
;; (defun oo-ing-symbol-p (obj)
;;   "Return non-nil of OBJ is an ing macro symbol.
;; Examples of such symbols include `appending!' and `collecting!'."
;;   (and (symbolp obj)
;;        (string-match-p "ing!\\'" (symbol-name obj))))

(defun oo-symbol-match-p (regexp sym)
  "Return non-nil only if symbol SYM matches REGEXP."
  (and (symbolp sym)
       (string-match-p regexp (symbol-name sym))))
;;;; =cl-prov= variant of =cl-flet=

;; For testing and guaranteeing that I do not overwrite an existing function, I
;; want to bind the function value of a gensym.  I could use `cl-letf' and know
;; that the symbol that I choose will be rebound, but if that function that I
;; choose is in the body of `cl-letf' then I will be overwritting it.  By
;; choosing a silly name like `foo' you minimize that chance, but there is still
;; a chance.
;; (comment!
;;  (defhook! hook&what-it-does (hook-args)
;;    [special args]))
(rx-to-string `(seq (group (one-or-more (not space)))
                    "@"
                    (group (or ,@(mapcar (-compose #'symbol-name #'car) oo-advice-how-alist)))      
                    (group (one-or-more (not space)))))

(rx-to-string '(: (group (: bol ";;;;" blank (0+ any) eol "\n"))
                  (0+ (or (: bol (** 0 3 ";") (not ";") (0+ any) eol)
                          (: bol (>= 5 ";") (0+ any) eol)
                          (: bol (not ";") (0+ any) eol)
                          "\n"))))

(defun! oo-sort-level-2-headings ()
  "Sort level 2 headings and their contents alphabetically by heading name."
  (interactive)
  (set! rx "\\(?:\\(\\(?:^;;;;[[:blank:]].*$\\)\n\\)\\(?:^;\\{0,3\\}[^;].*$\\|^;\\{5,\\}.*$\\|^[^;].*$\\|\n\\)*\\)")
  (save-excursion
    (goto-char (point-min))
    (sort-regexp-fields nil rx "\\1" (point) (point-max))))

(save-excursion (sort-regexp-fields nil "\\(?:(elpaca (?\\([^ )]+\\).*$\\)" "\\1" (point-min) (point-max)))

(defun oo/do-replace ()
  (interactive)
  (replace-regexp-in-region "\\(.+\\)->\\(.+\\)$" "(define-global-abbrev \"\1\" \"\2\")" (point-min) (point-max)))
;;;;; TODO: adjust start location in strings
;; Although abbrev is expanding in strings.
;; This should only need to be active in prog-mode.
;; (defadvice! abbrev--default-expand@ARadjust-start-location (expand-fn)
;;   (cond ((not (equal 'string (oo--enable-text-mode-abbrev-p)))
;;          (funcall expand-fn))
;;         ((not (looking-back (rx "\"" (group (1+ (not white))) (* space))
;;                             (line-beginning-position)))
;;          (funcall expand-fn))
;;         (t
;;          (funcall expand-fn)
;;          ;; This does not work for some reason.  Instead, what I will do is put
;;          ;; the stuff into a buffer.
;;          ;; (set! name (match-string 1))
;;          ;; (message "name-> %S" name)
;;          ;; (set! expansion (save-match-data
;;          ;;                   (with-temp-buffer
;;          ;;                     (insert name)
;;          ;;                     (expand-region-abbrevs (point-min) (point-max) t)
;;          ;;                     (buffer-string))))
;;          ;; (replace-match expansion nil nil nil 1)
;;          ;; (message "adjusting...%S" ret)
;;          ;; (set! name (match-string 1))
;;          ;; (set! symbol (intern name))
;;          ;; (set! start (match-beginning 1))
;;          ;; (set! end (match-end 1))
;;          ;; (set! ret (list symbol name start end))
;;          ;; (apply #'abbrev-insert ret)
;;          ;; (nflet! abbrev--before-point (-const ret))
;;          )))

;;;;;; TODO: title this heading properly
;; (oo-text-abbrev ".name" "Luis Henriquez-Perez")
;; (defun oo--insert-time (format-string &optional timezone)
;;   (insert (format-time-string format-string timezone)))
;; (define-abbrev global-abbrev-table ".year" "" (-partial #'oo--insert-time "%Y"))
;; (define-abbrev global-abbrev-table ".monthday" "" (-partial #'oo--insert-time "%d"))
;; (define-abbrev global-abbrev-table ".mday" "" (-partial #'oo--insert-time "%d"))
;; (define-abbrev global-abbrev-table ".month" "" (-partial #'oo--insert-time "%m"))
;; (define-abbrev global-abbrev-table ".minute" "" (-partial #'oo--insert-time "%M"))
;; (define-abbrev global-abbrev-table ".sec" "" (-partial #'oo--insert-time "%S"))
;; (define-abbrev global-abbrev-table ".second" "" (-partial #'oo--insert-time "%S"))

;;;; aas
;; (oo-add-hook 'emacs-startup-hook #'aas-global-mode)
;; (require 'aas)
;; (aas-set-snippets 'global
;;   "fifo" "first in first out"
;;   "filo" "first in last out"
;;   ";--" "—"
;;   ";->" "→")
;;;; autoloading
;; I tried making this as a macro called [[][catch-autoloads!]] but I ran in to
;; some issues.  First, my initial implementation ran into infinite recursion
;; during macroexpansion.  And then after I fixed that I had problems with lexical
;; binding.
(defun! oo-autoload-function (fn)
  "Call FN with ARGS trying to load features of any undefined symbols.
If an void-function or void-variable error is raised try to guess the parent
feature."
  (oo-cond-case-fn fn #'oo-autoload-action-function '(void-function void-variable)))

(defun! oo-candidate-features (fn paths)
  "Return a list of candidate features for FN.
FN is a function symbol."
  (set! fname (symbol-name fn))
  (for! (path paths)
    (set>>! base
      (directory-file-name path)
      (file-name-nondirectory)
      (file-name-sans-extension))
    (when (s-prefix-p base fname)
      (pushing! candidates (intern base))))
  (seq-sort-by (-compose #'length #'symbol-name) #'> candidates))

(defun! oo-try-autoload-on-error (function)
  "Call function.
Try guess if feature is bound.
ERROR is either a void-variable or void-function error."
  (set! (type symbol . rest) error)
  (cl-assert (member type '(void-variable void-function)))
  (set! bound-fn (cl-case type (void-variable #'boundp) (void-function #'fboundp)))
  (set! candidates (oo-candidate-features symbol load-path))
  (for! (candidate candidates)
    (require candidate nil t)
    (when (funcall bound-fn symbol)
      ;; Try calling the function again until.
      (return! (oo-funcall-autoload function))))
  (signal (car error) (cdr error)))
;;;; setup
;; Generic configuration macro.
;; (defmacro defsetup! (key args &rest body)
;;   (declare (indent defun)))

;; (defmacro setup! (key &rest args)
;;   "A cross-configuration macro."
;;   (apply setup-fn key args))

;; (setup! :popup bottom)
;; (defsetup! :popup (place regexp)
;;   "Open buffers at bottom that match regexp."
;;   `(alet (,regexp
;;           (display-buffer-at-bottom)
;;           (side bottom)
;;           (slot 1)
;;           (window-height 0.5)
;;           (window-parameters ((no-other-window t))))
;;      (push it display-buffer-alist)))

;; (defun tempel-setup-capf ()
;;   ;; Add the Tempel Capf to `completion-at-point-functions'.
;;   ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;   ;; `tempel-complete' if you want to see all matches, but then you
;;   ;; should also configure `tempel-trigger-prefix', such that Tempel
;;   ;; does not trigger too often when you don't expect it. NOTE: We add
;;   ;; `tempel-expand' *before* the main programming mode Capf, such
;;   ;; that it will be tried first.
;;   (pushing! completion-at-point-functions #'tempel-complete :setter setq-local)
;;   (setq-local completion-at-point-functions
;;               (cons #'tempel-expand
;;                     completion-at-point-functions)))
;; (defun oo-elisp-capf ()
;;   (cape-wrap-super #'tempel-complete #'elisp-completion-at-point))

;; (setq-local completion-at-point-functions (list #'oo-elisp-capf t))

;; ;; (collecting-hook! conf-mode-hook&setup-tempel-capf #'tempel-complete)
;; (add-hook 'conf-mode-hook 'tempel-setup-capf)
;; (add-hook 'prog-mode-hook 'tempel-setup-capf)
;; (add-hook 'text-mode-hook 'tempel-setup-capf)
;;;;;; special character abbrevs
;; I want to make it so that I do not need to leave the homerow for entering
;; these special characters.
(oo-text-abbrev ";tilde" "~")
(oo-text-abbrev ";back" "`")
(oo-text-abbrev ";bang" "!")
(oo-text-abbrev ";at" "@")
(oo-text-abbrev ";pound" "#")
(oo-text-abbrev ";doll" "$")
(oo-text-abbrev ";dollar" "$")
(oo-text-abbrev ";perc" "%")
(oo-text-abbrev ";car" "^")
(oo-text-abbrev ";apos" "&")
(oo-text-abbrev ";" "*")
(oo-text-abbrev ";lpar" "(")
(oo-text-abbrev "aa" ")")
(oo-text-abbrev ";und" "_")
(oo-text-abbrev "aa" "-")
(oo-text-abbrev ";plus" "+")
(oo-text-abbrev ";minus" "=")
(oo-text-abbrev ";eq" "=")

;;;;; benchmark-init
(require 'benchmark-init)

(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(oo-add-hook 'after-init-hook 'benchmark-init/deactivate)

;;;;; ah
(require 'ah)
(ah-mode 1)
