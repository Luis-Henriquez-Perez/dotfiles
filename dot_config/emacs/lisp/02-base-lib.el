;;; 02-base-lib.el --- main library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 02 Feb 2024
;;
;; URL:
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
;; This is the primary set of functions and macro I use to.  They are designed
;; to only rely on built-in Emacs packages so that they can be loaded into any
;; script and also to make.
;;;; require built-in packages
;; I need it to help me with pattern matching.
(require 'seq)
(require 'pcase)
;; It has several useful functions and macros.
(require 'subr-x)
(require 'map)
(require 'cl-lib)
;;;; aliases
;; Make symbols with consistent naming conventions to elisp predicate functions.
;; Most elisp predicate functions end in by "-p" to denote that they return
;; non-nil if a condition is true.  Many of the predicate functions provided by
;; =cl-lib= do not follow this convention.
;;;; helpers
(defun oo-cons-cell-p (obj)
  "Return non-nil only if OBJ is a cons-cell."
  (declare (pure t) (side-effect-free t))
  (and (consp obj)
       (not (listp (cdr obj)))))

(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms to be wrapped.  WRAPPERS are a list of forms
representing the wrappers to apply.  If WRAPPERS is empty, `progn' is added to
ensure the result is syntactically valid."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (append wrapper (list forms))))
  forms)

(defun oo-into-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defun oo-into-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-into-string args)))

(defun oo-into-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-into-symbol ":" args))
;;;; special symbols
(defun oo-list-marker-p (obj)
  "Return non-nil if OBJ is a list marker.
List markers are symbols that begin with `&' such as are `&rest' and
`&optional'."
  (declare (pure t) (side-effect-free t))
  (char-equal ?& (seq-first (symbol-name obj))))
;;;; anaphoric macros
;; I used the [[][anaphora]] package for these macros, but in reality they are
;; trivial to write on my own.
(defmacro alet! (expr &rest body)
  "Bind the result of EXPR to `it' during BODY."
  (declare (indent 1))
  `(let ((it ,expr)) ,@body))
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

;; One thing is the fact that because it is a function it can be composed and
;; chained.  Another is I can swap in and out the =condition-case= body and handlers
;; without having to write out the whole =condition-case= form.  Even though the
;; =condition-case= form is relatively simple I have admittedly had trouble
;; remembering its components off the top of my head.

;; I'd like to have the signature be something like ~(function &rest args &key ...)~;
;; that way it would be truly analogous to =funcall=. However, then the signature
;; would ambiguous if =function= has arguments that are the same as keys specified by
;; =&key=.
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
;;;; setting
(cl-defmacro appending! (place list &key (setter 'setf))
  "Append LIST to the end of PLACE.
SETTER is the symbol of the macro or function used to do the setting."
  `(,setter ,place (append ,place ,list)))

;; Important to note that this macro is not as efficient as pushing because it's
;; adding to the end of the list.  So this macro should be used only in
;; non-performance-intensive code.  In performance-intensive code we need the
;; =push-nreverse= idiom.
(cl-defmacro collecting! (place item &key (setter 'setf))
  "Affix ITEM to the end of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (append ,place (list ,item))))

(defalias 'snocing! 'collecting!)
(defalias 'affixing! 'collecting!)

;; You might be wondering why I didn't create a macro with a setter for these.
;; Well I haven't had a case yet when I've wanted to increment or decrement the
;; value symbol used in customization.
(defalias 'incrementing! 'cl-incf)
(defalias 'counting! 'cl-incf)
(defalias 'decrementing! 'cl-decf)

(cl-defmacro prepending! (place list &key (setter 'setf))
  "Prepend LIST to beginning of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (append ,list ,place)))

(cl-defmacro maxing! (place form &key (setter 'setf) (comparator '>))
  "Set PLACE to the greater of PLACE and FORM.
SETTER is the same as in `appending!'.  COMPARATOR is the comparison function
to determine the greater value."
  (cl-with-gensyms (value1 value2)
    `(,setter ,place (let ((,value1 ,form)
                           (,value2 ,place))
                       (if (,comparator ,value1 ,value2) ,value1 ,value2)))))

(cl-defmacro minning! (place form &key (setter 'setf) (comparator '<))
  "Set PLACE to the lesser of PLACE and FORM.
SETTER is the same as in `appending!'.  COMPARATOR is used to determine the
lesser value."
  `(maxing! ,place ,form :setter ,setter :comparator ,comparator))

(cl-defmacro concating! (place string &key (setter 'setf) separator)
  "Concat PLACE and STRING with SEPARATOR.
SETTER is the same as in `appending!'"
  `(,setter ,place (string-join (list ,place ,string) ,separator)))

(cl-defmacro adjoining! (place item &key test test-not key (setter 'setf))
  "Set PLACE to the value of `(cl-adjoin ITEM PLACE)'.
SETTER is the same as in `appending!'.  KEY, TEST, TEST-NOT are the same as in
`cl-adjoin'."
  (alet! `(:test ,test :test-not ,test-not :key ,key)
    `(,setter ,place (cl-adjoin ,item ,place ,@it))))

;; I know =push= already exists.  But I want a variant of push that can be used
;; with the =block!= macro.
(cl-defmacro pushing! (place item &key (setter 'setf))
  "Cons ITEM to PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (cons ,item ,place)))

;; To configure variables I don't use the standard =setq=--at least not
;; directly.  Instead, I use =set!=.  Adjoining is one of the most common
;; operations done to lisp symbols when configuring Emacs.

;; Something I was always confused about was why adjoin instead of just using
;; =push=.  The latter is more performant; however I don't think that's.  The
;; best reason I could think of is that sometimes you want to re-evaluate parts
;; of your configuration and in that case it is more convenient to have =adjoin=
;; over =push=.
(cl-defmacro unioning! (place list &key test test-not key (setter 'setf))
  "Set PLACE to the union of PLACE and LIST.
SETTER, KEY, TEST, TEST-NOT are the same as in `adjoining!'."
  (alet! `(:test ,test :test-not ,test-not :key ,key)
    `(,setter ,place (cl-union ,place ,list ,@it))))
;;;; map!
(defun oo--map-let-binds (map body regexp &optional use-keywords)
  "Return a list of let-bindings for MAP.
Collect symbols matching REGEXP in BODY and let bind them to."
  (let* ((mapsym (cl-gensym "map"))
         (let-binds `((,mapsym ,map)))
         (name nil)
         (key nil))
    (dolist (obj (flatten-tree body))
      (when (and obj
                 (symbolp obj)
                 (setq name (symbol-name obj))
                 (string-match regexp name)
                 (not (assoc obj let-binds)))
        (setq key (funcall (if use-keywords #'oo-into-keyword #'oo-into-symbol)
                           (match-string 1 name)))
        (pushing! let-binds `(,obj (map-elt ,mapsym ',key)))))
    (nreverse let-binds)))

(defmacro with-map! (map &rest body)
  "Let-bind bang symbols in BODY to corresponding keys in MAP."
  `(let* ,(oo--map-let-binds map body "!\\([^[:space:]]+\\)" nil)
     ,@body))
;;;; let!
;; I want `oo-tree-map-nodes' to be more flexible.  I want it to accept maybe an
;; alist of (PRED . FN) as opposed to a PRED, FN.
(defun oo-tree-map-nodes (pred fn tree)
  "Same as `-tree-map-nodes', but works for improper lists."
  (cond ((funcall pred tree)
         (funcall fn tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fn (car tree))
               (oo-tree-map-nodes pred fn (cdr tree))))
        ((vectorp tree)
         `[,@(mapcar (apply-partially #'oo-tree-map-nodes pred fn)
                     (append tree nil))])
        (t
         tree)))

(defun oo--to-pcase (match-form)
  "Return MATCH-FORM as pcase syntax.
MATCH form is a potentially nested structure of only list, vectors and symbols."
  (if (symbolp match-form)
      match-form
    (cl-flet ((true-symbolp (o) (and o (symbolp o)))
              (add-comma (o) (list '\, o)))
      (list '\` (oo-tree-map-nodes #'true-symbolp #'add-comma match-form)))))

(defvar oo--destruc-alist '((oo--&as-mf-p . oo--&as-mf-destruc)))

(defun oo--&as-mf-p (form)
  "Return non-nil if FORM is an `&as' match form pattern."
  (pcase form
    (`(,(or '&as '&whole) ,(and (pred symbolp) whole) ,part)
     t)
    (_
     nil)))

(defun oo--&as-mf-destruc (as-match-form value)
  "Return friendly bindings."
  (pcase-let* ((`(&as ,whole ,parts) as-match-form)
               (gsym (cl-gensym "special-&as-match-form")))
    `((,gsym ,value)
      (,whole ,gsym)
      (,parts ,gsym))))

(defun oo--mf-match-p (match-form)
  "Return non-nil if FORM matches any special match forms."
  (cl-some (lambda (pred) (funcall pred match-form))
           (mapcar #'car oo--destruc-alist)))

(defun oo--mf-destruc (match-form value)
  "Return"
  (cl-flet ((match-p (pcase-lambda (`(,pred . ,_)) (funcall pred match-form))))
    (pcase-let ((`(,_ . ,destruc-fn) (cl-find-if #'match-p oo--destruc-alist)))
      (funcall destruc-fn match-form value))))

(defun oo--mf-replace (match-form value)
  "Return list of bindings."
  (list  value)
  (let (other-bindings)
    (cl-flet ((replace (lambda (mf)
                         (cl-with-gensyms (mf-value)
                           (appending! other-bindings (oo--mf-destruc mf mf-value))
                           mf-value))))
      `((,(oo-tree-map-nodes #'oo--mf-match-p #'replace match-form) ,value)
        ,@other-bindings))))

(defun oo--to-pcase-let (match-form value)
  "Return BINDINGS."
  (mapcar (pcase-lambda (`(,mf ,val)) (list (oo--to-pcase mf) val))
          (oo--mf-replace match-form value)))

(defun oo--let-bind (bind)
  "Return a list of wrappers for binding BIND."
  (pcase bind
    ((pred symbolp)
     `((let* (,bind))))
    (`(,(pred symbolp) ,_)
     `((let* (,bind))))
    (`(,(or ':flet :stub) ,(and name (pred symbolp)) ,lambda)
     `((cl-flet ((,name ,lambda)))))
    (`(,(or ':flet :stub) ,(and name (pred symbolp)) ,args . ,body)
     `((cl-flet ((,name ,args . ,body)))))
    (`(#',(and fn (pred symbolp)) ,lambda)
     `((lef! ((,fn ,lambda)))))
    (`(,(or ':noflet :nflet) ,(and fn (pred symbolp)) ,lambda)
     `((lef! ((,fn ,lambda)))))
    (`(,(and mf (or (pred listp) (pred vectorp))) ,value)
     `((pcase-let* ,(oo--to-pcase-let mf value))))
    (_
     (error "Unknown predicate %S" bind))))

;; I do not think that sef extensions are crazy useful for `cl-letf' except for
;; `(symbol-function)'.  In terms of implementation I want to create a macro
;; that ties together all the "letters"--cl-letf, cl-flet, cl-labels,
;; cl-macrolet, etc.
(defmacro let! (bindings &rest body)
  "Bind BINDINGS during BODY.
This is like `let*' but it has two more kinds of possible bindings.

- (let! ((MATCH-FORM VALUE)) . BODY)
MATCH-FORM a nested form of vectors, list and non-nil symbols.

- (let! ((:lef foo FUNCTION)) . BODY)
- (let! ((:noflet foo FUNCTION)) . BODY)
- (let! ((#'foo FUNCTION)) . BODY)
Bind function via.

- (let! ((:lef foo FUNCTION)) . BODY)
- (let! ((:noflet foo FUNCTION)) . BODY)
- (let! ((#'foo FUNCTION)) . BODY)

- (let! ((:stub SYMBOL FN)) . BODY)
- (let! ((:flet SYMBOL FN)) . BODY)
Let bind function."
  (declare (indent 1))
  (oo-wrap-forms (mapcan #'oo--let-bind bindings) body))
;;;; block!
(defun oo-ing-symbol-p (obj)
  "Return non-nil of OBJ is an ing macro symbol.
Examples of such symbols include `appending!' and `collecting!'."
  (and (symbolp obj) (string-match-p "ing!\\'" (symbol-name obj))))

(defun oo--parse-block (data tree)
  "Return an updated list of (DATA TREE) based on contents of TREE.
DATA is a plist.  Tree is a list of forms.  For how a tree is interpreted see
`block!'."
  (pcase tree
    (`(,(or 'cl-function 'function 'quote 'backquote) . ,_)
     (list data tree))
    (`(,(and loop (or 'for! 'while 'dolist! 'dolist)) ,pred . ,body)
     (pcase-let* ((`(,data1 ,tree) (oo--parse-block nil body)))
       (list (map-merge-with 'plist #'append data data1)
             `(catch 'break! (,loop ,pred (catch 'continue! ,@tree))))))
    (`(,(and name (pred oo-ing-symbol-p)) ,symbol . ,(guard t))
     (alet! (cl-case name
              ((maxing! maximizing!) most-negative-fixnum)
              ((minning! minimizing!) most-positive-fixnum)
              (counting! 0))
       (adjoining! (map-elt data :let)
                   (list symbol it) :test #'equal :key #'car))
     (list data tree))
    (`((,(or 'with! 'wrap!) . ,(and wrappers (pred listp))) . ,rest)
     (appending! (map-elt data :wrappers) wrappers)
     (list data rest))
    (`((,(or 'without! 'exclude!) . ,symbols) . ,rest)
     (appending! (map-elt data :no-let) symbols)
     (list data rest))
    (`((gensym! ,(and name (pred symbolp))) . ,rest)
     (adjoining! (map-elt data :let) (list name (cl-gensym (symbol-name name))))
     (list data rest))
    (`(set! ,match-form ,value . ,(and plist (guard t)))
     (pushing! (map-elt data :let) (list match-form (map-elt plist :init)))
     (list data `(setq ,match-form ,value)))
    (`((,(or 'stub! 'flet!) . ,args) . ,rest)
     (let! (((data1 rest) (oo--parse-block nil rest)))
       (list (map-merge 'plist data data1) `((cl-flet ((,@args)) ,@rest)))))
    ;; This is my own variant that takes the original function.  I name it.
    (`((,(or 'nflet! 'noflet!) . ,args) . ,rest)
     (let! (((data1 rest) (oo--parse-block nil rest)))
       (list (map-merge 'plist data data1) `((lef! ((,@args)) ,@rest)))))
    ((and (pred listp) (pred (not oo-cons-cell-p)) (pred (not null)))
     (pcase-let ((`(,data1 ,tree1) (oo--parse-block nil (car tree)))
                 (`(,data2 ,tree2) (oo--parse-block nil (cdr tree))))
       (list (map-merge-with 'plist #'append data data1 data2)
             (cons tree1 tree2))))
    (_
     (list data tree))))

(defmacro block! (&rest body)
  "Same as `cl-block' but modify BODY depending on particular forms.
The following describes possible modifications.

- (set! SYM _ [:init EXPR])
Let bind SYM to nil.  If :init VAL is specified, let BIND SYM to EXPR.

- (maxing! SYM _ [:init EXPR])
Let bind SYM to `least-positive-fixnum'.

- (minning! SYM _ [:init EXPR])
Let bind SYM to `most-positive-fixnum'.

- (counting! SYM _ [:init EXPR])
Let bind SYM to 0.

- (INGMAC SYM VAL [:init EXPR])
INGMAC is a macro ending in \"ing!\" such as `appending!', `pushing!',
`collecting!', etc.

- (gensym! SYM)
Let bind SYM to (gensym \"SYM\").

- (wrap! . WRAPPERS)
Surround block body with WRAPPERS. WRAPPERS is as in `oo-wrap-forms'.

- (with! . WRAPPERS)
Same as wrap!.

- (exclude! . VARS)
Do not let bind any vars in VARS.

- (without! . VARS)
Same as `exclude!'.

- (stub! NAME ARGS . BODY)
Wrap subsequent forms with `(cl-flet ((NAME ARGS . BODY)))'.

- (nflet! NAME ARGS . BODY)
Wrap subsequent forms with `(lef! ((NAME ARGS)))'.

-(LOOP ...)
Wrap the loop with `(catch \\='break!) and its body.
with `(catch \\='continue!)'. LOOP can be `for!',
`dolist', `dolist!' or `while'.

Like `cl-block' `cl-return' and `cl-return-from' work in BODY."
  (declare (indent 1))
  (let! (((data body) (oo--parse-block nil body))
         ;; lets is an alist.
         (lets (map-elt data :let))
         ;; nolets is a list of symbols.
         (nolets (map-elt data :nolet))
         (binds (cl-remove-if (lambda (bind) (member (car bind) nolets)) lets))
         (wrappers `((catch 'return!) (let ,binds) ,@(map-elt data :wrappers))))
    (oo-wrap-forms wrappers body)))

(defmacro return! (&optional value)
  "Cause `block!' to exit and return VALUE.
See `block!'."
  `(throw 'return! ,value))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
See `block!'."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of loop.
See `block!'."
  `(throw 'continue! nil))

(defalias 'skip! 'continue!)

(defmacro exclude! (&rest _)
  "Signal to `block!' not to let bind VARS.
See `block!'.")
(defalias 'without! 'exclude!)

(defmacro stub! (name args &rest body)
  "Define a local function definition with `cl-flet'.
NAME, ARGS and BODY are the same as in `defun'.
Must be used in `block!'."
  (declare (indent defun))
  (ignore name args body))
(defalias 'flet! 'stub!)
(defalias 'noflet! 'stub!)
(defalias 'nflet! 'stub!)
;;;; lef!
(defmacro lef! (bindings &rest body)
  "Bind each symbol in BINDINGS to its corresponding function during BODY.
BINDINGS is a list of either (SYMBOL FUNCTION), where symbol is the symbol to be
bound and FUNCTION is the function to bind it to; or (SYMBOL ARGS BODY).  In
each of BINDINGS if the symbol is an existing function symbol let-bind the
original function to `this-fn', otherwise bind `this-fn' to nil."
  (let (binds orig-fn)
    (pcase-dolist (`(,sym . ,rest) bindings)
      (setq orig-fn (gensym "orig-fn"))
      (push `(,orig-fn (when (fboundp #',sym) (symbol-function #',sym))) binds)
      (push (list (symbol-function #',sym)
                  (pcase rest
                    (`(,fn . nil)
                     `(lambda (&rest args)
                        (let ((this-fn ,orig-fn))
                          (apply ,fn args))))
                    (`(,args . ,function-body)
                     `(lambda ,args
                        (let ((this-fn ,orig-fn))
                          ,@function-body)))))
            binds))
    `(cl-letf* ,(nreverse binds) ,@body)))
;;;; defmacro! and defun!
(defun oo-defun-components (body &optional show-nils)
  "Divide defun body, BODY, into its components.
The return value should be of the form ((docstring declarations interactive)
body)."
  (let (doc decls int)
    (setq doc (when (stringp (car-safe body)) (pop body)))
    (setq decls (when (equal 'declare (car-safe (car-safe body))) (pop body)))
    (setq int (when (equal 'interactive (car-safe (car-safe body))) (pop body)))
    (list (cl-remove-if (if show-nils #'ignore #'null) (list doc decls int)) body)))

;; It is easier to tests functions that return a value rather than macros.  So I
;; prefer writing a helper that returns the macro body as data as opposed to
;; doing the expansion directly in the macro.
(defun oo--definer-body (definer definer-args)
  "Return the form for DEFINER.
Meant to be used in `defmacro!' and `defun!'."
  (let! (((name arglist . body) definer-args)
         ((metadata body) (oo-defun-components body))
         (symbols (cl-remove-if #'oo-list-marker-p (flatten-tree arglist))))
    (oo-wrap-forms `((,definer ,name ,arglist ,@metadata)
                     (block! (exclude! ,@symbols)))
                   body)))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `block!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (oo--definer-body 'defmacro args))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `block!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (oo--definer-body 'defun args))
;;;; looping
;; There is a huge question of whether to automatically wrap loops with
;; =block!=, but I decided to.
(defmacro for! (loop-struct &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
BODY is the body of the loop.  LOOP-STRUCT determines how `for!' loops and can
take the following forms:

- n
- (repeat n)
Evaluate BODY N times where n is an integer equal to or greater than zero.

- (VAR NUMBER)
Same as `dotimes'.

- (MATCH-FORM SEQUENCE)
Evaluate BODY for every element in sequence.  MATCH-FORM is the same as in
`let!'."
  (declare (indent 1))
  (pcase loop-struct
    ((or `(repeat ,n) (and n (pred integerp)))
     `(dotimes (_ ,n) ,@body))
    (`(,(and match-form (or (pred listp) (pred vectorp))) ,list)
     (cl-with-gensyms (elt)
       `(for! (,elt ,list)
          (let! ((,match-form ,elt))
            ,@body))))
    (`(,(and elt (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
               (dolist (,elt ,list) ,@body))
              ((sequencep ,list)
               (seq-doseq (,elt ,list) ,@body))
              ((integerp ,list)
               (dotimes (,elt ,list) ,@body))
              (t
               (error "Unknown list predicate: %S" ',loop-struct)))))))

(defalias 'dolist! 'for!)
(defalias 'loop! 'for!)
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
;;;; set!
(defun! oo--get-symbols (pattern)
  "Return."
  (when-let (flattened (flatten-tree pattern))
    (delete-dups (append (thread-last (cl-remove-if-not #'vectorp flattened)
                                      (mapcar (lambda (it) (seq-into it 'list)))
                                      (apply #'append)
                                      (oo--get-symbols))
                         (cl-remove-if (lambda (it) (or (vectorp it) (member it '(\` \,))))
                                       flattened)))))

(defmacro set! (pattern value)
  "Like `pcase-setq' but use the syntax of `let!'."
  (if (symbolp pattern)
      `(setq ,pattern ,value)
    ;; Damn I did not realize I need to know the gensym values.  I need to make
    ;; sure not to bind the gensym values.
    ;; I need flatten to also work for vectors.
    (let* ((binds (oo--to-pcase-let pattern value))
           (non-gensyms (cl-remove-if #'oo-list-marker-p (oo--get-symbols pattern)))
           (all (oo--get-symbols (mapcar #'car binds)))
           (gensyms (cl-set-difference all non-gensyms)))
      `(let ,gensyms
         ,(macroexp-progn (mapcar (apply-partially #'cons 'pcase-setq) binds))))))
;;;; threading macros 
(defmacro alet>>! (&rest forms)
  "Bind the result of `thread-last' on FORMS to `it'."
  `(set! it (thread-last ,@forms)))
(defmacro alet>! (&rest forms)
  "Bind the result of `thread-first' on FORMS to `it'."
  `(set! it (thread-first ,@forms)))
(defmacro let>>! (pattern &rest forms)
  "Bind the result of `thread-last' on FORMS to PATTERN.
See `set!'."
  (declare (indent 1))
  `(set! ,pattern (thread-last ,@forms)))
(defmacro let>! (pattern &rest forms)
  "Bind the result of `thread-first' on FORMS to PATTERN.
See `set!'."
  (declare (indent 1))
  `(set! ,pattern (thread-first ,@forms)))
;;; provide
(provide '02-base-lib)
;;; 02-base-lib.el ends here
