;;; base-macros-autolet.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/
;;
;; This file is not part of GNU Emacs.
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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file provides a macro that automatically let bindings symbol based on
;; certain indicators in its body (among other things).  This macro was created
;; to.
;;
;; Sometimes you do not want symbol to be auto let-bound to nil, you actually
;; want to just modify the original symbol without let-binding it at all.  In
;; that case use `:noinit' which tells `autolet!' not to bind specified symbols
;; at all.  Other times you want a symbol to be bound to something else than the
;; default.  For example, counting! starts at 0 by default but maybe you want to
;; start at 10, in that case you can do `:init' ((count 10)).  I suppose init
;; can be used as a single-line alternative to `let*'.
;;
;;; Code:
;;;; requirements
(require 'cl-lib)
(require 'pcase)
(require 'base-macros-let)
(require 'base-macros-lef)
;;;; control flow macros
(defmacro return! (&optional value)
  "Exit `autolet!' and return VALUE."
  `(throw 'return! ,value))

(defmacro done! ()
  "Same as `return!' but always throw nil.
This is meant to be used within an `autolet!' body."
  `(return! nil))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
This is meant to be used inside of loop from within an `autolet!' body."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of loop."
  `(throw 'continue! nil))
(defalias 'skip! 'continue!)
;;;; let-binding stubs
;; This wrapper is mainly for.
(defmacro stub! (name args &rest body)
  "Define a local function definition with `cl-flet'.
NAME, ARGS and BODY are the same as in `defun'.
Must be used in `autolet!'."
  (declare (indent defun))
  (ignore name args body))
(defalias 'flet! 'stub!)
(defalias 'noflet! 'stub!)
(defalias 'nflet! 'stub!)
(defalias 'mlet! 'stub!)
(defalias 'macrolet! 'stub!)
;;;; accumulation macros
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
  `(,setter ,place (cl-adjoin ,item ,place :test ,test :test-not ,test-not :key ,key)))

(defalias 'adjoin! 'adjoining!)

;; I know =push= already exists.  But I want a variant of push that can be used
;; with the =autolet!= macro.
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
  `(,setter ,place (cl-union ,place ,list :test ,test :test-not ,test-not :key ,key)))

(defalias 'adding! 'cl-incf)
(defalias 'summing! 'cl-incf)
(defalias 'subtracting! 'cl-decf)
(defalias 'minusing! 'cl-decf)

(defmacro ignore! (&rest _) nil)

(defmacro autolet-inits! (sym)
  (let ((inits (gensym "inits"))
        (noinits (gensym "noinits"))
        (letbind (gensym "letbind")))
    `(let (,inits ,noinits)
       (while (pcase ,sym
                (`(:init ,(pred listp) . ,(guard t))
                 (pop ,sym)
                 (dolist (,letbind (pop ,sym))
                   (pcase ,letbind
                     ((pred symbolp)
                      (push (list ,letbind nil) ,inits))
                     (`(,_)
                      (push (append ,letbind (list nil)) ,inits))
                     (`(,_ ,_)
                      (push ,letbind ,inits))))
                 t)
                (`(:noinit ,(pred listp) . ,(guard t))
                 (pop ,sym)
                 (setq ,noinits (append ,noinits (pop ,sym)))
                 t)))
       (list ,inits ,noinits))))
;;;; helper
(defun oo--autolet-data (body)
  "Return a list of bindings and forms."
  (pcase-let ((`(,init ,noinit) (autolet-inits! body))
              (stack (list (list nil nil body)))
              (i 0)
              (bindings nil)
              (done-p nil)
              (letbind-alist '((macrolet! . cl-macrolet)
                               (mlet! . cl-macrolet)
                               (nflet! . lef!)
                               (noflet! . lef!)
                               (flet! . cl-flet)
                               (stub! . cl-flet)
                               (label! . cl-labels)
                               (labels! . cl-labels))))
    (cl-flet* ((quote-symbol-p (x) (memq x '(quote function backquote cl-function)))
               (loop-symbol-p (x) (memq x '(while dolist dotimes for! dolist!)))
               (ing-symbol-p (x) (and (symbolp x) (string-match-p "ing!$" (symbol-name x))))
               (letbind-symbol-p (x) (assoc x letbind-alist))
               (letbind-wrap (letsym args _ cdr) `((,(alist-get letsym letbind-alist) ((,@args)) ,@cdr)))
               (letbind-wrap-fn (letsym args) (apply-partially #'letbind-wrap letsym args))
               (vcons (car cdr) (vconcat (cons car cdr)))
               (loop-join (loop pred car cdr) `(catch 'break! (,loop ,pred (catch 'continue! ,@(cons car cdr)))))
               (loop-join-fn (loop pred) (apply-partially #'loop-join loop pred))
               (should-remove-p (x) (or (member (car x) noinit) (assoc (car x) init))))
      (while (not done-p)
        (cl-incf i)
        (pcase stack
          (`((t nil ,form))
           ;; (ignore! (message "pred ((t nil form)) -> t"))
           ;; (ignore! (message "done"))
           (setq done-p t)
           (ignore! (message "stack -> %s" stack)))
          (`((t ,fn ,cdr) (t ,join-fn ,car) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((t ,fn ,cdr) (t ,join-fn ,car) . ,(guard t))))
           (setq stack (cddr stack))
           (push (list t fn (funcall join-fn car cdr)) stack)
           (ignore! (message "stack -> %s" stack)))
          (`((t ,_ ,_) (nil ,_ ,_) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((t ,_ ,_) (nil ,_ ,_) . ,(guard t))))
           (push (pop (cdr stack)) stack)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,join-fn ,(and (pred vectorp) vector)) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil ,join-fn ,(and (pred vectorp) vector)) . ,(guard t))))
           (pop stack)
           (push (list nil join-fn (append (seq-rest vector) nil)) stack)
           (push (list nil vcons (seq-first vector)) stack)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,_ nil) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil nil nil) . ,(guard t))))
           (setf (caar stack) t)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,_ (,(pred quote-symbol-p) . ,_)) . (guard t))
           (ignore! (message "pred %S -> t" '`((nil ,_ (,(pred quote-symbol-p) . ,_)) . (guard t))))
           (setf (caar stack) t)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,_ (,(and name (pred ing-symbol-p)) ,symbol . ,(guard t))) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil ,_ (,(and name (pred ing-symbol-p)) ,symbol . ,(guard t))) . ,(guard t))))
           (cl-case name
             ((maxing! maximizing!)
              (push `(,symbol most-negative-fixnum) bindings))
             ((minning! minimizing!)
              (push `(,symbol most-positive-fixnum) bindings))
             ((summing! adding! counting!)
              (push `(,symbol 0) bindings))
             (t
              (push `(,symbol nil) bindings)))
           (setf (caar stack) t)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,fn (,(and loop (pred loop-symbol-p)) ,pred . ,(and body (guard t)))) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil ,fn (,(and loop (pred loop-symbol-p)) ,pred . ,(and body (guard t)))) . ,(guard t))))
           (setq stack (cdr stack))
           (push (list nil fn body) stack)
           (push (list nil (loop-join-fn loop pred) nil) stack)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,_ (set! ,pattern ,_ . ,(guard t))) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil ,_ (set! ,pattern ,_ . ,(guard t))) . ,(guard t))))
           (if (symbolp pattern)
               (cl-pushnew (list pattern nil) bindings :key #'car)
             (dolist (sym (reverse (oo--set-flatten pattern)))
               (cl-pushnew (list sym nil) bindings :key #'car)))
           (setf (caar stack) t)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,join-fn ((,(and letsym (pred letbind-symbol-p)) . ,args) . ,(and rest (guard t)))) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil ,join-fn ((,(and letsym (pred letbind-symbol-p)) . ,args) . ,(and rest (guard t)))) . ,(guard t))))
           (pop stack)
           (push (list nil join-fn rest) stack)
           (push (list nil (letbind-wrap-fn letsym args) nil) stack)
           (ignore! (message "stack -> %s" stack)))
          (`((nil ,_ ,(pred listp)) . ,(guard t))
           (ignore! (message "pred %S -> t" '`((nil nil ,(pred listp)) . ,(guard t))))
           (push (list nil #'cons (pop (cl-third (car stack)))) stack)
           (ignore! (message "stack -> %s" stack)))
          (_
           (ignore! (message "pred %S -> t" '_))
           (setf (cl-first (car stack)) t)
           (ignore! (message "stack -> %s" stack)))))
      (setq bindings (append init (cl-remove-if #'should-remove-p bindings)))
      (setq body (cl-third (car stack)))
      (message "iteration -> %s" (cl-incf i))
      (list bindings body))))
;;;; main macro
(defmacro autolet! (&rest body)
  "Let-bind symbols and transform forms based on indicators in BODY.

Process BODY, recognizing special forms and keywords to dynamically bind
variables or modify expressions.

:init BINDINGS    Predefine symbol bindings.  BINDINGS follow the same format as `let*'.
:noinit SYMS      Do not let bind any symbols in SYMS.  This takes precedence.

(set! SYM _)      Let bind SYM to nil.
(maxing! SYM _)   Let bind SYM to `most-negative-fixnum'.
(minning! SYM _)  Let bind SYM to `most-positive-fixnum'.
(counting! SYM _) Let bind SYM to 0.
(...ing! SYM VAL) Let bind SYM to nil.

(stub!|flet! NAME ARGS . BODY) Wrap subsequent forms with `(cl-flet ((NAME ARGS . BODY)))'.
(nflet!|noflet! NAME ARGS . BODY) Same as `stub!' but use `lef!'.
(label!|labels! NAME ARGS . BODY) Same as `stub!' but use `cl-labels'.

(LOOP CONDITION . BODY) Replace with `(catch 'return! (LOOP CONDITION (catch 'break! BODY)))'."
  (pcase-let ((`(,letbinds ,body) (oo--autolet-data body)))
    (if letbinds
        `(catch 'return! (let* ,letbinds ,@body))
      `(catch 'return! ,@body))))
;;; provide
(provide 'base-macros-autolet)
;;; base-macros-autolet.el ends here
