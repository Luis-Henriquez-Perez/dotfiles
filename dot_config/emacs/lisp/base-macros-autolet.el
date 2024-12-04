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
  "Exit `autolet!' and return VALUE.
Throw a `return!' signal, immediately terminating the `autolet!' execution and
returning VALUE from the surrounding context."
  `(throw 'return! ,value))

(defmacro done! ()
  "A shorthand for `(return! nil)'."
  `(return! nil))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
Inside an `autolet!' form exit the current loop and return VALUE."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of a loop.
Inside an `autolet' form, throw a `continue!' signal to end the current
iteration and move to the next."
  `(throw 'continue! nil))

(defalias 'skip! 'continue!)
;;;; let-binding stubs
(defmacro stub! (name args &rest body)
  "Indicator for defining local functions via `cl-flet' in `autolet!' forms."
  (declare (indent defun))
  (ignore name args body))
(defalias 'macrolet! 'stub! "Indicator for defining local macros via
`cl-macrolet' in `autolet!' forms.")
(defalias 'mlet! 'macrolet!)
(defalias 'flet! 'stub! "Same as `stub!'.")
(defalias 'noflet! 'stub! "Indicator for temporary overriding function
definitions.")
(defalias 'nflet! 'stub! "Same as `noflet!'")
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
;;;; helpers
(defmacro oo--autolet-inits (bodysym)
  "Produce a list of init and noinits from BODYSYM.
Extract initialization and exclusion bindings from BODYSYM.

This macro processes BODYSYM to identify `:init` and `:noinit` forms.
- `:init BINDINGS`: A list of symbol bindings, formatted like `let*`.
- `:noinit SYMS`: A list of symbols to exclude from let-bindings."
  (let ((inits (gensym "inits"))
        (noinits (gensym "noinits"))
        (letbind (gensym "letbind")))
    `(let (,inits ,noinits)
       (while (pcase ,bodysym
                (`(:init ,(pred listp) . ,(guard t))
                 (pop ,bodysym)
                 (dolist (,letbind (pop ,bodysym))
                   (pcase ,letbind
                     ((pred symbolp)
                      (push (list ,letbind nil) ,inits))
                     (`(,_)
                      (push (append ,letbind (list nil)) ,inits))
                     (`(,_ ,_)
                      (push ,letbind ,inits))))
                 t)
                (`(:noinit ,(pred listp) . ,(guard t))
                 (pop ,bodysym)
                 (setq ,noinits (append ,noinits (pop ,bodysym)))
                 t)))
       (list ,inits ,noinits))))

(defun oo--autolet-data (body)
  "Return let-bindings and processed forms used in `autolet!'.
Identify and collect symbols needed for let-binding."
  (pcase-let ((`(,init ,noinit) (oo--autolet-inits body))
              (bindings nil)
              (lets '((mlet! . cl-macrolet)
                      (macrolet! . cl-macrolet)
                      (nflet! . lef!)
                      (noflet! . lef!)
                      (flet! . cl-flet)
                      (stub! . cl-flet)
                      (label! . cl-labels)
                      (labels! . cl-labels))))
    (cl-flet ((quote-symbol-p (x) (memq x '(quote function backquote cl-function)))
              (loop-symbol-p (x) (memq x '(while dolist dotimes for! dolist!)))
              (ing-symbol-p (x) (and (symbolp x) (string-match-p "ing!$" (symbol-name x))))
              (letbind-symbol-p (x) (assoc x lets))
              (should-remove-p (x) (or (member (car x) noinit) (assoc (car x) init))))
      (cl-labels ((process-form (form)
                    (pcase form
                      ;; Leave quoted forms as-is.
                      (`(,(pred quote-symbol-p) . ,_)
                       form)
                      ;; Match `(set! VAR VALUE)` and collect VARIABLE.
                      (`(set! ,pattern ,_ . ,(guard t))
                       (if (symbolp pattern)
                           (cl-pushnew (list pattern nil) bindings :key #'car)
                         (dolist (symbol (reverse (oo--set-flatten pattern)))
                           (cl-pushnew (list symbol nil) bindings :key #'car)))
                       form)
                      ;; Surround loops with a catch.
                      (`(,(and loop (pred loop-symbol-p)) ,pred . ,(and body (guard t)))
                       `(catch 'break! (,loop ,pred (catch 'continue! ,@(process-form body)))))
                      ;; Properly initialize variables in ingmacro declarations.
                      ;; Just for brevity I use string-match to check instead
                      ;; of listing all my ing macros but there has been a clash
                      ;; with org-ml that uses some macros that end in "ing!".
                      (`(,(and name (pred ing-symbol-p)) ,symbol . ,(guard t))
                       (cl-case name
                         ((maxing! maximizing!)
                          (cl-pushnew `(,symbol most-negative-fixnum) bindings))
                         ((minning! minimizing!)
                          (cl-pushnew `(,symbol most-positive-fixnum) bindings))
                         ((summing! adding! counting!)
                          (cl-pushnew `(,symbol 0) bindings))
                         (t
                          (cl-pushnew `(,symbol nil) bindings :key #'car)))
                       form)
                      ;; Handle special let shortcuts.
                      (`((,(and macro (pred letbind-symbol-p)) . ,args) . ,(and rest (guard t)))
                       `((,(alist-get macro lets) ((,@args)) ,@(process-form rest))))
                      ((pred null)
                       form)
                      ;; Recurse into lists.
                      ((pred listp)
                       (cons (process-form (car form)) (process-form (cdr form))))
                      ;; Leave other forms untouched.
                      (_
                       form))))
        (setq body (process-form body)))
      (setq bindings (append init (cl-remove-if #'should-remove-p bindings))))
    (list bindings body)))
;;;; main macro
(defmacro autolet! (&rest body)
  "Dynamically let-bind symbols and modify forms in BODY.

Process BODY by recognizing special forms and keywords for dynamically
let-binding symbols, automatically wrapping forms and enhancing the control flow
of loops.

Keywords:
:init BINDINGS    Predefine symbol bindings.  Follow the same format as `let*'.
This takes precedence over other let-binding indicators.
:noinit SYMS      Do not let bind any symbols in SYMS, regardless of whether
those symbols where specified by dynamic let-binding indicators.

Dynamic let-binding:
(set! SYM _)      Let bind SYM to nil.
(maxing! SYM _)   Let bind SYM to `most-negative-fixnum'.
(minning! SYM _)  Let bind SYM to `most-positive-fixnum'.
(counting! SYM _) Let bind SYM to 0.
(...ing! SYM VAL) Let bind SYM to nil.

Wrapping forms:
(mlet!|macrolet! NAME ARGS . BODY) Wrap subsequent forms with
`(cl-macrolet ((NAME ARGS . BODY)))'.
(stub!|flet! NAME ARGS . BODY)     Same as macrolet but use `cl-letf'.
(nflet!|noflet! NAME ARGS . BODY)  Same as `stub!' but use `lef!'.
(label!|labels! NAME ARGS . BODY)  Same as `stub!' but use `cl-labels'.

Enhanced looping control flow:
(while|dotimes|dolist CONDITION . BODY) Replace with
`(catch \='return! (LOOP CONDITION (catch \='break! BODY)))'."
  (pcase-let ((`(,bindings ,body) (oo--autolet-data body)))
    `(let ,bindings (catch 'return! ,@body))))
;;; provide
(provide 'base-macros-autolet)
;;; base-macros-autolet.el ends here
