;;; syntactic sugar for generic modification
;; These macros are designed to provide me with "syntactic sugar" macros for
;; very common operations that take the form of ~(setf a (funcall f a
;; ...))~--where =a=, a [[https://emacsdocs.org/docs/elisp/Setting-Generalized-Variables][setfable place]], is being set to some function of
;; itself.  Essentially, these macros are more specialized variants of
;; =cl-callf=.  These macros were inspired by [[][loopy]]; specifically, by its
;; [[][accumulation clauses]].
(require 'cl-lib)
;;;; appending!
(cl-defmacro appending! (place list &key (setter 'setf))
  "Append LIST to the end of PLACE.
SETTER is the symbol of the macro or function used to do the setting."
  `(,setter ,place (append ,place ,list)))
;;;; collecting!
;; Important to note that this macro is not as efficient as pushing because it's adding to the end
;; of the list.  So this macro should be used only in non-performance-intensive code.  In
;; performance-intensive code we need the =push-nreverse= idiom.
(cl-defmacro collecting! (place item &key (setter 'setf))
  "Affix ITEM to the end of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (-snoc ,place ,item)))

(defalias 'snocing! 'collecting!)
(defalias 'affixing! 'collecting!)
;;;; incrementing! and decrementing! and counting!
;; You might be wondering why I didn't create a macro with a setter for these.
;; Well I haven't had a case yet when I've wanted to increment or decrement the
;; value symbol used in customization.
(defalias 'incrementing! 'cl-incf)
(defalias 'counting! 'cl-incf)
(defalias 'decrementing! 'cl-decf)
;;;; prepending!
;; This is less commonly used than =appending!=, but it can come in handy
;; sometimes.
(cl-defmacro prepending! (place list &key (setter 'setf))
  "Prepend LIST to beginning of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (append ,list ,place)))
;;;; maxing!
(cl-defmacro maxing! (place form &key (setter 'setf) (comparator '>))
  "Set PLACE to the greater of PLACE and FORM.
SETTER is the same as in `appending!'."
  (mmt-with-gensyms (value1 value2)
                    `(,setter ,place (let ((,value1 ,form)
                                           (,value2 ,place))
                                       (if (,comparator ,value1 ,value2) ,value1 ,value2)))))
;;;; minning!
(cl-defmacro minning! (place form &key (setter 'setf) (comparator '<))
  "Set PLACE to the lesser of PLACE and FORM.
SETTER is the same as in `appending!'. COMPARATOR is the same as in `maxing!'."
  `(maxing! ,place ,form :setter ,setter :comparator ,comparator))
;;;; concating!
(cl-defmacro concating! (place string &key (setter 'setf) separator)
  "Concat PLACE and STRING with SEPARATOR.
SETTER is the same as in `appending!'"
  `(,setter ,place (string-join (list ,place ,string) ,separator)))
;;;; adjoining!
(cl-defmacro adjoining! (place item &key test test-not key (setter 'setf))
  "Set PLACE to the value of `cl-adjoin'.
SETTER is the same as in `appending!'."
  `(,setter ,place (cl-adjoin ,item ,place :test ,test :test-not ,test-not :key ,key)))
;;;; pushing!
;; I know =push= already exists.  But I want a variant of push that can be used
;; with the =block!= macro.
(cl-defmacro pushing! (place item &key (setter 'setf))
  "Cons ITEM to PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (cons ,item ,place)))
;;;; adjoin!
;; To configure variables I don't use the standard =setq=--at least not directly.
;; Instead, I use =set!=.  Adjoining is one of the most common operations done to
;; lisp symbols when configuring Emacs.

;; Something I was always confused about was why adjoin instead of just using
;; =push=.  The latter is more performant; however I don't think that's.  The best reason I could
;; think of is that sometimes you want to re-evaluate parts of your configuration
;; and in that case it is more convenient to have =adjoin= over =push=.
(cl-defmacro adjoin! (place value &key test key test-not)
  "Adjoin value to place.
Same as `adjoining!' but use `set!' as the setter.  Meant to be used for
customizing variables."
  `(adjoining! ,place ,value :test ,test :key ,key :test-not ,test-not :setter set!))
;;;; unioning!
(cl-defmacro unioning! (place list &key test test-not key (setter 'setf))
  "Set PLACE to the union of PLACE and FORM.
SETTER is the same as in `appending!'."
  `(,setter ,place (cl-union ,place ,list :test ,test :test-not ,test-not :key ,key)))
;;;; take!
(defmacro take! (pred place)
  (cl-with-gensyms (taken)
    (cl-once-only (pred)
      `(let (,taken)
	     (pcase ,pred
	       ((pred integerp)
	        (dotimes (_ ,pred)
	          (collecting! ,taken (pop ,place))))
	       (_
	        (while (and ,place (funcall ,pred (car ,place)))
	          (collecting! ,taken (pop ,place)))))
	     ,taken))))

;;; provide
(provide 'oo-modification-macros)
