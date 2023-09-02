(require 'mmt)
(require 'gv)

(defmacro updating! (place fn &rest args)
  "Set PLACE to the result of `(apply FN PLACE args)'."
  `(setf ,place (apply ,fn ,place ,args)))

(defmacro updating-it! (place expr)
  "Set PLACE to the value of EXPR.
PLACE is bound to `it' for the duration of EXPR."
  `(setf ,place (alet ,place ,expr)))

(defalias 'it-updating! 'updating-it!)

(defmacro appending! (place list)
  "Append LIST to the end of PLACE."
  (declare (indent 1))
  `(setf ,place (append ,place ,list)))

(defalias 'incrementing! 'cl-incf)

(defmacro collecting! (place item)
  "Affix ITEM to the end of PLACE."
  (declare (indent defun))
  `(setf ,place (append ,place (list ,item))))

(defalias 'snocing! 'collecting!)
(defalias 'affixing! 'collecting!)

(defmacro finding! (place expr)
  "Set VAR to the result of \(or VAR EXPR\)."
  `(setf ,place (or ,place ,expr)))

(defmacro counting! (place &optional x)
  "Increment PLACE by 1 if FORM is non-nil."
  `(cl-incf ,place (or ,x 1)))

(defmacro prepending! (place list)
  "Prepend LIST to beginning of PLACE."
  `(setf ,place (append ,list ,place)))

(defmacro maxing! (var form)
  "Set VAR to."
  (let ((form-var (gensym "form-var-")))
    `(let ((,form-var ,form))
       (setq ,var (if (> ,form-var ,var) ,form-var ,var)))))

(defalias 'decrementing! 'cl-decf)

(defmacro concating! (place string &optional sep)
  "Concat PLACE and STRING with optional separator, SEP."
  `(setf ,place (string-join (list ,place ,string) ,sep)))

(defmacro unioning! (place form &optional test)
  "Set PLACE to the union of PLACE and FORM."
  `(updating! ,place #'cl-union ,form :test ,(or test '#'equal)))

(defmacro updating! (place fn &rest args)
  "Set PLACE to the result of `(apply FN PLACE args)'."
  `(cl-callf2 funcall ,fn ,place ,@args))

(defmacro take! (pred place)
  (mmt-with-gensyms (taken)
    (mmt-once-only (pred)
      `(let (,taken)
	     (pcase ,pred
	       ((pred integerp)
	        (dotimes (_ ,pred)
	          (collecting! ,taken (pop ,place))))
	       (_
	        (while (and ,place (funcall ,pred (car ,place)))
	          (collecting! ,taken (pop ,place)))))
	     ,taken))))

(defmacro adjoining! (place item &optional test)
  "Set PLACE to ITEM consed onto the front of PLACE only if it's not already there."
  `(setf ,place (cl-adjoin ,item ,place :test ,(or test '#'equal))))

(defmacro pushing! (place form)
  "Same as `push', except the arguments are switched.
Designed to be used in `block!'."
  (declare (indent 1))
  `(push ,form ,place))

(defmacro adding-to-list! (var value &optional append compare-fn)
  `(set! ,var (add-to-list ',var ,value ,append ,compare-fn)))

(provide 'oo-modification-macros)
