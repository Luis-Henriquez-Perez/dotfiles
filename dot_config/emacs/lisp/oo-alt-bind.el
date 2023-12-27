(require 'oo-)

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

(defun! oo-alt-bind (map orig alt &optional condition)
  "Remap keys bound to ORIG so ALT is called if CONDITION returns non-nil.
ORIG and ALT are command symbols.  CONDITION is a function that returns non-nil
when ALT should be invoked instead of ORIG."
  (flet! oo-when-fn (condition fn)
    `(lambda (&rest _) (when (funcall #',condition) #',alt)))
  (push (oo-when-fn (or condition #'always) alt) (gethash orig oo-alternate-commands))
  (define-key map `[remap ,orig] `(menu-item "" ,orig :filter oo-alternate-command-choose-fn)))

;; Extend =oo-bind= to use =alt-bind= with =:alt= keyword.

(provide 'oo-base-alt-bind)
