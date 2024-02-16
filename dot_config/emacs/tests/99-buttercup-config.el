(require '02-base-lib)
(require 'buttercup)

(defmacro describe! (&rest body)
  "Same as `it' but wraps body with `block!'."
  `(describe (block! ,@body)))
