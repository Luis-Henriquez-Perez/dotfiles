;; For helpful information see [[][]]
;; (grugru-define-on-major-mode)

;;; grugru

;;;; define opposites
;; Define grugru globally.  This is applied in all buffers.
(grugru-define-global 'symbol '("yes" "no"))

(grugru-define-global 'symbol '("up" "down"))

(grugru-define-global 'symbol '("left" "right"))

(grugru-define-global 'symbol '("wrong" "right"))

(grugru-define-global 'symbol '("red" "orange" "yellow" "green" "blue" "indigo" "violet"))

(grugru-define-global 'symbol '("front" "back"))

(grugru-define-global 'symbol '("inner" "outer"))

;;;; emacs-lisp

(grugru-define-on-major-mode '(emacs-lisp) 'word '("let" "let*" "let!" "-let"))

(provide '20-config-grugru)
