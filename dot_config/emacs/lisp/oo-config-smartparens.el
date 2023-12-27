(sp-local-pair sp-lisp-modes "'" nil :actions nil)

(sp-local-pair sp-lisp-modes "`" "'" :when '(sp-in-string-p sp-in-comment-p))

(sp-local-pair 'minibuffer-mode "'" nil :actions nil)
(sp-local-pair 'minibuffer-mode "`" nil :actions nil)

(require 'smartparens-config)
