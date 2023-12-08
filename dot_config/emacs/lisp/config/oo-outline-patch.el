;;; patch promotion and demotion of headlines
;; This file is intended as a patch to `outline-promote' which doesn't seem to
;; be working properly.  The problem is that when you try to promote

;; What I don't understand is why =outline-promote= gets the appropriate level
;; by going up a heading instead of just computing it based on the current one.
;; See the [[][]].
(defun oo-outline-promote-advice (orig-fn &rest args)
  ;; If there's an error in `outline-up-heading' then do my own
  ;; promotion or demotion.  I should be able to do it by just traversing the
  ;; subheadings and promoting or demoting each level by 1.
  (condition-case err
      (apply orig-fn args)
    ;; What 
    ()
    ()))

(provide 'oo-outline-configuration)
