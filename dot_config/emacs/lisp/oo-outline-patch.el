;; This file is intended as a patch to `outline-promote' which doesn't seem to
;; be working properly.  The problem is that when you try to promote

(defun oo-outline-promote-advice (orig-fn &rest args)
  ())

(provide 'oo-outline-promote-patch)
