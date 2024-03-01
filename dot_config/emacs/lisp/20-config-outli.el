;; outli doesn't refontify headlines after theme change.  So if you change a
;; theme, the headlines will remain in the color of the previous theme.
(defadvice! load-theme@ARfontify-outli (fn &rest args)
  (let (outli-buffers)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when outli-mode
          (outli-mode -1)
          (push buffer outli-buffers))))
    (unwind-protect (apply fn args)
      (dolist (buffer outli-buffers)
        (with-current-buffer buffer
          (outli-mode 1))))))

;; (defun oo-print-fn-and-args (orig-fn arg1 arg2)
;;   (message "(outli-fontify-headlines %S %S)" arg1 arg2)
;;   (funcall orig-fn arg1 arg2))

;; (advice-add #'outli-fontify-headlines :around #'oo-print-fn-and-args)
;; (advice-remove 'outli-fontify-headlines #'oo-print-fn-and-args)

(setf (cl-fourth (assoc 'emacs-lisp-mode outli-heading-config)) nil)

(provide '20-config-outli)
