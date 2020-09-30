(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string (cl-ppcre:regex-replace-all
                        ;; Discard the package prefix.
                        "next-user::?"
                        (write-to-string
                         `(progn ,@s-exps) :case :downcase)
                        "")))
    (log:debug "Sending to Emacs: ~s" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(in-package :next-user)

(defclass my-buffer (buffer)
  ((default-modes :initform
     (cons 'vi-normal-mode (get-default 'buffer 'default-modes)))))

(setf *buffer-class* 'my-buffer)
