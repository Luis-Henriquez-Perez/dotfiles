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

(defvar *my-keymap* (make-keymap "my-map")
  "Keymap for `my-mode'.")

(define-command org-capture (&optional (buffer (current-buffer)))
  "Org-capture current page."
  (eval-in-emacs
   `(org-link-set-parameters
     "next"
     :store (lambda ()
              (org-store-link-props
               :type "next"
               :link ,(url buffer)
               :description ,(title buffer))))
   `(org-capture)))

(define-key *my-keymap* "C-o" 'org-capture)

(in-package :next-user)

(defclass my-buffer (buffer)
  ((default-modes :initform
     (cons 'vi-normal-mode (get-default 'buffer 'default-modes)))))

(setf *buffer-class* 'my-buffer)
