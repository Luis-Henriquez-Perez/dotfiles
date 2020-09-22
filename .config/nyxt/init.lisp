;; (in-package nyxt)

;; (define-command org-capture (&optional (buffer (current-buffer)))
;;   "Org-capture current page."
;;   (eval-in-emacs
;;    `(org-link-set-parameters
;;      "next"
;;      :store (lambda ()
;;               (org-store-link-props
;;                :type "next"
;;                :link ,(url buffer)
;;                :description ,(title buffer))))
;;    `(org-capture)))

; $HOME/.config/next/init.lisp
;; set vim keys
(in-package :next-user)

(defclass my-buffer (buffer)
  ((default-modes :initform
     (cons 'vi-normal-mode (get-default 'buffer 'default-modes)))))

(setf *buffer-class* 'my-buffer)

;; (define-key *my-keymap* "C-M-o" 'org-capture)

;;

;; (define-configuration buffer ((default-modes (append '(vi-normal-mode) %slot-default))))
