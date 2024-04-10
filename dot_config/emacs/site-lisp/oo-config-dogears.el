;;; Make =dogears-go= display more readable information
;; The function =dogears-go= displays.

;; First step is to see what is actually in =dogears-list so that I
;; know what I have to work with.  The following form shows what
;; information is recorded by dogears when it makes a bookmark.

;; (cl-prettyprint (mapcar #'car (cdr (car dogears-list))))

;; (line
;;  mode
;;  within
;;  buffer
;;  manualp
;;  filename
;;  front-context-string
;;  rear-context-string
;;  position
;;  last-modified)


