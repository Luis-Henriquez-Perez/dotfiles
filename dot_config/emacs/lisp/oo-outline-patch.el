;; This file is intended as a patch to `outline-promote' which doesn't seem to
;; be working properly.  The problem is that when you try to promote

;; What I don't understand is why =outline-promote= gets the appropriate level
;; by going up a heading instead of just computing it based on the current one.
;; See the [[][]].
(defun oo-outline-promote-advice (orig-fn &rest args)
  ;; If there's an error in `outline-up-heading' then do my own
  ;; promotion or demotion.  I should be able to do it by just traversing the
  ;; subheadings and promoting or demoting each level by 1.
  (cond
   ((eq which 'region)
    (outline-map-region #'outline-promote (region-beginning) (region-end)))
   (which
    (outline-map-region #'outline-promote
			            (point)
			            (save-excursion (outline-get-next-sibling) (point))))
   (t
    (outline-back-to-heading t)
    (let* ((head (match-string-no-properties 0))
	       (level (save-match-data (funcall outline-level)))
	       (up-head (or (outline-head-from-level (1- level) head)
			            ;; Use the parent heading, if it is really
			            ;; one level less.
			            (save-excursion
			              (save-match-data
			                (outline-up-heading 1 t)
			                (and (= (1- level) (funcall outline-level))
				                 (match-string-no-properties 0))))
                        ;; Bummer!! There is no lower level heading.
                        (outline-invent-heading head 'up))))

      (unless (rassoc level outline-heading-alist)
	    (push (cons head level) outline-heading-alist))

      (replace-match up-head nil t)))))

(provide 'oo-outline-promote-patch)
