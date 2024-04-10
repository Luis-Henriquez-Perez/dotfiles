;; There's no function to skip a node and I can't see a quick/clever way to do it
;; with the existing functions.  I want to be where I would be if I had deleted the
;; node, but I don't want the node itself to be deleted.  If there is a right node
;; in the same level skipping is tantamount to [[][treepy-right]].
(defun +treepy-skip (zipper)
  "Skip the current node."
  (let ((orig zipper))
    (while (and (not (treepy-right zipper)) (treepy-up zipper))
      (setq zipper (treepy-up zipper)))
    (if (treepy-right zipper)
        (setq zipper (treepy-right zipper))
      ;; If we've reached the top level, that means there is no next node.  So
      ;; let's go back to where we were and go next until we reach the end.
      (setq zipper orig)
      (while (not (treepy-end-p zipper))
        (setq zipper (treepy-next zipper)))
      zipper)))


;; This will also address fixing the fact that =treepy-delete= doesn't seem to work.
