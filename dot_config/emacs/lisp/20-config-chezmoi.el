;; TODO: integrate with =defhook= once I figure out how that is going to work.
;; TODO: I need to figure out how to lazy require chezmoi and f.  I get the
;; error that the features have not been loaded.
(defhook! after-save-hook&chezmoi-maybe-write-file ()
  (set! file (expand-file-name (buffer-file-name)))
  (when (and file (member file (mapcar #'expand-file-name (chezmoi-managed))))
    (chezmoi-write file)))
