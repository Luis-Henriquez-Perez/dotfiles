(oo-add-hook 'on-first-file-hook #'save-place-mode)

(set! save-place-file (concat oo-cache-dir "saveplace"))
(set! save-place-limit nil)
