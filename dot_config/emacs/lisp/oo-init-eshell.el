(oo-popup-at-bottom "\\*eshell")

(oo-bind 'oo-app-map "e" #'eshell)

;;; Specify locations for files
(setq eshell-directory-name (concat oo-cache-dir "eshell/"))

(set! eshell-history-file-name (concat eshell-directory-name "history"))

;;; don't display a banner message
(set! eshell-banner-message "")

;;; eshell history
(set! eshell-hist-ignoredups t)

;;; prevent eshell from printing out messages on load
;; Eshell prints various messages about loading modules.  These messages
;; originate from the function [[][eshell-unload-all-modules]].  I would rather
;; not see these messages.
(oo-add-advice #'eshell-mode :around #'oo-funcall-silently)

;; At first I thought the culprit was this function, but I was wrong.  The
;; printing comes from =eshell-mode=.  In any case, however, I silence it as
;; well.
(oo-add-advice #'eshell-unload-all-modules :around #'oo-funcall-silently)

