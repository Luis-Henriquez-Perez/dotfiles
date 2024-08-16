;; http://xahlee.info/emacs/emacs/elisp_htmlize.html
;; This is recommended by Xah Lee to prevent htmlize from creating "ugly HTML
;; entities".
(opt! htmlize-convert-nonascii-to-entities nil)
(opt! htmlize-html-charset "utf-8")
(opt! htmlize-untabify nil)
