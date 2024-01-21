;; I've changed my mind about having all these tiny files, but I do not want to
;; have an org file.  My initial reasoning with the tiny files was that they are
;; consistent and modular.  Consistent because the files are regularly named and
;; very predictable--as in, I find a new package I like I create a file
;; =oo-init-PACKAGE.el= for its setup code.  Modular because I can just remove
;; the file from the ~/.config/emacs/lisp/ directory if I do not want it
;; anymore.  However, this is at a cost.  It is less convenient to search and
;; bulk modify the code when its segregated into so many files than it is if it
;; was all just one in file.  Additionally, it does not perform as well.

;; 1. loop through files in the directory
;; 2. get their contents
;; 3. append them to setup.el as well as a comment on top
(block! nil
  (let! regexp "\\`oo-init-\\(.+?\\)\\.el\\'")
  (let! setup-buffer (find-file-noselect (expand-file-name "~/.config/emacs/setup.el")))
  (let! dir (expand-file-name "~/.config/emacs/lisp/"))
  (with-current-buffer setup-buffer
    (for! (init-file (directory-files dir t regexp))
      (let! fname (file-name-nondirectory init-file))
      (string-match regexp fname)
      (let! package (match-string-no-properties 1 fname))
      (goto-char (point-max))
      (insert (format ";;;;; %s\n" package))
      (insert-file-contents init-file nil))))
