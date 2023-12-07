;; **** convert a buffer to html and then convert that to an image
;; :PROPERTIES:
;; :ID:       20230802T153555.835461
;; :END:
;; The =wkhtmltoimage= command converts an =html= file to am image.  The package
;; [[https://github.com/hniksic/emacs-htmlize][htmlize]] provides a function called [[file:snapshots/helpful-command--htmlize-buffer.png][htmlize-buffer]] that produces a buffer with html code rendered (by a web browser
;; for example) to produce a display of that buffer.  So what I'm going to do
;; generate such a buffer with =htmlize=, save it to a file because =wkhtmltoimage=
;; doesn't know about buffers, and then use =wkhtmltoimage= to convert it to an
;; image. There is a built-in function to convert a buffer to html called
;; htmlfontify, but as of writing this it
;; [[file:snapshots/backtrace-for-htmlfontify-buffer.png][seems to be bugged]].  I wanted to use it because it seemed to use the current font I was using.
(defun! oo-snapshot-buffer (buffer image-file)
  "Create image of BUFFER as IMAGE-FILE"
  (cl-assert (executable-find "wkhtmltoimage"))
  (with-current-buffer buffer
    (with-current-buffer (htmlize-buffer)
      (let! contents (buffer-string))))
  (let! html-file (shut-up (make-temp-file "snapshot" nil ".html" contents)))
  (call-process "wkhtmltoimage" nil nil nil html-file image-file)
  (delete-file html-file nil))
;; **** make a command for taking a snapshot of the current buffer
;; :PROPERTIES:
;; :ID:       20230802T163408.745309
;; :END:
;; You might ask why I don't just take a screenshot.  As far as I know, I don't
;; think it's possible to take a screenshot of a buffer that's not displayed on the
;; screen; and sometimes I want to do that.  For example, it would be nice to
;; take a snapshot (I call it this because it's not from the screen) of all
;; =helpful= buffers I've looked up.

;; The purpose of this command is to let me take buffer snapshots rapidly.  It
;; should just be: invoke a keybinding and--bang!--a snapshot in my snapshot
;; directory.  Most of the time I'm fine with an automatically generated name. When
;; I'm not I'll specifically rename the file myself.
(defun! oo-take-buffer-snapshot ()
  "Take a snapshot of the current buffer.
Produce a png image of the current buffer and add it into."
  (interactive)
  (let! image (expand-file-name (format "%s.png" (oo-legal-filename (buffer-name))) oo-snapshot-dir))
  (when (file-exists-p image)
    (unless (y-or-n-p "Snapshot already exists, replace it?")
      (return!)))
  (oo-snapshot-buffer (current-buffer) image))
