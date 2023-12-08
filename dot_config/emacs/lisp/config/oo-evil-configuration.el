;;; settings
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(set! evil-echo-state nil)

;; Whether the cursor is moved backwards when exiting insert state.
(set! evil-move-cursor-back nil)

;; It's confusing to if the cursor is allowed to move beyond the end of the line.
(set! evil-move-beyond-eol nil)

;;; cursor color
