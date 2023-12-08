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
;; Changing the cursor shape and color depending on the state is a convenient and
;; asthetically pleasing way of determining which state you're in.  Some add some
;; modeline indicator for this but I find that the cursor suffices.
(defun oo-set-default-evil-cursors (&rest _)
  "Set the evil cursors."
  (set! evil-insert-state-cursor '((bar . 3) "chartreuse3"))
  (set! evil-emacs-state-cursor '((bar . 3) "SkyBlue2"))
  (set! evil-normal-state-cursor '(box "DarkGoldenrod2"))
  (set! evil-visual-state-cursor '((hollow) "dark gray"))
  (set! evil-operator-state-cursor '((hbar . 10) "hot pink"))
  (set! evil-replace-state-cursor '(box "chocolate"))
  (set! evil-motion-state-cursor '(box "plum3")))

(oo-add-hook 'evil-mode-hook #'oo-set-default-evil-cursors)
(oo-add-advice #'load-theme :after #'oo-set-default-evil-cursors)
