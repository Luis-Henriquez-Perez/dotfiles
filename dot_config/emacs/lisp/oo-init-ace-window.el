(set! aw-swap-invert t)

(set! aw-keys (list ?f ?j ?d ?k ?s ?l ?a ?\; ?g ?h ?t ?y ?r ?u))

;; **** select a window with =w=, =j= or =o=
;; There are commands such as.  I do not need these commands.  After moving left,
;; right, up or down some direction once, the effort needed to traverse a window
;; using directional window movement commands greatly increases.  The command
;; [[file:snapshots/_helpful_command__ace-window][ace-window]] in contrast scales really well with a greater number of
;; windows.  And it only loses slightly to directional window commands when moving
;; one time.

;; The command [[file:snapshots/_helpful_command__ace-window_.png][ace-window]] leverages [[https://github.com/abo-abo/avy][avy]] to select a window.  It assigns each window
;; a character (I'm using [[][letters]] close to the homerow) which it displays on
;; the upper right-hand corner of windows. I've found that
;; ace-window is the quickest way possible to switch focus form one live window to
;; another.

;; The mnemonic bind is =w= and the quick bindings--which I will likely use most
;; often--are =o= and =j=.
(oo-bind 'oo-window-map "w" #'ace-window :wk "select")
(oo-bind 'oo-window-map "j" #'ace-window :wk "select")
(oo-bind 'oo-window-map "o" #'ace-window :wk "select")

;; ******* swap two windows with =s=
;; :PROPERTIES:
;; :ID:       20231012T125502.066095
;; :END:
;; Often when I want to switch focus from my main window to one of its
;; subsidiaries; I will want to swap buffers from the two windows.
;; Actually, =edwina= does provide functions to do this: namely
;; [[_helpful_command__edwina-swap-next-window_.png][edwina-swap-next-window]] and [[file:snapshots/_helpful_command__edwina-swap-previous-window_.png][edwina-swap-previous-window]].
;; But I can do something similar, but much faster with.  This is a case where =s= is
;; mnemonic and easy to press.
(oo-bind 'oo-window-map "s" #'ace-swap-window :wk "swap")
