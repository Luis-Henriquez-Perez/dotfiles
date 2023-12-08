;; *** variant of =ace-swap-windows= that keeps cursor on same window
;; :PROPERTIES:
;; :ID:       20231011T071635.290759
;; :END:
;; Assume you're currently on window A.  The function =ace-swap-windows= swaps the
;; buffers from windows A and window B and sets the focus to window B.  After the
;; buffer swap I want the focus to still be on window A.  In other words, I want
;; buffers to be swapped but the cursor should be on the original window.
;; Fortunately, the variable =aw-swap-invert= does just that.
(set! aw-swap-invert t)
;; *** use letters instead of numbers to select windows
;; :PROPERTIES:
;; :ID:       20231011T071800.840648
;; :END:
;; Letters are faster for me to press on.  And since I use a [[https://en.wikipedia.org/wiki/QWERTY][QWERTY]] keyboard, the
;; [[file:pictures/homerow-keys.jpg][homerow keys]] are the fastest of all other letters to type.  And even among the
;; homerow keys, the letters closes to the center are the fastest. Therefore, I set the
;; letters in order by the keys closest to the center of the QWERTY keyboard.
(set! aw-keys (list ?f ?j ?d ?k ?s ?l ?a ?\; ?g ?h ?t ?y ?r ?u))
