;; org-appear
;; This package allows for the editing of org emphasis markers and links in a
;; "do-what-I-mean" type way.  Typically when I edit an org link or markup I have
;; to manually toggle [[][]] and then edit it. That's because I tell org mode to
;; display links and emphasis markers in a "prettified" way.  Although it's more
;; pleasant to the eye to see emphasis markers this way; it is harder to edit
;; them.  So what will end up happening is I toggle.  What this package does is
;; make it so if your cursor is on the emphasis marker or link, it's visibility
;; automatically toggled.
(elpaca (org-appear :fetcher github :repo "awth13/org-appear" :ref "eb9f9db" :branch "master"))
