;; Script for opening the archlinux feed.
(require 'elfeed)

;; Ensure the archl feed is in `elfeed-feeds'.
(cl-pushnew "https://archlinux.org/feeds/news/" elfeed-feeds :test #'equal)

;; Just update the archlinux feed so it is snappy.
(elfeed-update-feed "https://archlinux.org/feeds/news/")

(elfeed)

;; (elfeed-search-set-filter "=https://archlinux.org/feeds/news/")
