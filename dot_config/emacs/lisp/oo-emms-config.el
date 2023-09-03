(set! emms-source-file-default-directory (expand-file-name "Music/" "~/"))
(set! emms-directory (expand-file-name "emms/" oo-cache-dir))

(oo-call-after-load 'emms #'require 'emms-player-mpv)
(set! emms-player-list '(emms-player-mpv))

(provide 'oo-emms-config)
