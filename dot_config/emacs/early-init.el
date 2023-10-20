(push '(left-fringe  . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(advice-add #'x-apply-session-resources :override #'ignore)

(setq-default mode-line-format nil)

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
