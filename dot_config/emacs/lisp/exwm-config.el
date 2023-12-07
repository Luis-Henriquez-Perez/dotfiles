(require 'oo-base-library)
(require 'oo-ext-bind-exwm-key)

;; [[https://systemreboot.net/dot-emacs#org89a07df][bind-EXWM keys]]
(defun)

(defun oo-make-external-command (command)
  "Return a function that..."
  (lambda ()
    (interactive)
    (let ((buffer-name (car (split-string command))))
      (cond
       ((equal buffer-name (buffer-name))
        (switch-to-last-used-buffer))
       ((get-buffer buffer-name)
        (switch-to-buffer (get-buffer buffer-name)))
       (t
        (start-process-shell-command buffer-name nil command))))))

;; Add an extension to `oo-define-key' that allows me to bind EXWM keys as if
;; they were normal keybindings.

;; One question is when should, how should these variables load the files.  This
;; customization should be included for `oo-define-key' before any binding is
;; done with `exwm-input-keys', otherwise `oo-define-key' will mistake the
;; binding as a normal one.  There needs to be the concept of "init", code that
;; is evaluated immediately and "configuration" code that's evaluated only after
;; the target package is loaded.
(defun oo--bind-exwm-keys (fns metadata)
  "If map is `exwm-input-keys'"
  (if-let ((map (alet (plist-get metadata :map)
                  (and (equal 'exwm-input-keys it) it)))
           (key (plist-get metadata :key))
           (def (plist-get metadata :def)))
      (oo-call-after-load 'exwm #'exwm-input-set-key key def)
    (oo--bind-do-binding fns metadata)))
(adjoining! oo-bind-functions #'oo--bind-exwm-keys)

(provide 'exwm-config)
