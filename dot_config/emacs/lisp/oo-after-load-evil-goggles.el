(require 'evil-goggles)

;; 1. advise all commands used by evil-goggles
(defvar evil-goggles--commands
  '((evil-delete                :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--generic-blocking-advice)
    (evil-delete-line           :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
    (evil-org-delete            :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
    (evil-yank                  :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
    (evil-yank-line             :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
    (evil-change                :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-change-line           :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-change-whole-line     :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-indent                :face evil-goggles-indent-face                :switch evil-goggles-enable-indent                :advice evil-goggles--generic-async-advice)
    (evil-join                  :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
    (evil-join-whitespace       :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
    (evil-fill-and-move         :face evil-goggles-fill-and-move-face         :switch evil-goggles-enable-fill-and-move         :advice evil-goggles--generic-async-advice)
    (evil-shift-left            :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-shift-right           :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-org-<                 :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-org->                 :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
    (evil-surround-region       :face evil-goggles-surround-face              :switch evil-goggles-enable-surround              :advice evil-goggles--generic-async-advice)
    (evil-commentary            :face evil-goggles-commentary-face            :switch evil-goggles-enable-commentary            :advice evil-goggles--generic-async-advice)
    (evilnc-comment-operator    :face evil-goggles-nerd-commenter-face        :switch evil-goggles-enable-nerd-commenter        :advice evil-goggles--generic-async-advice)
    (evil-replace-with-register :face evil-goggles-replace-with-register-face :switch evil-goggles-enable-replace-with-register :advice evil-goggles--generic-async-advice-1)
    (evil-set-marker            :face evil-goggles-set-marker-face            :switch evil-goggles-enable-set-marker            :advice evil-goggles--set-marker-advice)
    (evil-record-macro          :face evil-goggles-record-macro-face          :switch evil-goggles-enable-record-macro          :advice evil-goggles--record-macro-advice)
    (evil-paste-before          :face evil-goggles-paste-face                 :switch evil-goggles-enable-paste                 :advice evil-goggles--paste-advice :after t)
    (evil-paste-after           :face evil-goggles-paste-face                 :switch evil-goggles-enable-paste                 :advice evil-goggles--paste-advice :after t)))

;; This is kind of tricky to lazy-load.  What I have to do is enable
;; `evil-goggle' the first time one of these functions is called.  So I should
;; create an advice and add it to all of these functions.
(defun! oo--load-evil-goggles (fn &rest args)
  ""
  ;; First load `evil-goggles'.
  (require 'evil-goggles)
  ;; This should trigger after blocks.
  (evil-goggles-mode 1)
  ;; Then register any command that needs registering.
  ;; (set! commands '((evil-yank                 . lispyville-yank)
  ;;                  (evil-delete               . lispyville-delete)
  ;;                  (evil-change               . lispyville-change)
  ;;                  (evil-yank-line            . lispyville-yank-line)
  ;;                  (evil-delete-line          . lispyville-delete-line)
  ;;                  (evil-change-line          . lispyville-change-line)
  ;;                  (evil-delete-char          . lispyville-delete-char-or-splice)
  ;;                  (evil-delete-backward-char . lispyville-delete-char-or-splice-backwards)
  ;;                  (evil-substitute           . lispyville-substitute)
  ;;                  (evil-change-whole-line    . lispyville-change-whole-line)
  ;;                  (evil-join                 . lispyville-join)))
  ;; (for! ((old . new) commands)
  ;;   (awhen (assoc old evil-goggles--commands)
  ;;     (pushing! evil-goggles--commands (cons new (cdr it)))))
  )

;; (progn!
;;   (set! commands '((evil-yank                 . lispyville-yank)
;;                    (evil-delete               . lispyville-delete)
;;                    (evil-change               . lispyville-change)
;;                    (evil-yank-line            . lispyville-yank-line)
;;                    (evil-delete-line          . lispyville-delete-line)
;;                    (evil-change-line          . lispyville-change-line)
;;                    (evil-delete-char          . lispyville-delete-char-or-splice)
;;                    (evil-delete-backward-char . lispyville-delete-char-or-splice-backwards)
;;                    (evil-substitute           . lispyville-substitute)
;;                    (evil-change-whole-line    . lispyville-change-whole-line)
;;                    (evil-join                 . lispyville-join)))
;;   (advice-add new :around #'oo--lazy-load-evil-goggles)
;;   ;; (for! ((old . new) commands)
;;   ;;   (awhen (assoc old evil-goggles--commands)
;;   ;;     (pushing! evil-goggles--commands (cons new (cdr it)))))
;;   )
;;;;; evil-goggles
;; There is probably a better way to lazy-load this.
;; TODO: Figure out a way to lazy-load this.
;; (oo-add-hook 'evil-mode-hook #'evil-goggles-mode)

;; Let me write a quick macro to set these commands.
;; (progn!
;;   (setup! :goggle evil-operator-eval-replace evil-delete)
;;   (setup! :goggle evil-operator-eval evil-delete)
;;   (setup! :goggle lispyville-yank evil-change)
;;   (setup! :goggle lispyville-yank-line evil-change)
;;   (setup! :goggle lispyville-change evil-change)
;;   (setup! :goggle lispyville-delete evil-delete))
