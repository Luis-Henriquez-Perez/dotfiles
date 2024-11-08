;;; init-mu4e.el --- Initialize mu4e -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Initialize mu4e.
;;
;;; Code:
;; Add mu4e to the load-path:
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(autoload #'mu4e "mu4e" nil t 'function)
(autoload #'+mu4e--main-enter-message "config-mu4e" nil t 'function)
(autoload #'+mu4e--main-leave-message "config-mu4e" nil t 'function)
(autoload #'+mu4e--legacy-enter-message "config-mu4e" nil t 'function)
(autoload #'+mu4e--legacy-leave-message "config-mu4e" nil t 'function)
(autoload #'oo--mail-signature "config-mu4e" nil t 'function)

(defun! oo--message-signature (&rest _)
  "Produce a signature for a message."
  (insert (s-join "\n" '("-- Yours Truly," "Luis M Henriquez-Perez\n"))))

(opt! message-signature '(funcall #'oo--message-signature))

(opt! mu4e-maildir (f-full "~/.mail"))
(opt! mu4e-headers-skip-duplicates t)
(opt! mu4e-view-show-images t)
(opt! mu4e-view-show-addresses t)
(opt! mu4e-compose-format-flowed nil)
(opt! mu4e-headers-date-format "%Y/%m/%d")
(opt! mu4e-change-filenames-when-moving t)
(opt! mu4e-attachments-dir (f-full "~/Downloads"))
(opt! mu4e-compose-signature '(funcall #'oo--message-signature))

(opt! mu4e-contexts
      (list (make-mu4e-context
             :name "luis@luishp.xyz"
             :enter-func #'+mu4e--main-enter-message
             :leave-func #'+mu4e--main-leave-message
             :vars `((user-mail-address      . "luis@luishp.xyz")
                     (user-full-name         . "Luis M Henriquez Perez")
                     (mu4e-refile-folder     . ,(f-full "/luishp/archive"))
                     (mu4e-drafts-folder     . ,(f-full "/luishp/drafts"))
                     (mu4e-sent-folder       . ,(f-full "/luishp/sent"))
                     (mu4e-trash-folder      . ,(f-full "/luishp/trash"))))
            (make-mu4e-context
             :name "gmail"
             :enter-func #'+mu4e--legacy-enter-message
             :leave-func #'+mu4e--legacy-leave-message
             :vars `((user-mail-address      . "luishenriquezperez@gmail.com")
                     (user-full-name         . "Luis M Henriquez Perez")
                     (mu4e-refile-folder     . "/luishenriquezperez/archive")
                     (mu4e-drafts-folder     . ,(f-full "/luishenriquezperez/drafts"))
                     (mu4e-sent-folder       . ,(f-full "/luishenriquezperez/sent"))
                     (mu4e-trash-folder      . ,(f-full "/luishenriquezperez/trash"))))))

(opt! message-send-mail-function   'smtpmail-send-it)
(opt! smtpmail-default-smtp-server "smtp.fastmail.com")
(opt! smtpmail-smtp-server         "smtp.fastmail.com")

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(opt! mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
(opt! mu4e-compose-context-policy nil)
;; This allows me to use 'helm' to select mailboxes
(opt! mu4e-completing-read-function 'completing-read)
;; Why would I want to leave my message open after I've sent it?
(opt! message-kill-buffer-on-exit t)
;; Don't ask to quit... why is this the default?
(opt! mu4e-confirm-quit nil)

;; [mu4e] Tip: `user-mail-address' ('luis@luishp.xyz') is not part of mu's addresses; add it with 'mu init
;; --my-address='
;;; provide
(provide 'init-mu4e)
;;; init-mu4e.el ends here
