;; Define leader map.
(require 'oo-bind-functions)

;; **** emacs leaders
;; :PROPERTIES:
;; :ID:       f3299c73-837e-46f9-a29e-9932c4570858
;; :END:
;; These leaders are for evil insert and emacs states as well as vanilla
;; Emacs.  Note that evil Emacs state is different from vanilla Emacs.  One of the
;; goals with these bindings is to set up keybindings in the case that I disable
;; evil mode or in the case that I want to use my bindings in insert or Emacs
;; state--or even vanilla Emacs.  The choice behind the bindings is the same as
;; [[id:][before]], except I just prepended the =Meta= (a.k.a. the =Alt= key) to everything.
(defconst oo-insert-leader-key "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst oo-insert-localleader-key "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-localleader-key "C-c l m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-emacs-localleader-short-key "C-c s")

;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

(oo-define-key :g oo-override-mode-map oo-emacs-leader-key #'oo-leader-prefix-command)
(oo-define-key :i oo-override-mode-map oo-emacs-leader-key #'oo-leader-prefix-command)
(oo-define-key :nmv oo-override-mode-map oo-emacs-leader-key #'oo-leader-prefix-command)

;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(oo-define-key oo-leader-map oo-normal-leader-key #'execute-extended-command)
(oo-define-key :nmv oo-leader-map oo-normal-leader-key #'execute-extended-command)
(oo-define-key :i "A-x" #'execute-extended-command)
(oo-define-key :i "M-x" #'execute-extended-command)

(provide 'oo-leader-map)
