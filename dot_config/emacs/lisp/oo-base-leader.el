(require 'oo-base-bind-key)
(require 'oo-override-mode)

;; This file provides leaders keys for evil and non-evil states and it binds
;; these leader keys.

;; These leaders are specifically for evil mode states (not including insert and
;;                                                          Emacs).  I choose the space (=SPC=) key for evil leaders because it is one of if
;; not the easiest key to press because of its central placement on the keyboard
;; and its sheer size--at least on the [[https://en.wikipedia.org/wiki/QWERTY][qwerty]] keyboard that I use.  The choice
;; of =SPC m= for the major mode specific keys is simply for the pnemonic =m= which
;; stands for "major mode".  The short major mode prefix key =,= is for cases when I
;; want to shorten a key binding.  Although obviously not as easy to remember as
;; =m=, it provides me with one shorter keypress in certain situations.
(defconst oo-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst oo-normal-localleader-key "SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-normal-localleader-short-key ","
  "A shorter alternative `oo-localleader-key'.")
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

(oo-bind :g   'oo-override-mode-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(oo-bind :i   'oo-override-mode-map oo-insert-leader-key #'oo-leader-prefix-command)
(oo-bind :nmv 'oo-override-mode-map oo-normal-leader-key #'oo-leader-prefix-command)

;; One of the most common--if not the most common--command you use in Emacs is
;; [[helpfn:execute-extended-command][execute-extended-command]].  This command let's you search any other command and
;; upon pressing enter, then you execute the command.  The fact that this command is
;; invoked so frequently suggests it should have one of the shortest, easiest to
;; press bindings.  I chose to give it =SPC SPC= and =;=.  =SPC SPC= is short and
;; quick to type as well as consistent with other =SPC= bindings.  While =;= is
;; super fast to press as well and even faster than =SPC SPC=.
(oo-bind :nmv 'oo-override-mode-map ";" #'execute-extended-command)
(oo-bind oo-leader-map oo-normal-leader-key #'execute-extended-command :wk "execute command")
(oo-bind :i "A-x" #'execute-extended-command)
(oo-bind :i "M-x" #'execute-extended-command)

(defun! oo--bind-localleader (fns metadata)
  "Automate binding to leader."
  (let! alist '((oo-normal-localleader-short-key . normal)
                (oo-normal-localleader-key . normal)
                (oo-insert-localleader-key . insert)
                (oo-insert-localleader-short-key . insert)
                (oo-emacs-localleader-key . emacs)
                (oo-emacs-localleader-short-key . emacs)
                (oo-emacs-localleader-key . global)
                (oo-emacs-localleader-short-key . global)))
  (let! key (map-elt metadata :key))
  (let! localleader (map-elt metadata :localleader))
  (if localleader
      (for! ((leader . state) alist)
        (let! new-key (if (vectorp key)
                          key
                        (concat (symbol-value leader) "\s" key)))
        (alet (-> metadata
                  (map-insert :key new-key)
                  (map-insert :state state))
          (oo--resolve-binding fns it)))
    (oo--resolve-binding fns metadata)))

(adjoining! oo-binding-fns #'oo--bind-localleader)

(provide 'oo-base-leader)
