(require 'oo-call-after-keymap)
(require 'oo-call-after-load)

;; ***** binding functions
;; These functions are the building blocks of binding.  Keybinding in Emacs is
;; extensive and there are so many packages that it necessitates an extensible and
;; malleable design for a binding function.  This is the part of the design where I
;; try to ensure extensibility.
;; ****** binding functions
;; The functions here are functions to generate the body of the =bind!= macro.  I
;; divided form generation into two parts: the functions generate the deferment for
;; bindings and decide which binding function and arguments to use and the ones
;; that generate the actual binding forms.
;; ******* oo-binding-fns
;; This variable will contain the functions that will be called in turn to produce
;; the side-effect that results in the actual binding.  Order matters here.
;; #+begin_src elisp
(defvar oo-binding-fns '(oo--bind-localleader
                         oo--bind-alt
                         oo--bind-ensure-keybinding
                         oo--bind-exwm-key
                         oo--bind-evil-state-keyword
                         oo--bind-evil-define-key
                         oo--bind-define-key)
  "List of functions that generate the body of `bind!'.")

;; This is the function that will be invoked by the functions in
;; [[id:20231026T133140.783912][oo-binding-functions]] to proceed to the next step.

(defun oo--resolve-binding (fns metadata)
  "Run the next function in `oo-bind-functions'.
If there are no more functions, do nothing."
  (funcall (or (car fns) #'ignore) (cdr fns) metadata))

(defun oo--bind-alt (fns metadata)
  (cond ((map-elt metadata :alt)
         (unless (map-elt metadata :keymap)
           (setf (map-elt metadata :keymap) global-map))
         (awhen (map-elt metadata :feature)
           (setf (map-elt metadata :condition)
                 `(lambda (&rest _) (or (featurep ',it) (require ',it nil t)))))
         (oo--do-binding metadata #'oo-alt-bind :keymap :key :def :condition))
        (t
         (oo--resolve-binding fns metadata))))

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
;; ******* oo--bind-ensure-keybinding
;; I think [[][general]] does the same kind of thing where you can specify a symbol
;; as the keymap in case you want to load the keybinding only when the keymap
;; symbol is defined.
(defun! oo--bind-ensure-keybinding (fns metadata)
  "Ensure metadata has essential components of a keybinding."
  (unless (and (map-elt metadata :key)
               (--any-p (map-contains-key metadata it) '(:keymap :localleader))
               (map-contains-key metadata :def))
    (error "Does not contain necessary components of keybinding."))
  (let! keymap (map-elt metadata :keymap))
  (cond ((symbolp keymap)
         (flet! insert-keymap-value (keymap fns metadata)
           (--> (map-insert metadata :keymap-symbol keymap)
             (map-insert it :keymap (symbol-value keymap))
             (oo--resolve-binding fns it)))
         (oo-call-after-keymap keymap #'insert-keymap-value keymap fns metadata))
        (t
         (oo--resolve-binding fns metadata))))
;; ******* oo--bind-exwm-key
;; #+begin_src elisp
;; (defun! oo--bind-exwm-key (fns metadata)
;;   "If map is `exwm-input-keys' use `exwm-input-set-key' instead of `define-key'."
;;   (let! keymap (map-elt metadata :keymap))
;;   (if (equal keymap 'exwm-input-keys)
;;       (-p-> (oo--do-binding metadata #'exwm-input-set-key :key :def)
;;             (oo-call-after-load 'exwm))
;;     (oo--resolve-binding fns metadata)))
;; #+end_src
;; ******* oo--bind-state-key
;; This function is to deal with the user passing in an abbreviation for a set of
;; states or as I call it an =evil-state-keyword= . The abbreviation.
;; #+begin_src elisp
;; ;; I need a separate function for this because I need to determine the state on
;; ;; the fly once I know it exists.  That is I can't just specify it in
;; ;; `oo--bind-state-key' as I would like to because I don't know what it is yet.
;; (defun! oo--bind-with-state-key (key metadata)
;;   "Replace state-key with state."
;;   (let! state (oo-evil-state key))
;;   (setf (map-elt metadata :state) state)
;;   (oo--bind-evil-define-key nil metadata))

;; (defun oo--bind-evil-state-keyword (fns metadata)
;;   (let! state-key (map-elt metadata :state-key))
;;   (if (not state-key)
;;       (oo--resolve-binding fns metadata)
;;     (let! keys
;;       (--> (symbol-name state-key)
;;         (seq-rest it)
;;         (string-split it "" t)
;;         (mapcar #'oo-args-to-keyword it)))
;;     (dolist (key keys)
;;       (if (equal key :g)
;;           (oo--resolve-binding fns metadata)
;;         (-p-> (oo--bind-with-state-key key metadata)
;;               (oo-call-after-evil-state key))))))
;; #+end_src
;; ******* oo--bind-evil-define-key
;; Part of the reason that =evil= was deferred separately in [[][]] is immediately
;; apparent: this function can be written with the assumption that =evil= is already
;; loaded.

;; The reason I use =oo-call-after-keymap= here is to defer.  This is the frugal way
;; of.  I am considering [[][creating a hook]] specifically which is more involved.
;; I worry about the bindings not being active when I define a state myself and no
;; file is being loaded by =after-load-functions=.  But maybe in practice this will
;; always work?  I'm not sure so I will test it out first.
;; #+begin_src elisp
;; (defun! oo--bind-evil-define-key (fns metadata)
;;   "Define evil key based on metadata."
;;   (let! states (-list (map-elt metadata :state)))
;;   (if (not states)
;;       (oo--resolve-binding fns metadata)
;;     (dolist (state states)
;;       (cond ((equal state 'global)
;;              (oo--resolve-binding fns metadata))
;;             ((map-elt metadata :mode)
;;              (-p-> (oo--do-binding metadata #'evil-define-minor-mode-key :state :mode :key :def)
;;                    (oo-call-after-evil-state state)))
;;             (t
;;              (-p-> (oo--do-binding metadata #'evil-define-key* :state :keymap :key :def)
;;                    (oo-call-after-evil-state state)))))))
;; #+end_src
;; ******* oo--bind-define-key
;; This is the default behavior for binding a key and what I generally will want to
;; use if I do not specify a state.
;; #+begin_src elisp
;; (defun oo--bind-define-key (_ metadata)
;;   (oo--do-binding metadata #'define-key :keymap :key :def))
;; #+end_src
;; ******* oo--do-binding
;; This function is the indicator for when the function that actually does the
;; keybinding is actually called as opposed to forms that may defer the keybinding
;; until a later time.  With this I can access the precise time when a key is going
;; to be bound by advising or [[https://github.com/nicferrier/emacs-noflet][nofletting]] this function. From this function I can do
;; useful things such as override =define-key= to do something else (in fact this I
;; actually do this for using =which-key=) or even override =oo--do-binding= itself so
;; that it only either [[][logs]] or outputs to the messages buffer.  The latter is
;; useful if I want to see what keys would be bound by an invocation to =oo-bind=
;; instead of actually binding them.
;; #+begin_src elisp
;; (defun! oo--do-binding (metadata fn &rest keys)
;;   "Call FN with values of KEYS in METADATA."
;;   (flet! kbd-maybe (x) (if (vectorp x) x (kbd x)))
;;   (let! desc (map-elt metadata :wk))
;;   (let! args (mapcar (-partial #'map-elt metadata) keys))
;;   ;; (message "%S" (cons fn (-replace-where #'keymapp (-const 'keymap) args)))
;;   (stub! define-key (keymap key def)
;;     (when desc
;;       (-p-> (which-key-add-keymap-based-replacements keymap key desc)
;;             (oo-call-after-load 'which-key)))
;;     (funcall this-fn keymap (kbd-maybe key) def))
;;   (apply fn args))
;; #+end_src
;; ****** cannot compose kbd function for which-key
;; I need to combine the =noflet= form for =which-key= and using kbd because.
;; ****** oo--bind
;; This is the internal function that I will use to bind keys.
;; #+begin_src elisp
;; (defun oo--bind (&rest metadata)
;;   (oo--resolve-binding oo-binding-fns metadata))
;; This is the "front-end" that I'll be using to bind keys.  Unlike the internal
;; function [[id:20231026T075710.285898][oo--bind]] whose goal is to be programmatically consistent (e.g. in
;; other functions and macros), the goal of =oo-bind= is to be easy and concise for
;; me to use for configuration.
(defun! oo-bind (&rest args)
  "Bind KEY in KEYMAP to DEFINITION.
This is the same as `define-key'.  If KEYMAP is not specified use `global-map'.
If STATES is specified use evil binding.  If KEYMAP is a symbol."
  (flet! plist-p (x) (or (null x) (keywordp (car x))))
  ;; States is a list of symbols
  (flet! states-p (x) (or (symbolp x) (and (listp x) (-all-p #'symbolp x))))
  (flet! map-symbol-p (x) (and (symbolp x)
                               (not (keywordp x))
                               (string-match-p "-map\\'" (symbol-name x))))
  (flet! keymap-p (-orfn #'keymapp #'map-symbol-p))
  (flet! key-p (-orfn #'stringp #'vectorp))
  (pcase args
    ;; Just passing in key and def, implies `global-map'.
    (`(:alt ,key ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :alt t :key key :def def metadata))
    (`(,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :keymap global-map :key key :def def metadata))
    (`(,(and (pred keymap-p) keymap) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :keymap keymap :key key :def def metadata))
    (`(,(and (pred keymap-p) keymap) ,(and (pred keywordp) state-key) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :state-key state-key :keymap keymap :key key :def def metadata))
    (`(,(and (pred keywordp) state-key) ,(and (pred keymap-p) keymap) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :state-key state-key :keymap keymap :key key :def def metadata))
    (`(,(and (pred keywordp) state-key) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :state-key state-key :keymap 'global-map :key key :def def metadata))
    (`(,(and (pred states-p) states) ,(and (pred keymap-p) keymap) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :states states :keymap keymap :key key :def def metadata))
    (`(,(and (pred states-p) states) ,(and (pred key-p) key) ,def . ,(and (pred plist-p) metadata))
     (apply #'oo--bind :states states :keymap 'global-map :key key :def def metadata))
    (_
     (error "unknown binding syntax"))))
(provide 'oo-bind)

