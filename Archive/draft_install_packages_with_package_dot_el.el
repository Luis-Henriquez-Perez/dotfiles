;;; Setting up package.el
;; This describes setting up package.el for package management.  Package.el got
;; support for installing packages as git repos similar to straight.el.  And
;; despite the existence of elpaca I will admit that I have had a an inclination
;; to use package.el because it is built in, because it is synchronous, and
;; because it is officially supported by the emacs maintainers.  I do not know.
;; To me I feel that even if it is not perfect--and certainly it is quite
;; quirky, I have a lot of room to hack on it.
;;;; ideas
;;;;; Sorting the Packages by Dependency
;;;;; revision wont work sometimes 
;; I kept getting empty checkout errors for packages.  I suspect there is some
;; bug/oversight having to do with the command for cloning a specific revision
;; from a non-github site.  The revision cloning from a non-github site worked
;; fine.

;; This makes me curious whether =package-vc-checkout= would and then
;; =package-vc-install-from-checkout= would work.  I have my questions about.
;;;; helpers
;;;;; Advice to get arguments
;; This is an advice to.  This will not always work perfectly because sometimes
;; the FN is not a symbol.
(defun oo-message-fn-and-args (fn &rest args)
  (message "%S" (cons 'vc-clone args))
  (apply fn args))
;; I need to know whether vc-clone is the problem or if the problem is the
;; arguments passed into vc-clone are incorrect.
(advice-add #'vc-clone :around #'oo-message-fn-and-args)
;;;;; Suppressing unecessary messages
;; There are tons of compilation messages which is one of the complaints with
;; the package.el.  The idea is if we do not. 

;; Funny thing is I actually do not know which packages were.
;; The idea is to use this macro to determine whether.
;; (defmacro with-suppressed-messages! (&rest body)
;;   "Execute BODY, suppressing messages and capturing them in a string."
;;   (let ((original-message-fn (symbol-function 'message)))
;;     `(let ((capture-buffer (generate-new-buffer " *captured-messages*")))
;;        (with-current-buffer capture-buffer
;;          (unwind-protect
;;              (progn
;;                (fset 'message (lambda (&rest args)
;;                                 (with-current-buffer ,captured-messages
;;                                   (goto-char (point-max))
;;                                   (insert (apply 'format args) "\n"))))
;;                ,@body)
;;            (fset 'message ,original-message-fn)
;;            (kill-buffer capture-buffer))))))
;;;;; message only fail pass
;; (defun oo-message-only-fail-pass (fn args)
;;   (with-suppressed-messages!
;;    (apply fn args)))
;; (advice-add #'package-vc-install :around #'oo-message-only-fail-pass)
;;;;; wrapper around =package-vc-install= 
;; (defun oo-package-install ()
;;   )
;;;; basic setup
;; Some of the packages are duplicated as well.  To me this implies that it
;; installed dependencies of a package without consulting the recipes I
;; specified in =package-vc-selected-packages=.  This is what I was worried
;; about.  I suspect that a package's dependencies are determined .
(require 'package)
(require 'package-vc)
(require 'vc)

;; I want a clearer name.  When I saw the name "elpa" I had no idea what was in that folder.
(setq package-user-dir (locate-user-emacs-file "packages/"))

;; Add MELPA repository if it's not already included
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize the package system
(package-initialize)

;; Refresh package contents if the package list is empty
(unless package-archive-contents
  (package-refresh-contents))

;; One thing I do not find clear in the manual is how to avoid the problem of
;; registering packages.  Straight solves it by giving the user the option to
;; register packages.  Whereas, elpaca solves it by queuing.
;;;; Deal With Dependencies
;; So when I first installed everything I noticed duplicate packages in the
;; dired directory.  So how to deal with this?  The first thing that comes to mind I have to figure out how
;; package-vc is installing dependencies, then intercept it when its choosing
;; the dependencies to install and tell it to prefer installing a dependency as
;; is already specified by =package-vc-selected-packages=.

;; The first step is just to get more information because I do not have a clear
;; idea of what is going on.

;; 1. Is to get the printed output of the messages produced.  That way I can see
;;    which packages were actually installed, what information I want to.
;; 2. I should look at the actual directory and determine programmatically which
;;    packages have duplicates and figure out why, what is the commonality
;;    between these packages with duplicates.

;; I also want to know how package-vc determines which package is a dependency
;; and which is not.
;;;; Populate =package-vc-selected-packages=
;;;;; elpaca
;;;;; outli 
(push '(outli :url "https://github.com/jdtsmith/outli" :branch "main" :commit "956755f") package-vc-selected-packages)
;;;;; edwina
;; There are [[several]] emacs packages that save and restore window configuration. But
;; before being able to save and restore window configurations you need to create a
;; window configuration and that process can be repetitive and tedious. This
;; package actually automates the building of a window configuration from one
;; window.  This package provides tools to configure =display-buffer= so that when
;; called I can open a buffer in a [[file:screenshots/edwina-usage.png][master-slave layout]] (see [[file:screenshots/edwina-usage.png][edwina's usage]]).
(push '(edwina :fetcher gitlab :url "https://github.com/ajgrf/edwina" :branch "master" :commit "f95c31b") package-vc-selected-packages)
;;;;; loop
(push '(loop :url "https://github.com/Wilfred/loop.el") package-vc-selected-packages)
;;;;; htmlize
;; This package converts emacs buffers to html.  I use it in combination with the
;; command-line utility [[][wkhtmltoimage]] to take [[id:20230802T153110.062543][snapshots]] of emacs buffers.
;; There is a similar built-in emacs package [[][html-fontify]] that I have tried
;; to use, but I encountered [[][an error]].
(push '(htmlize :url "https://github.com/hniksic/emacs-htmlize" :commit "dd27bc3" :branch "master") package-vc-selected-packages)
;;;;; org-appear
;; This package allows for the editing of org emphasis markers and links in a
;; "do-what-I-mean" type way.  Typically when I edit an org link or markup I have
;; to manually toggle [[][]] and then edit it. That's because I tell org mode to
;; display links and emphasis markers in a "prettified" way.  Although it's more
;; pleasant to the eye to see emphasis markers this way; it is harder to edit
;; them.  So what will end up happening is I toggle.  What this package does is
;; make it so if your cursor is on the emphasis marker or link, it's visibility
;; automatically toggled.
(push '(org-appear :url "https://github.com/awth13/org-appear" :commit "eb9f9db" :branch "master") package-vc-selected-packages)
;;;;; outshine
;; Outshine is an Emacs package that allows for the use of org mode in non org

;; features.  It promises a kind of "have your cake and eat it too" type thing
;; where you can utilize the features of org mode--which include markup support,
;; link support, headline refiling, etc--while not paying the price of needing
;; an initial tangling script.
(push '(outshine :url "https://github.com/alphapapa/outshine" :commit "bf1eed1") package-vc-selected-packages)
;;;;; outorg
;; Outshine depends on the package Outorg.  Outorg is what outshine uses to
;; convert back and forth between a file with outshine syntax and an org file.
(push '(outorg :url "https://github.com/alphapapa/outorg" :commit "ef0f86f") package-vc-selected-packages)
;;;;; navi-mode
;; This package is a dependency of `outorg'.  It has to do with providing
;; navigation commands for `outshine'.
(push '(navi-mode :url "https://github.com/alphapapa/navi" :commit "cf97e1e") package-vc-selected-packages)
;;;;; orglink
;; This package lets me display org links in non org buffer.  In practice I use
;; this for buffers with outshine enabled.
(push '(orglink :url "https://github.com/tarsius/orglink" :commit "afbeffd") package-vc-selected-packages)
;;;;; chezmoi
;; This package provides some functions to interface with =chezmoi=, a dotfile
;; manager.  At first--and even still to be honest I wanted to use Org as a
;; dotfile manager; but org although having some useful dotfile management tools
;; (such as tangling) is not designed to be a dotfile manager.  Whereas,
;; =chezmoi= was actually designed to solve many dotfile problems.  In the
;; future, I'd like to scrap =chezmoi= and use a dotfile manager written in
;; Elisp; preferably with the optional of optionally using org mode tools for
;; dotfile management.
(push '(chezmoi :url "https://github.com/tuh8888/chezmoi.el" :commit "1389782") package-vc-selected-packages)
;;;;; lua-mode
;; Emacs doesn't have a mode for lua built-in--not to say that's a bad
;; thing.  This package provides such a mode.
(push '(lua-mode :url "https://github.com/immerrr/lua-mode" :commit "7eb8eaa") package-vc-selected-packages)
;;;;; org-bookmark-heading
;; This package provides bookmark functions catered specifically for org files.

;; =point= as well as surrounding text that can be searched for in text files.
;; Obviously, this is not perfect but because a bookmark may easily become
;; Broken when you edit the region.  With org you can do better because each
;; Headline has a unique ID.  This package provides the function for storing the
;; information necessary and it provides the function to jump to that ID.
(push '(org-bookmark-heading :url "https://github.com/alphapapa/org-bookmark-heading" :commit "4e97fab") package-vc-selected-packages)
;;;;; cape
;; This package provides several =capfs= and utility functions for working with
;; =capfs=.  By the way =capfs= stands for a [[][completion at point function]].
;; I am particular interested in a function it provides to merge =capfs=.
(push '(cape :url "https://github.com/minad/cape" :branch "main" :commit "4645762") package-vc-selected-packages)
;;;;; filladapt
(push '(filladapt :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/?h=externals/filladapt" :branch "externals/filladapt" :commit "802c194") package-vc-selected-packages)
;;;;; burly
;; Burly provides functions that can save a window configuration via bookmarks.
;; There are a plethora of packages out there provide ways to manage window
;; configurations; to name a few: [[about:blank][eyebrowse]], [[https://github.com/knu/elscreen][persp-mode]], [[https://github.com/knu/elscreen][elscreen]], [[https://elpa.gnu.org/packages/wconf.html][wconf]].  What
;; makes Burly different is that instead of making its own abstraction for
;; window configuration saving, it uses bookmarks which already have existing
;; support in Emacs.
(push '(burly :url "https://github.com/alphapapa/burly.el" :branch "master" :commit "f570fa8") package-vc-selected-packages)
;;;;; dogears
(push '(dogears :url "https://github.com/alphapapa/dogears.el" :files (:defaults (:exclude "helm-dogears.el"))) package-vc-selected-packages)
;;;;; captain
;; This package provides me with auto-capitalization.  It checks each word as I
;; type and after I finish typing that word it checks whether or not it should
;; be capitalized.
;; (push '(captain :url "git://git.sv.gnu.org/emacs/elpa" :local-repo "https://github.com/captain" :branch "externals/captain" :commit "364ee98") package-vc-selected-packages)
;;;;; lambda-themes
;; These are some beautiful and elegant themes.  They are on the minimal side, but I
;; like that very much.
(push '(lambda-themes :url "https://github.com/Lambda-Emacs/lambda-themes" :branch "main" :commit "7342250") package-vc-selected-packages)
;;;;; consult
(push '(consult :url "https://github.com/minad/consult" :branch "main" :commit "fae9b50") package-vc-selected-packages)
;;;;; textsize
(push '(textsize :url "https://github.com/WJCFerguson/textsize" :branch "master" :commit "df91392") package-vc-selected-packages)
;;;;; on.el
;; This package provides hooks inspired by [[][Doom Emacs]].  Generally, the main
;; idea behind the hooks these package offers is to provide ways to improve
;; performance usually by spreading out the loading of features.  The hooks it
;; provides are [[][]].
(push '(on :host github :url "https://github.com/ajgrf/on.el" :branch "master" :commit "3cf623e") package-vc-selected-packages)
;;;;; ligature
(push '(ligature :url "https://github.com/mickeynp/ligature.el" :commit "3d14604") package-vc-selected-packages)
;;;;; org
(push '(org :url "https://git.savannah.gnu.org/git/emacs/org-mode.git" :commit "f731d45") package-vc-selected-packages)
;;;;; refine
(push '(refine :url "https://github.com/Wilfred/refine" :commit "d72fa50") package-vc-selected-packages)
;;;;; ace-window
(push '(ace-window :url "https://github.com/abo-abo/ace-window" :commit "c7cb315") package-vc-selected-packages)
;;;;; aggressive-fill-paragraph
(push '(aggressive-fill-paragraph :url "https://github.com/davidshepherd7/aggressive-fill-paragraph-mode" :commit "4a620e6") package-vc-selected-packages)
;;;;; aggressive-indent
(push '(aggressive-indent :url "https://github.com/Malabarba/aggressive-indent-mode" :commit "b0ec004") package-vc-selected-packages)
;;;;; all-the-icons
(push '(all-the-icons :url "https://github.com/domtronn/all-the-icons.el" :commit "be99987") package-vc-selected-packages)
;;;;; all-the-icons-completion
(push '(all-the-icons-completion :url "https://github.com/iyefrat/all-the-icons-completion" :commit "286e2c0") package-vc-selected-packages)
;;;;; anaphora
;; This package provides me with common [[https://en.wikipedia.org/wiki/Anaphoric_macro][anaphoric macros]].
(push '(anaphora :url "https://github.com/rolandwalker/anaphora" :commit "3b2da3f") package-vc-selected-packages)
;;;;; async
(push '(async :url "https://github.com/jwiegley/emacs-async" :commit "9a8cd0c") package-vc-selected-packages)
;;;;; auth-source-pass
(push '(auth-source-pass :url "https://github.com/DamienCassou/auth-source-pass" :commit "aa7f171") package-vc-selected-packages)
;;;;; auto-capitalize
(push '(auto-capitalize :url "https://github.com/emacsmirror/auto-capitalize" :commit "0ee14c7") package-vc-selected-packages)
;;;;; avy
(push '(avy :url "https://github.com/abo-abo/avy" :commit "e92cb37") package-vc-selected-packages)
;;;;; buffer-expose
(push '(buffer-expose :host github :url "https://github.com/clemera/buffer-expose" :commit "c4a1c74") package-vc-selected-packages)
;;;;; centered-cursor-mode
(push '(centered-cursor-mode :url "https://github.com/andre-r/centered-cursor-mode.el" :commit "ebaeb80") package-vc-selected-packages)
;;;;; centered-window
(push '(centered-window :url "https://github.com/anler/centered-window-mode" :old-names (centered-window-mode) :commit "80965f6") package-vc-selected-packages)
;;;;; corfu
(push '(corfu :host github :branch "main" :url "https://github.com/minad/corfu" :commit "a59c41d"
               :files (:defaults "extensions/corfu-history.el" "extensions/corfu-quick.el")) package-vc-selected-packages)
;;;;; counsel
(push '(counsel :url "https://github.com/abo-abo/swiper" :commit "8f2abd3") package-vc-selected-packages)
;;;;; dash
(push '(dash :url "https://github.com/magnars/dash.el" :commit "7a9c937") package-vc-selected-packages)
;;;;; dash-functional
(push '(dash-functional :url "https://github.com/magnars/dash.el" :commit "7a9c937") package-vc-selected-packages)
;;;;; dashboard
(push '(dashboard :url "https://github.com/emacs-dashboard/emacs-dashboard" :commit "36c8da4") package-vc-selected-packages)
;;;;; decide
(push '(decide :url "https://github.com/lifelike/decide-mode" :commit "668fa55") package-vc-selected-packages)
;;;;; default-text-scale
(push '(default-text-scale :url "https://github.com/purcell/default-text-scale" :commit "bfc0987") package-vc-selected-packages)
;;;;; dirvish
(push '(dirvish :url "https://github.com/alexluigit/dirvish" :commit "ec41006") package-vc-selected-packages)
;;;;; dirvish-collapse
(push '(dirvish-collapse :url "https://github.com/alexluigit/dirvish" :commit "ec41006") package-vc-selected-packages)
;;;;; dirvish-icons
(push '(dirvish-icons :url "https://github.com/alexluigit/dirvish" :commit "ec41006") package-vc-selected-packages)
;;;;; dirvish-media
(push '(dirvish-media :url "https://github.com/alexluigit/dirvish" :commit "ec41006") package-vc-selected-packages)
;;;;; dirvish-subtree
(push '(dirvish-subtree :url "https://github.com/alexluigit/dirvish" :commit "ec41006") package-vc-selected-packages)
;;;;; doct
(push '(doct :url "https://github.com/progfolio/doct" :commit "15974ad") package-vc-selected-packages)
;;;;; edit-indirect
(push '(edit-indirect :url "https://github.com/Fanael/edit-indirect" :commit "bdc8f54") package-vc-selected-packages)
;;;;; ednc
(push '(ednc :url "https://github.com/sinic/ednc" :commit "d1a3c37") package-vc-selected-packages)
;;;;; elfeed
(push '(elfeed :url "https://github.com/skeeto/elfeed" :commit "de4b64b") package-vc-selected-packages)
;;;;; elfeed-org
(push '(elfeed-org :url "https://github.com/remyhonig/elfeed-org" :commit "77b6bbf") package-vc-selected-packages)
;;;;; elfeed-score
(push '(elfeed-score :url "https://github.com/sp1ff/elfeed-score" :commit "5fff415") package-vc-selected-packages)
;;;;; elisp-demos
(push '(elisp-demos :url "https://github.com/xuchunyang/elisp-demos" :commit "ed9578d") package-vc-selected-packages)
;;;;; elisp-refs
(push '(elisp-refs :url "https://github.com/Wilfred/elisp-refs" :branch "master" :commit "bf3cca8") package-vc-selected-packages)
;;;;; ellocate
(push '(ellocate :url "https://github.com/walseb/ellocate" :commit "8140508") package-vc-selected-packages)
;;;;; embark
(push '(embark :url "https://github.com/oantolin/embark" :commit "5d0459d") package-vc-selected-packages)
;;;;; emms
(push '(emms :url "https://git.savannah.gnu.org/git/emms.git" :url "https://github.com/emacsmirror/emms" :commit "5c3226b") package-vc-selected-packages)
;;;;; eros
(push '(eros :url "https://github.com/xiongtx/eros" :commit "dd89102") package-vc-selected-packages)
;;;;; eshell-up
(push '(eshell-up :url "https://github.com/peterwvj/eshell-up" :commit "9c100ba") package-vc-selected-packages)
;;;;; eshell-z
(push '(eshell-z :url "https://github.com/xuchunyang/eshell-z" :commit "337cb24") package-vc-selected-packages)
;;;;; evil
(push '(evil :url "https://github.com/emacs-evil/evil" :commit "cc9d688") package-vc-selected-packages)
;;;;; evil-cleverparens
(push '(evil-cleverparens :url "https://github.com/luxbock/evil-cleverparens" :commit "8c45879") package-vc-selected-packages)
;;;;; evil-easymotion
(push '(evil-easymotion :url "https://github.com/PythonNut/evil-easymotion" :commit "f96c2ed") package-vc-selected-packages)
;;;;; evil-goggles
(push '(evil-goggles :url "https://github.com/edkolev/evil-goggles" :commit "08a2205") package-vc-selected-packages)
;;;;; evil-magit
(push '(evil-magit :url "https://github.com/emacs-evil/evil-magit" :commit "f4a8c8d") package-vc-selected-packages)
;;;;; evil-surround
(push '(evil-surround :url "https://github.com/emacs-evil/evil-surround" :old-names (surround) :commit "346d4d8") package-vc-selected-packages)
;;;;; expand-region
(push '(expand-region :url "https://github.com/magnars/expand-region.el" :commit "ea6b4cb") package-vc-selected-packages)
;;;;; exwm
(push '(exwm :branch "master" :host github :url "https://github.com/ch11ng/exwm" :commit "b62d5e7") package-vc-selected-packages)
;;;;; exwm-edit
(push '(exwm-edit :url "https://github.com/agzam/exwm-edit" :commit "2fd9426") package-vc-selected-packages)
;;;;; exwm-firefox-core
(push '(exwm-firefox-core :url "https://github.com/walseb/exwm-firefox-core" :commit "e2fe2a8") package-vc-selected-packages)
;;;;; exwm-firefox-evil
(push '(exwm-firefox-evil :url "https://github.com/walseb/exwm-firefox-evil" :commit "14643ee") package-vc-selected-packages)
;;;;; exwm-float
;; https://gitlab.com/mtekman/exwm-float.el.git
(push '(exwm-float :vc-backend Git :branch "master" :url "https://gitlab.com/mtekman/exwm-float.el.git" :commit "eb1b60b4") package-vc-selected-packages)
;;;;; f
(push '(f :url "https://github.com/rejeep/f.el" :commit "1814209") package-vc-selected-packages)
;;;;; fennel-mode
(push '(fennel-mode :url "https://git.sr.ht/~technomancy/fennel-mode" :commit "da958db") package-vc-selected-packages)
;;;;; figlet
(push '(figlet :url "https://github.com/jpkotta/figlet" :commit "19a3878") package-vc-selected-packages)
;;;;; frame-cmds
(push '(frame-cmds :url "https://github.com/emacsmirror/frame-cmds" :commit "b803354") package-vc-selected-packages)
;;;;; frame-fns
(push '(frame-fns :url "https://github.com/emacsmirror/frame-fns" :commit "b675ee5") package-vc-selected-packages)
;;;;; gcmh
(push '(gcmh :url "https://gitlab.com/koral/gcmh" :fetcher gitlab :commit "0089f9c") package-vc-selected-packages)
;;;;; git-auto-commit-mode
(push '(git-auto-commit-mode :url "https://github.com/ryuslash/git-auto-commit-mode" :commit "a6b6e0f") package-vc-selected-packages)
;;;;; git-commit
(push '(git-commit :url "https://github.com/magit/magit" :old-names (git-commit-mode) :commit "86eec7b") package-vc-selected-packages)
;;;;; git-gutter+
(push '(git-gutter+ :url "https://github.com/nonsequitur/git-gutter-plus" :commit "b772699") package-vc-selected-packages)
;;;;; goto-chg
(push '(goto-chg :url "https://github.com/emacs-evil/goto-chg" :commit "2af6121") package-vc-selected-packages)
;;;;; grugru
;; =Grugru= is a package that lets me switch.
(push '(grugru :url "https://github.com/ROCKTAKEY/grugru" :commit "92e588e") package-vc-selected-packages)
;;;;; helm
(push '(helm :url "https://github.com/emacs-helm/helm" :commit "8de5444") package-vc-selected-packages)
;;;;; helm-core
(push '(helm-core :url "https://github.com/emacs-helm/helm" :commit "8de5444") package-vc-selected-packages)
;;;;; helm-system-packages
(push '(helm-system-packages :url "https://github.com/emacs-helm/helm-system-packages" :commit "e93f4ae") package-vc-selected-packages)
;;;;; helpful
(push '(helpful :url "https://github.com/Wilfred/helpful" :branch "master" :commit "6f8991a") package-vc-selected-packages)
;;;;; hide-mode-line
(push '(hide-mode-line :url "https://github.com/hlissner/emacs-hide-mode-line" :commit "8888882") package-vc-selected-packages)
;;;;; highlight-quoted
(push '(ht :url "https://github.com/Wilfred/ht.el" :commit "2850301") package-vc-selected-packages)
;;;;; hydra
(push '(hydra :url "https://github.com/abo-abo/hydra" :commit "2d55378") package-vc-selected-packages)
;;;;; ialign
(push '(ialign :url "https://github.com/mkcms/interactive-align" :commit "bc4d30d") package-vc-selected-packages)
;;;;; idle-require
(push '(idle-require :url "https://github.com/nschum/idle-require.el" :commit "33592bb") package-vc-selected-packages)
;;;;; iedit
(push '(iedit :url "https://github.com/victorhge/iedit" :commit "27c6186") package-vc-selected-packages)
;;;;; ivy
(push '(ivy :url "https://github.com/abo-abo/swiper" :commit "8f2abd3") package-vc-selected-packages)
;;;;; key-chord
(push '(key-chord :url "https://github.com/emacsorphanage/key-chord" :commit "7f7fd7c") package-vc-selected-packages)
;;;;; lispy
(push '(lispy :url "https://github.com/abo-abo/lispy" :commit "1ad128b") package-vc-selected-packages)
;;;;; lispyville
(push '(lispyville :url "https://github.com/noctuid/lispyville" :commit "0f13f26") package-vc-selected-packages)
;;;;; loopy
(push '(loopy :url "https://github.com/okamsn/loopy" :commit "31dc58f") package-vc-selected-packages)
;;;;; lv
(push '(lv :url "https://github.com/abo-abo/hydra" :commit "2d55378") package-vc-selected-packages)
;;;;; macrostep
(push '(macrostep :url "https://github.com/joddie/macrostep" :commit "424e373") package-vc-selected-packages)
;;;;; magit
(push '(magit :url "https://github.com/magit/magit" :commit "86eec7b") package-vc-selected-packages)
;;;;; magit-section
(push '(magit-section :url "https://github.com/magit/magit" :commit "86eec7b") package-vc-selected-packages)
;;;;; map
(push '(map :host github :url "https://github.com/emacs-straight/map" :commit "dc4f657") package-vc-selected-packages)
;;;;; marginalia
(push '(marginalia :url "https://github.com/minad/marginalia" :commit "b65d66e") package-vc-selected-packages)
;;;;; markdown-mode
(push '(markdown-mode :url "https://github.com/jrblevin/markdown-mode" :commit "c338cdf") package-vc-selected-packages)
;;;;; mini-modeline
(push '(mini-modeline :url "https://github.com/kiennq/emacs-mini-modeline" :commit "7dcd0ab") package-vc-selected-packages)
;;;;; mmt
(push '(mmt :url "https://github.com/mrkkrp/mmt" :commit "d772956") package-vc-selected-packages)
;;;;; modus-themes
(push '(modus-themes :url "https://github.com/protesilaos/modus-themes" :commit "38236a9") package-vc-selected-packages)
;;;;; notmuch
(push '(notmuch :url "https://git.notmuchmail.org/git/notmuch" :fetcher git :commit "a5f7efd") package-vc-selected-packages)
;;;;; orderless
(push '(orderless :url "https://github.com/oantolin/orderless" :commit "cbc0109") package-vc-selected-packages)
;;;;; org-auto-tangle
(push '(org-auto-tangle :url "https://github.com/yilkalargaw/org-auto-tangle" :commit "2494a6f") package-vc-selected-packages)
;;;;; org-ml
(push '(org-ml :url "https://github.com/ndwarshuis/org-ml" :commit "385e3be") package-vc-selected-packages)
;;;;; org-ql
(push '(org-ql :url "https://github.com/alphapapa/org-ql" :commit "d7ada53") package-vc-selected-packages)
;;;;; org-remark
(push '(org-remark :host github :url "https://github.com/emacs-straight/org-remark" :commit "7e72e86") package-vc-selected-packages)
;;;;; org-super-agenda
(push '(org-super-agenda :url "https://github.com/alphapapa/org-super-agenda" :commit "f5e80e4") package-vc-selected-packages)
;;;;; org-superstar
(push '(org-superstar :url "https://github.com/integral-dw/org-superstar-mode" :commit "7f83636") package-vc-selected-packages)
;;;;; ov
(push '(ov :url "https://github.com/emacsorphanage/ov" :commit "c5b9aa4") package-vc-selected-packages)
;;;;; paredit
(push '(paredit :fetcher nil :url "https://mumble.net/~campbell/git/paredit.git" :url "https://github.com/https://mumble.net/~campbell/git/paredit.git" :commit "d0b1a2f") package-vc-selected-packages)
;;;;; pass
(push '(pass :url "https://github.com/NicolasPetton/pass" :commit "a095d24") package-vc-selected-packages)
;;;;; password-generator
(push '(password-generator :url "https://github.com/vandrlexay/emacs-password-genarator" :commit "c1da979") package-vc-selected-packages)
;;;;; password-store
(push '(password-store :url "https://github.com/zx2c4/password-store" :commit "f152064") package-vc-selected-packages)
;;;;; password-store-otp
(push '(password-store-otp :url "https://github.com/volrath/password-store-otp.el" :commit "04998c8") package-vc-selected-packages)
;;;;; pinentry
(push '(pinentry :host github :url "https://github.com/emacs-straight/pinentry" :commit "cd942f7") package-vc-selected-packages)
;;;;; plural
(push '(plural :url "https://github.com/emacsmirror/plural" :commit "b91ce15") package-vc-selected-packages)
;;;;; popup
(push '(popup :url "https://github.com/auto-complete/popup-el" :commit "bd5a0df") package-vc-selected-packages)
;;;;; popwin
(push '(popwin :url "https://github.com/emacsorphanage/popwin" :commit "215d6cb") package-vc-selected-packages)
;;;;; rainbow-delimiters
(push '(rainbow-delimiters :url "https://github.com/Fanael/rainbow-delimiters" :commit "f43d48a") package-vc-selected-packages)
;;;;; redacted
(push '(redacted :url "https://github.com/bkaestner/redacted.el" :commit "156311e") package-vc-selected-packages)
;;;;; restart-emacs
(push '(restart-emacs :url "https://github.com/iqbalansari/restart-emacs" :commit "1607da2") package-vc-selected-packages)
;;;;; s
(push '(s :url "https://github.com/magnars/s.el" :commit "43ba8b5") package-vc-selected-packages)
;;;;; search-web
(push '(search-web :url "https://github.com/tomoya/search-web.el" :commit "c4ae86a") package-vc-selected-packages)
;;;;; shut-up
(push '(shut-up :url "https://github.com/cask/shut-up" :commit "081d6b0") package-vc-selected-packages)
;;;;; smartparens
(push '(smartparens :url "https://github.com/Fuco1/smartparens" :commit "63695c6") package-vc-selected-packages)
;;;;; spell-number
(push '(spell-number :url "https://github.com/emacsmirror/spell-number" :commit "3ce612d") package-vc-selected-packages)
;;;;; super-save
(push '(super-save :url "https://github.com/bbatsov/super-save" :commit "886b551") package-vc-selected-packages)
;;;;; swiper
(push '(swiper :url "https://github.com/abo-abo/swiper" :commit "8f2abd3") package-vc-selected-packages)
;;;;; swiper-helm
(push '(swiper-helm :url "https://github.com/abo-abo/swiper-helm" :commit "93fb6db") package-vc-selected-packages)
;;;;; tempel
(push '(tempel :url "https://github.com/minad/tempel" :commit "b4bb703") package-vc-selected-packages)
;;;;; transient
(push '(transient :url "https://github.com/magit/transient" :commit "90e640f") package-vc-selected-packages)
;;;;; transpose-frame
(push '(transpose-frame :url "https://github.com/emacsorphanage/transpose-frame" :commit "12e523d") package-vc-selected-packages)
;;;;; treepy
(push '(treepy :url "https://github.com/Luis-Henriquez-Perez/treepy.el" :commit "191d84c") package-vc-selected-packages)
;;;;; ts
(push '(ts :url "https://github.com/alphapapa/ts.el" :commit "b7ca357") package-vc-selected-packages)
;;;;; undo-tree
(push '(undo-tree :host github :url "https://github.com/emacs-straight/undo-tree" :commit "e326c61") package-vc-selected-packages)
;;;;; vc-auto-commit
(push '(vc-auto-commit :url "https://github.com/thisirs/vc-auto-commit" :commit "56f4780") package-vc-selected-packages)
;;;;; vertico
(push '(vertico :host github :branch "main" :url "https://github.com/minad/vertico" :commit "956c81b"
                 :files (:defaults "extensions/vertico-buffer.el" "extensions/vertico-quick.el" "extensions/vertico-directory.el")) package-vc-selected-packages)
;;;;; which-key
(push '(which-key :url "https://github.com/justbur/emacs-which-key" :commit "428aedf") package-vc-selected-packages)
;;;;; with-editor
(push '(with-editor :url "https://github.com/magit/with-editor" :commit "139ef39") package-vc-selected-packages)
;;;;; with-emacs
(push '(with-emacs :url "https://github.com/twlz0ne/with-emacs.el" :commit "9f99bec") package-vc-selected-packages)
;;;;; workgroups2
(push '(workgroups2 :url "https://github.com/pashinin/workgroups2" :commit "c9403c6") package-vc-selected-packages)
;;;;; xelb
(push '(xelb :host github :url "https://github.com/emacs-straight/xelb" :commit "f5880e6") package-vc-selected-packages)
;;;;; xr
(push '(xr :host github :url "https://github.com/emacs-straight/xr" :commit "277c549") package-vc-selected-packages)
;;;;; ssh-agency
;; This package automates the process of starting up =ssh-agent= and connecting to
;; it "so that pushes and pulls from magit will not require any passphrase".  This
;; is exactly what I had been looking for so I could [[][auto push and commit]].
(push '(ssh-agency :url "https://github.com/magit/ssh-agency" :commit "a5377e4") package-vc-selected-packages)
;;;;; zone-matrix
(push '(zone-matrix :url "https://github.com/emacsmirror/zone-matrix" :commit "e1fc8c7") package-vc-selected-packages)
;;;;; zone-rainbow
(push '(zone-rainbow :url "https://github.com/kawabata/zone-rainbow" :commit "2ba4f1a") package-vc-selected-packages)
;;;;; zone-sl
(push '(zone-sl :url "https://github.com/kawabata/zone-sl" :commit "7ec22e3") package-vc-selected-packages)
;;;;; zoom-frm
(push '(zoom-frm :url "https://github.com/emacsmirror/zoom-frm" :commit "59e2fce" ) package-vc-selected-packages)
;;;;; zoom-window
(push '(zoom-window :url "https://github.com/emacsorphanage/zoom-window" :commit "474ca47") package-vc-selected-packages)
;;;;; lgr
;; This package provides functions and macro to log information.  It's the most
;; recent package for logging I could find.
(push '(lgr :url "https://github.com/Fuco1/Emacs-lgr" :commit "4ab6c22") package-vc-selected-packages)
;;;;; compat 
;; (push '(compat :url "https://github.com/emacs-compat/compat") package-vc-selected-packages)
;; (push '(list-utils :url "https://github.com/rolandwalker/list-utils") package-vc-selected-packages)
;; (push '(zoutline :url "https://github.com/abo-abo/zoutline") package-vc-selected-packages)
;;;;; setup
;; This package is similar to =use-package=.  Essentially, it's an abstraction to
;; configure packages.  There are parts of it that I agree with and parts that I
;; don't.  One thing I will say is I don't like the use of it that tries to create
;; a dsl for forms building a language.  One of the first examples from its site
;; illustrates this.  It says that this ~(setup foo-mode (:hook bar-mode))~ expands
;; to ~(add-hook 'foo-mode-hook #'bar-mode)~.  I think that the macro is actually far
;; inferior to the normal =add-hook= invocation because it hides what's going on for
;; no good reason.  What I do see is the need to abstract things for cross
;; configuring packages.

;; What I want is a uniform, declarative interface to configuring things that come
;; up often across several packages.
(push '(setup :url "https://git.sr.ht/~pkal/setup" :commit "b2e3f3a") package-vc-selected-packages)
;;;;; noflet
(push '(noflet :branch "master" :url "https://github.com/nicferrier/emacs-noflet" :commit "7ae84dc3257637af7334101456dafe1759c6b68a") package-vc-selected-packages)
;;;; Install Packages
;; I am aware that there is a function for doing this
;; =package-vc-install-selected-packages=.  The problem with this function is
;; that I cannot find a way to install specific package specs along with the
;; specified version.  That function respects the specs I specify but it does
;; not respect the version.  There does not seem to be anywhere to include the
;; version in it actually.  So it looks like I need to do it myself by invoking
;; =package-vc-install= directly with the proper arguments.

;; One question is how do I determine whether a package installation failed or
;; not--that I want to know.  Compile warnings I do not care much about to be
;; honest.  So yea, right now I want to suppress the compile messages but keep
;; certain ones.

;; O.K. so I had thought the revision could specify a commit, but it can't.  So
;; after I clone that package but before unpacking it, I need to checkout the
;; proper commit.

(defun oo-checkout-commit-before-setup (commit fn pkg-desc pkg-dir)
  (let ((default-directory pkg-dir)
        (buff (generate-new-buffer "vc-checkout")))
    (unwind-protect (progn
                      (message "Checkout %s in %s..." commit pkg-dir)
                      (vc-do-command buff 0 "git" nil "checkout" commit))
      (kill-buffer buff))
    (message "Activating %s...")
    (apply fn pkg-desc pkg-dir)))

(pcase-dolist (`(,name . ,spec) package-vc-selected-packages)
  (when (stringp name) (setq name (intern name)))
  (let* ((pkg-descs (assoc name package-alist))
         (recipe (cons name spec))
         (pkg-desc (package-desc-create :name name :kind 'vc))
         (url (plist-get spec :url))
         (commit (plist-get spec :commit)))
    (unless (seq-some #'package-vc-p (cdr pkg-descs))
      (message "Cloning %s from %s..." name url)
      (flet! ((package-vc--unpack-1 (desc dir) (message "Cloning %s from %s..." name url) (funcall this-fn desc dir))) (package-vc-install recipe)))))

;; Honestly I might not deal with =package-vc-checkout= and
;; =package-vc-install-from-checkout=.  I have had a hard time with these
;; functions.  I think their choice of arguments is strange and I think it is
;; not good for =package-vc-checkout= to change to the directory.  The
;; alternative is to advice =package--vc-unpack-1=.
