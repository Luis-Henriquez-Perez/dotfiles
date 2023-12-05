;; **** recipes
;; :PROPERTIES:
;; :ID:       20230914T171534.475976
;; :END:
;; ***** elpaca
;; ***** edwina
;; :PROPERTIES:
;; :ID:       20231010T195639.822840
;; :END:
;; There are [[several]] emacs packages that save and restore window configuration. But
;; before being able to save and restore window configurations you need to create a
;; window configuration and that process can be repetitive and tedious. This
;; package actually automates the building of a window configuration from one
;; window.  This package provides tools to configure =display-buffer= so that when
;; called I can open a buffer in a [[file:screenshots/edwina-usage.png][master-slave layout]] (see [[file:screenshots/edwina-usage.png][edwina's usage]]).
(elpaca (edwina :fetcher gitlab :repo "ajgrf/edwina" :ref "f95c31b" :branch "master"))
;; ***** htmlize
;; :PROPERTIES:
;; :ID:       20231009T093234.539078
;; :END:
;; This package converts emacs buffers to html.  I use it in combination with the
;; command-line utility [[][wkhtmltoimage]] to take [[id:20230802T153110.062543][snapshots]] of emacs buffers.
;; There is a similar built-in emacs package [[][html-fontify]] that I have tried
;; to use, but I encountered [[][an error]].
(elpaca (htmlize :fetcher github :repo "hniksic/emacs-htmlize" :ref "dd27bc3" :branch "master"))
;; ***** org-appear
;; :PROPERTIES:
;; :ID:       20230919T095126.299286
;; :END:
;; This package allows for the editing of org emphasis markers and links in a
;; "do-what-I-mean" type way.  Typically when I edit an org link or markup I have
;; to manually toggle [[][]] and then edit it. That's because I tell org mode to
;; display links and emphasis markers in a "prettified" way.  Although it's more
;; pleasant to the eye to see emphasis markers this way; it is harder to edit
;; them.  So what will end up happening is I toggle.  What this package does is
;; make it so if your cursor is on the emphasis marker or link, it's visibility
;; automatically toggled.
(elpaca (org-appear :fetcher github :repo "awth13/org-appear" :ref "eb9f9db" :branch "master"))
;; ***** noflet
(elpaca (noflet :fetcher github :branch "master" :repo "nicferrier/emacs-noflet" :ref "7ae84dc3257637af7334101456dafe1759c6b68a"))
;; ***** outshine
;; :PROPERTIES:
;; :ID:       20230914T155737.045932
;; :END:
;; Outshine is an Emacs package that allows for the use of org mode in non org
;; files.  It is what I use to allow me to go back and forth between org mode
;; features.  It promises a kind of "have your cake and eat it too" type thing
;; where you can utilize the features of org mode--which include markup support,
;; link support, headline refiling, etc--while not paying the price of needing
;; an initial tangling script.
(elpaca (outshine :fetcher github :repo "alphapapa/outshine" :ref "bf1eed1"))
;; ***** outorg
;; :PROPERTIES:
;; :ID:       20230914T155733.094553
;; :END:
;; Outshine depends on the package Outorg.  Outorg is what outshine uses to
;; convert back and forth between a file with outshine syntax and an org file.
(elpaca (outorg :fetcher github :repo "alphapapa/outorg" :ref "ef0f86f"))
;; ***** navi-mode
;; :PROPERTIES:
;; :ID:       20230914T155730.004610
;; :END:
;; This package is a dependency of `outorg'.  It has to do with providing
;; navigation commands for `outshine'.
(elpaca (navi-mode :fetcher github :repo "alphapapa/navi" :ref "cf97e1e"))
;; ***** orglink
;; This package lets me display org links in non org buffer.  In practice I use
;; this for buffers with outshine enabled.
(elpaca (orglink :fetcher github :repo "tarsius/orglink" :ref "afbeffd"))
;; ***** chezmoi
;; :PROPERTIES:
;; :ID:       20230914T155724.680294
;; :END:
;; This package provides some functions to interface with =chezmoi=, a dotfile
;; manager.  At first--and even still to be honest I wanted to use Org as a
;; dotfile manager; but org although having some useful dotfile management tools
;; (such as tangling) is not designed to be a dotfile manager.  Whereas,
;; =chezmoi= was actually designed to solve many dotfile problems.  In the
;; future, I'd like to scrap =chezmoi= and use a dotfile manager written in
;; Elisp; preferably with the optional of optionally using org mode tools for
;; dotfile management.
(elpaca (chezmoi :fetcher github :repo "tuh8888/chezmoi.el" :ref "1389782"))
;; ***** lua-mode
;; :PROPERTIES:
;; :ID:       20230914T171004.330624
;; :END:
;; Emacs doesn't have a mode for lua built-in--not to say that's a bad
;; thing.  This package provides such a mode.
(elpaca (lua-mode :fetcher github :repo "immerrr/lua-mode" :ref "7eb8eaa"))
;; ***** org-bookmark-heading
;; :PROPERTIES:
;; :ID:       20230906T111914.254820
;; :END:
;; This package provides bookmark functions catered specifically for org files.
;; Typically bookmarks store where something is by using a combination of
;; =point= as well as surrounding text that can be searched for in text files.
;; Obviously, this is not perfect but because a bookmark may easily become
;; Broken when you edit the region.  With org you can do better because each
;; Headline has a unique ID.  This package provides the function for storing the
;; information necessary and it provides the function to jump to that ID.
(elpaca (org-bookmark-heading :fetcher github :repo "alphapapa/org-bookmark-heading" :ref "4e97fab"))
;; ***** cape
;; :PROPERTIES:
;; :ID:       20230906T105029.007786
;; :END:
;; This package provides several =capfs= and utility functions for working with
;; =capfs=.  By the way =capfs= stands for a [[][completion at point function]].
;; I am particular interested in a function it provides to merge =capfs=.
(elpaca (cape :repo "minad/cape" :branch "main" :fetcher github :ref "4645762"))
;; ***** filladapt
;; :PROPERTIES:
;; :ID:       20230905T210004.690956
;; :END:
(elpaca (filladapt :repo "git://git.sv.gnu.org/emacs/elpa" :branch "externals/filladapt" :ref "802c194"))
;; ***** burly
;; :PROPERTIES:
;; :ID:       20230905T205622.304023
;; :END:
;; Burly provides functions that can save a window configuration via bookmarks.
;; There are a plethora of packages out there provide ways to manage window
;; configurations; to name a few: [[about:blank][eyebrowse]], [[https://github.com/knu/elscreen][persp-mode]], [[https://github.com/knu/elscreen][elscreen]], [[https://elpa.gnu.org/packages/wconf.html][wconf]].  What
;; makes Burly different is that instead of making its own abstraction for
;; window configuration saving, it uses bookmarks which already have existing
;; support in Emacs.
(elpaca (burly :fetcher github :repo "alphapapa/burly.el" :branch "master" :ref "f570fa8"))
;; ***** dogears
;; :PROPERTIES:
;; :ID:       20230827T190849.871899
;; :END:
(elpaca (dogears :fetcher github :repo "alphapapa/dogears.el" :files (:defaults (:exclude "helm-dogears.el"))))
;; ***** captain
;; :PROPERTIES:
;; :ID:       20230824T145027.998151
;; :END:
;; This package provides me with auto-capitalization.  It checks each word as I
;; type and after I finish typing that word it checks whether or not it should
;; be capitalized.
(elpaca (captain :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo "captain" :branch "externals/captain" :ref "364ee98"))
;; ***** lambda-themes
;; :PROPERTIES:
;; :ID:       20230807T140023.427948
;; :END:
;; These are some beautiful and elegant themes.  They are on the minimal side, but I
;; like that very much.
(elpaca (lambda-themes :repo "Lambda-Emacs/lambda-themes" :branch "main" :fetcher github :ref "7342250"))
;; ***** consult
;; :PROPERTIES:
;; :ID:       20230116T114841.931887
;; :END:
(elpaca (consult :repo "minad/consult" :fetcher github :branch "main" :ref "fae9b50"))
;; ***** textsize
;; :PROPERTIES:
;; :ID:       20230116T081738.677037
;; :END:
(elpaca (textsize :repo "WJCFerguson/textsize" :fetcher github :branch "master" :ref "df91392"))
;; ***** on.el
;; :PROPERTIES:
;; :ID:       20230114T155809.027252
;; :END:
;; This package provides hooks inspired by [[][Doom Emacs]].  Generally, the main
;; idea behind the hooks these package offers is to provide ways to improve
;; performance usually by spreading out the loading of features.  The hooks it
;; provides are [[][]].
(elpaca (on :host github :repo "ajgrf/on.el" :branch "master" :ref "3cf623e"))
;; ***** ligature
;; :PROPERTIES:
;; :ID:       20230116T082105.773563
;; :END:
(elpaca (ligature :fetcher github :repo "mickeynp/ligature.el" :ref "3d14604"))
;; ***** org
;; :PROPERTIES:
;; :ID:       20221224T154312.085877
;; :END:
(elpaca (org :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :ref "f731d45"))
;; ***** refine
;; :PROPERTIES:
;; :ID:       20221212T072440.158706
;; :END:
(elpaca (refine :repo "Wilfred/refine" :fetcher github :ref "d72fa50"))
;; ***** ace-window
;; :PROPERTIES:
;; :ID:       a9af3603-79f9-4ab9-b8b0-cd62da54a0b6
;; :END:
(elpaca (ace-window :repo "abo-abo/ace-window" :fetcher github :ref "c7cb315"))
;; ***** aggressive-fill-paragraph
;; :PROPERTIES:
;; :ID:       88530387-52a0-42d1-abdd-d5cfc24a8bf4
;; :END:
(elpaca (aggressive-fill-paragraph :fetcher github :repo "davidshepherd7/aggressive-fill-paragraph-mode" :ref "4a620e6"))
;; ***** aggressive-indent
;; :PROPERTIES:
;; :ID:       e4e48016-f267-44d3-9056-3df03df40870
;; :END:
(elpaca (aggressive-indent :repo "Malabarba/aggressive-indent-mode" :fetcher github :ref "b0ec004"))
;; ***** all-the-icons
;; :PROPERTIES:
;; :ID:       23f25241-7033-4ea2-b741-d757fcf0b2ca
;; :END:
(elpaca (all-the-icons :repo "domtronn/all-the-icons.el" :fetcher github :ref "be99987"))
;; ***** all-the-icons-completion
;; :PROPERTIES:
;; :ID:       907942ab-cbdf-4ab4-a8ed-94c9c9fdcd29
;; :END:
(elpaca (all-the-icons-completion :repo "iyefrat/all-the-icons-completion" :fetcher github :ref "286e2c0"))
;; ***** anaphora
;; :PROPERTIES:
;; :ID:       73f2264a-70cf-4bbc-8bae-31cb1f7bd1b9
;; :END:
;; This package provides me with common [[https://en.wikipedia.org/wiki/Anaphoric_macro][anaphoric macros]].
(elpaca (anaphora :repo "rolandwalker/anaphora" :fetcher github :ref "3b2da3f"))
;; ***** async
;; :PROPERTIES:
;; :ID:       7854e9f7-ca80-406b-be93-94f3af724ab3
;; :END:
(elpaca (async :repo "jwiegley/emacs-async" :fetcher github :ref "9a8cd0c"))
;; ***** auth-source-pass
;; :PROPERTIES:
;; :ID:       17b178c3-c82a-4e8b-b458-0696fe37b3df
;; :END:
(elpaca (auth-source-pass :fetcher github :repo "DamienCassou/auth-source-pass" :ref "aa7f171"))
;; ***** auto-capitalize
;; :PROPERTIES:
;; :ID:       fdaa28dd-86a3-4abc-8cbc-b13f96b8a777
;; :END:
(elpaca (auto-capitalize :fetcher github :repo "emacsmirror/auto-capitalize" :ref "0ee14c7"))
;; ***** avy
;; :PROPERTIES:
;; :ID:       29b9606d-3f2b-484d-aadf-0875322bc66e
;; :END:
(elpaca (avy :repo "abo-abo/avy" :fetcher github :ref "e92cb37"))
;; ***** buffer-expose
;; :PROPERTIES:
;; :ID:       9a11dbe2-9e07-4c21-a21b-79f9df389025
;; :END:
(elpaca (buffer-expose :host github :repo "clemera/buffer-expose" :fetcher github :ref "c4a1c74"))
;; ***** centered-cursor-mode
;; :PROPERTIES:
;; :ID:       0b8bac4e-d720-439d-aec7-5281a7e6682e
;; :END:
(elpaca (centered-cursor-mode :fetcher github :repo "andre-r/centered-cursor-mode.el" :ref "ebaeb80"))
;; ***** centered-window
;; :PROPERTIES:
;; :ID:       40410cd5-fb58-4b71-9d4e-5f9b3a736653
;; :END:
(elpaca (centered-window :fetcher github :repo "anler/centered-window-mode" :old-names (centered-window-mode) :ref "80965f6"))
;; ***** corfu
;; :PROPERTIES:
;; :ID:       601cf820-6f77-44f4-958e-27345f685b70
;; :END:
(elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref "a59c41d"
               :files (:defaults "extensions/corfu-history.el" "extensions/corfu-quick.el")))
;; ***** counsel
;; :PROPERTIES:
;; :ID:       f37029eb-006f-469b-b4c4-385b432c06b8
;; :END:
(elpaca (counsel :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))
;; ***** dash
;; :PROPERTIES:
;; :ID:       27b6c230-6d77-47ab-90ce-a8862fd23a39
;; :END:
(elpaca (dash :fetcher github :repo "magnars/dash.el" :ref "7a9c937"))
;; ***** dash-functional
;; :PROPERTIES:
;; :ID:       3b851884-f0d9-46d7-ba7b-fa9c756ffdce
;; :END:
(elpaca (dash-functional :fetcher github :repo "magnars/dash.el" :ref "7a9c937"))
;; ***** dashboard
;; :PROPERTIES:
;; :ID:       824bb6a2-c482-443f-a1c3-7eb5e0f644c1
;; :END:
(elpaca (dashboard :fetcher github :repo "emacs-dashboard/emacs-dashboard" :ref "36c8da4"))
;; ***** decide
;; :PROPERTIES:
;; :ID:       e5b5b2a7-619a-4394-bb00-9dbcbe4f8327
;; :END:
(elpaca (decide :fetcher github :repo "lifelike/decide-mode" :ref "668fa55"))
;; ***** default-text-scale
;; :PROPERTIES:
;; :ID:       bdde6aeb-c624-4a40-bfbd-9ffc8748413f
;; :END:
(elpaca (default-text-scale :fetcher github :repo "purcell/default-text-scale" :ref "bfc0987"))
;; ***** dirvish
;; :PROPERTIES:
;; :ID:       ed269614-a449-46e3-b534-7c680c15c5a6
;; :END:
(elpaca (dirvish :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;; ***** dirvish-collapse
;; :PROPERTIES:
;; :ID:       7b3fa568-c770-4e22-9a52-4983ba12754f
;; :END:
(elpaca (dirvish-collapse :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;; ***** dirvish-icons
;; :PROPERTIES:
;; :ID:       de14b784-b5f9-4695-8a4b-e8868d9d3c6e
;; :END:
(elpaca (dirvish-icons :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;; ***** dirvish-media
;; :PROPERTIES:
;; :ID:       de8e897a-8606-4796-a1bd-e85e854fcf66
;; :END:
(elpaca (dirvish-media :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;; ***** dirvish-subtree
;; :PROPERTIES:
;; :ID:       4e013ea5-452b-4355-9279-4d7548cc7401
;; :END:
(elpaca (dirvish-subtree :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;; ***** doct
;; :PROPERTIES:
;; :ID:       da5c3402-79fc-43a0-8d7a-40cceb91681b
;; :END:
(elpaca (doct :repo "progfolio/doct" :fetcher github :ref "15974ad"))
;; ***** edit-indirect
;; :PROPERTIES:
;; :ID:       23093401-44f5-409f-a38c-5ac42ceb3592
;; :END:
(elpaca (edit-indirect :fetcher github :repo "Fanael/edit-indirect" :ref "bdc8f54"))
;; ***** ednc
;; :PROPERTIES:
;; :ID:       54af7c3f-5bc5-4e5d-8966-dbb47f9343df
;; :END:
(elpaca (ednc :repo "sinic/ednc" :fetcher github :ref "d1a3c37"))
;; ***** elfeed
;; :PROPERTIES:
;; :ID:       d1095193-3e69-47db-b417-51e1ad4b7804
;; :END:
(elpaca (elfeed :repo "skeeto/elfeed" :fetcher github :ref "de4b64b"))
;; ***** elfeed-org
;; :PROPERTIES:
;; :ID:       46cc4c63-49c4-4b56-935d-2a10d13a751d
;; :END:
(elpaca (elfeed-org :repo "remyhonig/elfeed-org" :fetcher github :ref "77b6bbf"))
;; ***** elfeed-score
;; :PROPERTIES:
;; :ID:       ad8ce653-aad8-4993-90b2-f0b856435dc0
;; :END:
(elpaca (elfeed-score :fetcher github :repo "sp1ff/elfeed-score" :ref "5fff415"))
;; ***** elisp-demos
;; :PROPERTIES:
;; :ID:       4467475c-e4d2-4f75-a1bb-5c3e74aa7216
;; :END:
(elpaca (elisp-demos :fetcher github :repo "xuchunyang/elisp-demos" :ref "ed9578d"))
;; ***** elisp-refs
;; :PROPERTIES:
;; :ID:       dfa7c79f-3955-4e53-ac0d-6af60404e3c1
;; :END:
(elpaca (elisp-refs :repo "Wilfred/elisp-refs" :branch "master" :fetcher github :ref "bf3cca8"))
;; ***** ellocate
;; :PROPERTIES:
;; :ID:       337766df-8aaf-457e-b7ee-9337e373b4af
;; :END:
(elpaca (ellocate :fetcher github :repo "walseb/ellocate" :ref "8140508"))
;; ***** embark
;; :PROPERTIES:
;; :ID:       d55e9a5a-71de-469a-814a-89ed0600fa9a
;; :END:
(elpaca (embark :repo "oantolin/embark" :fetcher github :ref "5d0459d"))
;; ***** emms
;; :PROPERTIES:
;; :ID:       88e49aa5-a5a7-4976-9d94-90fb2d5a4614
;; :END:
(elpaca (emms :fetcher github :url "https://git.savannah.gnu.org/git/emms.git" :repo "emacsmirror/emms" :ref "5c3226b"))
;; ***** eros
;; :PROPERTIES:
;; :ID:       74bfede3-db33-49de-8318-df27596f267b
;; :END:
(elpaca (eros :fetcher github :repo "xiongtx/eros" :ref "dd89102"))
;; ***** eshell-up
;; :PROPERTIES:
;; :ID:       5f48e81a-ab37-4716-b513-051d170ffb7f
;; :END:
(elpaca (eshell-up :fetcher github :repo "peterwvj/eshell-up" :ref "9c100ba"))
;; ***** eshell-z
;; :PROPERTIES:
;; :ID:       b4f34808-dd00-4bbc-a3a4-30076a2b149b
;; :END:
(elpaca (eshell-z :fetcher github :repo "xuchunyang/eshell-z" :ref "337cb24"))
;; ***** evil
;; :PROPERTIES:
;; :ID:       47a108d5-4528-4441-bb5d-dd3b7bd2ffda
;; :END:
(elpaca (evil :repo "emacs-evil/evil" :fetcher github :ref "cc9d688"))
;; ***** evil-cleverparens
;; :PROPERTIES:
;; :ID:       290e07ea-761f-4d61-933b-847b50d5e12f
;; :END:
(elpaca (evil-cleverparens :fetcher github :repo "luxbock/evil-cleverparens" :ref "8c45879"))
;; ***** evil-easymotion
;; :PROPERTIES:
;; :ID:       f36875c6-5e49-4959-bf11-93936f33b9f6
;; :END:
(elpaca (evil-easymotion :repo "PythonNut/evil-easymotion" :fetcher github :ref "f96c2ed"))
;; ***** evil-goggles
;; :PROPERTIES:
;; :ID:       f64f0b90-28e0-4209-be2d-bf8598657451
;; :END:
(elpaca (evil-goggles :repo "edkolev/evil-goggles" :fetcher github :ref "08a2205"))
;; ***** evil-magit
;; :PROPERTIES:
;; :ID:       00a6bdb5-ff07-4e7f-89b8-d3b939c265cb
;; :END:
(elpaca (evil-magit :fetcher github :repo "emacs-evil/evil-magit" :ref "f4a8c8d"))
;; ***** evil-surround
;; :PROPERTIES:
;; :ID:       29d829c6-005c-420c-bca0-53b72582b622
;; :END:
(elpaca (evil-surround :repo "emacs-evil/evil-surround" :fetcher github :old-names (surround) :ref "346d4d8"))
;; ***** expand-region
;; :PROPERTIES:
;; :ID:       f6fd968a-c08d-42e2-91ec-4d1168ae0509
;; :END:
(elpaca (expand-region :repo "magnars/expand-region.el" :fetcher github :ref "ea6b4cb"))
;; ***** exwm
;; :PROPERTIES:
;; :ID:       552c31d2-12e4-420d-a967-f3afa8ada923
;; :END:
(elpaca (exwm :branch "master" :host github :repo "ch11ng/exwm" :fetcher github :ref "b62d5e7"))
;; ***** exwm-edit
;; :PROPERTIES:
;; :ID:       d19ca9ef-2475-4e2e-8fc3-8cec8c931f77
;; :END:
(elpaca (exwm-edit :repo "agzam/exwm-edit" :fetcher github :ref "2fd9426"))
;; ***** exwm-firefox-core
;; :PROPERTIES:
;; :ID:       6efe88da-2b36-4460-baad-61e374d452da
;; :END:
(elpaca (exwm-firefox-core :fetcher github :repo "walseb/exwm-firefox-core" :ref "e2fe2a8"))
;; ***** exwm-firefox-evil
;; :PROPERTIES:
;; :ID:       a02a90af-0911-4ada-8b8c-f3adf983f4f6
;; :END:
(elpaca (exwm-firefox-evil :fetcher github :repo "walseb/exwm-firefox-evil" :ref "14643ee"))
;; ***** exwm-float
;; :PROPERTIES:
;; :ID:       62c9a4d0-b89f-4a4c-a727-7b49a3f9b9ed
;; :END:
(elpaca (exwm-float :fetcher gitlab :repo "mtekman/exwm-float.el" :ref "eb1b60b"))
;; ***** f
;; :PROPERTIES:
;; :ID:       846389a5-6878-4c63-973f-72d4963c49eb
;; :END:
(elpaca (f :fetcher github :repo "rejeep/f.el" :ref "1814209"))
;; ***** fennel-mode
;; :PROPERTIES:
;; :ID:       60071ed0-9459-4fb3-ab4b-208f32761896
;; :END:
(elpaca (fennel-mode :fetcher sourcehut :repo "technomancy/fennel-mode" :ref "da958db"))
;; ***** figlet
;; :PROPERTIES:
;; :ID:       0e2d4f83-1c93-475c-adb2-c1fb61f933da
;; :END:
(elpaca (figlet :fetcher github :repo "jpkotta/figlet" :ref "19a3878"))
;; ***** frame-cmds
;; :PROPERTIES:
;; :ID:       b40e3a30-57ac-4437-89ca-4bd543604342
;; :END:
(elpaca (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds" :ref "b803354"))
;; ***** frame-fns
;; :PROPERTIES:
;; :ID:       164a42ba-5ccf-4c57-87b7-0ce7ad4581db
;; :END:
(elpaca (frame-fns :fetcher github :repo "emacsmirror/frame-fns" :ref "b675ee5"))
;; ***** gcmh
;; :PROPERTIES:
;; :ID:       808f391b-5327-4e7c-ab1f-b58f6a06790d
;; :END:
(elpaca (gcmh :repo "koral/gcmh" :fetcher gitlab :ref "0089f9c"))
;; ***** git-auto-commit-mode
;; :PROPERTIES:
;; :ID:       2ace7d78-3a21-4b9f-8f7c-3a2f45d0513a
;; :END:
(elpaca (git-auto-commit-mode :fetcher github :repo "ryuslash/git-auto-commit-mode" :ref "a6b6e0f"))
;; ***** git-commit
;; :PROPERTIES:
;; :ID:       81ce0b06-6845-4df7-ac01-e392adee0086
;; :END:
(elpaca (git-commit :fetcher github :repo "magit/magit" :old-names (git-commit-mode) :ref "86eec7b"))
;; ***** git-gutter+
;; :PROPERTIES:
;; :ID:       fe85cbe2-83b0-4b7b-befe-b79f48f791eb
;; :END:
(elpaca (git-gutter+ :fetcher github :repo "nonsequitur/git-gutter-plus" :ref "b772699"))
;; ***** goto-chg
;; :PROPERTIES:
;; :ID:       41a6278a-eadc-49f6-95a4-4b5c9e582f1a
;; :END:
(elpaca (goto-chg :repo "emacs-evil/goto-chg" :fetcher github :ref "2af6121"))
;; ***** grugru
;; :PROPERTIES:
;; :ID:       06cb28e3-7609-40c7-9df7-56875b4b778f
;; :END:
;; =Grugru= is a package that lets me switch.
(elpaca (grugru :repo "ROCKTAKEY/grugru" :fetcher github :ref "92e588e"))
;; ***** helm
;; :PROPERTIES:
;; :ID:       e5dc51a1-d431-4409-aa2f-2db7f03b8174
;; :END:
(elpaca (helm :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))
;; ***** helm-core
;; :PROPERTIES:
;; :ID:       30fc8261-6d30-46b1-9511-7a5c144eaa04
;; :END:
(elpaca (helm-core :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))
;; ***** helm-system-packages
;; :PROPERTIES:
;; :ID:       9529e9f7-7434-4fdb-9741-a1b22221e1c3
;; :END:
(elpaca (helm-system-packages :repo "emacs-helm/helm-system-packages" :fetcher github :ref "e93f4ae"))
;; ***** helpful
;; :PROPERTIES:
;; :ID:       cf77b716-c8b5-4c12-85e9-1faa0afecf23
;; :END:
(elpaca (helpful :repo "Wilfred/helpful" :branch "master" :fetcher github :ref "6f8991a"))
;; ***** hide-mode-line
;; :PROPERTIES:
;; :ID:       89a4a5f3-3fba-4084-938f-72e62f620f37
;; :END:
(elpaca (hide-mode-line :repo "hlissner/emacs-hide-mode-line" :fetcher github :ref "8888882"))
;; ***** highlight-quoted
;; :PROPERTIES:
;; :ID:       9f8f3e54-34a6-473a-8de5-d235f2cb7fcc
;; :END:
(elpaca (highlight-quoted :fetcher github :repo "Fanael/highlight-quoted" :ref "2410347"))
;; ***** ht
;; :PROPERTIES:
;; :ID:       4c03ba12-852b-4a0b-aeeb-8bb6aa3f157b
;; :END:
(elpaca (ht :fetcher github :repo "Wilfred/ht.el" :ref "2850301"))
;; ***** hydra
;; :PROPERTIES:
;; :ID:       c747090a-410b-4764-bf0c-aaa15e5c4dc4
;; :END:
(elpaca (hydra :repo "abo-abo/hydra" :fetcher github :ref "2d55378"))
;; ***** ialign
;; :PROPERTIES:
;; :ID:       1c80eccf-1021-4f97-bf12-1aa7b3240398
;; :END:
(elpaca (ialign :fetcher github :repo "mkcms/interactive-align" :ref "bc4d30d"))
;; ***** idle-require
;; :PROPERTIES:
;; :ID:       03a83af0-3579-4704-9615-066b6a4c5493
;; :END:
(elpaca (idle-require :fetcher github :repo "nschum/idle-require.el" :ref "33592bb"))
;; ***** iedit
;; :PROPERTIES:
;; :ID:       41899a80-614d-4c1f-bc19-472458d2ddfe
;; :END:
(elpaca (iedit :repo "victorhge/iedit" :fetcher github :ref "27c6186"))
;; ***** ivy
;; :PROPERTIES:
;; :ID:       7a7be1cb-90fa-4fb0-88fd-6d6f25f30399
;; :END:
(elpaca (ivy :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))
;; ***** key-chord
;; :PROPERTIES:
;; :ID:       d3065ac7-bd54-4044-8f97-0c56b66800b1
;; :END:
(elpaca (key-chord :fetcher github :repo "emacsorphanage/key-chord" :ref "7f7fd7c"))
;; ***** lispy
;; :PROPERTIES:
;; :ID:       8cba9c2c-3344-4d22-ab1e-d47770937d21
;; :END:
(elpaca (lispy :repo "abo-abo/lispy" :fetcher github :ref "1ad128b"))
;; ***** lispyville
;; :PROPERTIES:
;; :ID:       fb4a8054-4cf8-48a8-af1c-b605e48665f8
;; :END:
(elpaca (lispyville :fetcher github :repo "noctuid/lispyville" :ref "0f13f26"))
;; ***** loopy
;; :PROPERTIES:
;; :ID:       4af29d9a-3c30-42ae-8019-64babca099fb
;; :END:
(elpaca (loopy :fetcher github :repo "okamsn/loopy" :ref "31dc58f"))
;; ***** lv
;; :PROPERTIES:
;; :ID:       d50412a8-69a4-4e50-b647-19888f9a7a31
;; :END:
(elpaca (lv :repo "abo-abo/hydra" :fetcher github :ref "2d55378"))
;; ***** macrostep
;; :PROPERTIES:
;; :ID:       2d2c8fb0-4af9-471e-88fa-dc5aa8b01e2d
;; :END:
(elpaca (macrostep :fetcher github :repo "joddie/macrostep" :ref "424e373"))
;; ***** magit
;; :PROPERTIES:
;; :ID:       ecd3979a-7402-4129-afc6-59cb1b529aa1
;; :END:
(elpaca (magit :fetcher github :repo "magit/magit" :ref "86eec7b"))
;; ***** magit-section
;; :PROPERTIES:
;; :ID:       58a04d11-1410-4149-a0df-1507f54c2105
;; :END:
(elpaca (magit-section :fetcher github :repo "magit/magit" :ref "86eec7b"))
;; ***** map
;; :PROPERTIES:
;; :ID:       45552f80-1214-494e-99fe-ed2d578daa1e
;; :END:
(elpaca (map :host github :repo "emacs-straight/map" :fetcher github :ref "dc4f657"))
;; ***** marginalia
;; :PROPERTIES:
;; :ID:       532c6caf-e9ee-4152-a097-38fd402ce700
;; :END:
(elpaca (marginalia :repo "minad/marginalia" :fetcher github :ref "b65d66e"))
;; ***** markdown-mode
;; :PROPERTIES:
;; :ID:       12cf26e7-9582-424f-ad3d-df423f24cf7c
;; :END:
(elpaca (markdown-mode :fetcher github :repo "jrblevin/markdown-mode" :ref "c338cdf"))
;; ***** mini-modeline
;; :PROPERTIES:
;; :ID:       4a77ead9-fa68-4bad-8c98-20399a10f8ef
;; :END:
(elpaca (mini-modeline :repo "kiennq/emacs-mini-modeline" :fetcher github :ref "7dcd0ab"))
;; ***** mmt
;; :PROPERTIES:
;; :ID:       07d788d1-2916-4c12-a3ea-17f06fb9005f
;; :END:
(elpaca (mmt :repo "mrkkrp/mmt" :fetcher github :ref "d772956"))
;; ***** modus-themes
;; :PROPERTIES:
;; :ID:       6824ab97-0509-4058-bea1-b4e56cf15610
;; :END:
(elpaca (modus-themes :fetcher github :repo "protesilaos/modus-themes" :ref "38236a9"))
;; ***** notmuch
;; :PROPERTIES:
;; :ID:       46d1a3cb-ff70-4aa8-94fe-42f98c43d500
;; :END:
(elpaca (notmuch :url "https://git.notmuchmail.org/git/notmuch" :fetcher git :ref "a5f7efd"))
;; ***** orderless
;; :PROPERTIES:
;; :ID:       97e9489e-1c05-4ac3-b779-6c471ca22995
;; :END:
(elpaca (orderless :repo "oantolin/orderless" :fetcher github :ref "cbc0109"))
;; ***** org-auto-tangle
;; :PROPERTIES:
;; :ID:       188ee47f-b95b-452b-8adc-45f40b7744aa
;; :END:
(elpaca (org-auto-tangle :repo "yilkalargaw/org-auto-tangle" :fetcher github :ref "2494a6f"))
;; ***** org-ml
;; :PROPERTIES:
;; :ID:       fede5d13-2974-49f2-b2aa-8baf76e98f55
;; :END:
(elpaca (org-ml :repo "ndwarshuis/org-ml" :fetcher github :ref "385e3be"))
;; ***** org-ql
;; :PROPERTIES:
;; :ID:       b0c5ff1d-899e-4ef7-ba9f-5b7410ba71a8
;; :END:
(elpaca (org-ql :fetcher github :repo "alphapapa/org-ql" :ref "d7ada53"))
;; ***** org-remark
;; :PROPERTIES:
;; :ID:       d9fb63a1-0f33-48f0-ae30-73bd60a13a5c
;; :END:
(elpaca (org-remark :host github :repo "emacs-straight/org-remark" :ref "7e72e86"))
;; ***** org-super-agenda
;; :PROPERTIES:
;; :ID:       f09fcbd5-55eb-4c0c-b2aa-4d4ac6d93d51
;; :END:
(elpaca (org-super-agenda :fetcher github :repo "alphapapa/org-super-agenda" :ref "f5e80e4"))
;; ***** org-superstar
;; :PROPERTIES:
;; :ID:       fcb62b32-b4f0-4d62-9af0-dee43fcd1247
;; :END:
(elpaca (org-superstar :fetcher github :repo "integral-dw/org-superstar-mode" :ref "7f83636"))
;; ***** ov
;; :PROPERTIES:
;; :ID:       094ce65c-04e1-4102-827c-74d33be248bc
;; :END:
(elpaca (ov :fetcher github :repo "emacsorphanage/ov" :ref "c5b9aa4"))
;; ***** paredit
;; :PROPERTIES:
;; :ID:       c10b3c7c-3907-4038-81f0-6a44f7232de6
;; :END:
(elpaca (paredit :fetcher nil :url "https://mumble.net/~campbell/git/paredit.git" :repo "https://mumble.net/~campbell/git/paredit.git" :ref "d0b1a2f"))
;; ***** pass
;; :PROPERTIES:
;; :ID:       80f8581b-3304-4fb2-b25f-cee221c910c1
;; :END:
(elpaca (pass :fetcher github :repo "NicolasPetton/pass" :ref "a095d24"))
;; ***** password-generator
;; :PROPERTIES:
;; :ID:       8604ab40-4f33-459d-9562-d368cd4ebf37
;; :END:
(elpaca (password-generator :fetcher github :repo "vandrlexay/emacs-password-genarator" :ref "c1da979"))
;; ***** password-store
;; :PROPERTIES:
;; :ID:       3a5a17d7-ffec-4d02-b070-13ccfc844f9d
;; :END:
(elpaca (password-store :fetcher github :repo "zx2c4/password-store" :ref "f152064"))
;; ***** password-store-otp
;; :PROPERTIES:
;; :ID:       69118d1a-98cc-4d7e-96f4-5fceb4cd173e
;; :END:
(elpaca (password-store-otp :repo "volrath/password-store-otp.el" :fetcher github :ref "04998c8"))
;; ***** pinentry
;; :PROPERTIES:
;; :ID:       c9b343de-958a-42fc-b373-5a8949f00e7e
;; :END:
(elpaca (pinentry :host github :repo "emacs-straight/pinentry" :fetcher github :ref "cd942f7"))
;; ***** plural
;; :PROPERTIES:
;; :ID:       18601a31-5993-4b8a-86a4-0e62cb784d03
;; :END:
(elpaca (plural :fetcher github :repo "emacsmirror/plural" :ref "b91ce15"))
;; ***** popup
;; :PROPERTIES:
;; :ID:       8ae817d1-294b-468a-934c-a9c3e2e5787d
;; :END:
(elpaca (popup :fetcher github :repo "auto-complete/popup-el" :ref "bd5a0df"))
;; ***** popwin
;; :PROPERTIES:
;; :ID:       4c517e3a-044d-4b4c-9be2-42c4dcece1df
;; :END:
(elpaca (popwin :fetcher github :repo "emacsorphanage/popwin" :ref "215d6cb"))
;; ***** rainbow-delimiters
;; :PROPERTIES:
;; :ID:       2aa02b88-7cde-42ad-be18-ab92b0130662
;; :END:
(elpaca (rainbow-delimiters :fetcher github :repo "Fanael/rainbow-delimiters" :ref "f43d48a"))
;; ***** redacted
;; :PROPERTIES:
;; :ID:       76a8bd4d-6ecb-4b5c-8330-4cb6fb188a69
;; :END:
(elpaca (redacted :fetcher github :repo "bkaestner/redacted.el" :ref "156311e"))
;; ***** restart-emacs
;; :PROPERTIES:
;; :ID:       3faf6144-9114-4771-9ebc-b078d91886bf
;; :END:
(elpaca (restart-emacs :fetcher github :repo "iqbalansari/restart-emacs" :ref "1607da2"))
;; ***** s
;; :PROPERTIES:
;; :ID:       b739064e-831e-47a7-bad9-81f634574593
;; :END:
(elpaca (s :fetcher github :repo "magnars/s.el" :ref "43ba8b5"))
;; ***** search-web
;; :PROPERTIES:
;; :ID:       cd58b8df-f1b8-4d32-89f8-c6ad274fdf1a
;; :END:
(elpaca (search-web :repo "tomoya/search-web.el" :fetcher github :ref "c4ae86a"))
;; ***** shut-up
;; :PROPERTIES:
;; :ID:       f0c06026-f834-44af-8381-e4e2d4c549eb
;; :END:
(elpaca (shut-up :fetcher github :repo "cask/shut-up" :ref "081d6b0"))
;; ***** smartparens
;; :PROPERTIES:
;; :ID:       57b50aaf-505b-4403-a2f3-8444511032b5
;; :END:
(elpaca (smartparens :fetcher github :repo "Fuco1/smartparens" :ref "63695c6"))
;; ***** spell-number
;; :PROPERTIES:
;; :ID:       826cb3b0-b820-4dfb-b8f5-65f3f2070ef4
;; :END:
(elpaca (spell-number :fetcher github :repo "emacsmirror/spell-number" :ref "3ce612d"))
;; ***** super-save
;; :PROPERTIES:
;; :ID:       874aa539-841f-454c-b9c1-21f87af50a15
;; :END:
(elpaca (super-save :fetcher github :repo "bbatsov/super-save" :ref "886b551"))
;; ***** swiper
;; :PROPERTIES:
;; :ID:       eae8fb23-7d9a-4607-9dd1-ecf0ebaa9bd4
;; :END:
(elpaca (swiper :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))
;; ***** swiper-helm
;; :PROPERTIES:
;; :ID:       410651e8-8188-4677-88d9-616a83b74a36
;; :END:
(elpaca (swiper-helm :repo "abo-abo/swiper-helm" :fetcher github :ref "93fb6db"))
;; ***** tempel
;; :PROPERTIES:
;; :ID:       756039d8-a42c-42a2-a5a1-31d7ebd34981
;; :END:
(elpaca (tempel :repo "minad/tempel" :fetcher github :ref "b4bb703"))
;; ***** transient
;; :PROPERTIES:
;; :ID:       e45d3f0f-6f8e-4069-bfb4-81c0ce6b3db1
;; :END:
(elpaca (transient :fetcher github :repo "magit/transient" :ref "90e640f"))
;; ***** transpose-frame
;; :PROPERTIES:
;; :ID:       7de3871f-281c-4b19-a441-610406a4ff2f
;; :END:
(elpaca (transpose-frame :fetcher github :repo "emacsorphanage/transpose-frame" :ref "12e523d"))
;; ***** treepy
;; :PROPERTIES:
;; :ID:       54567606-8d68-4094-91d0-d87c907c9492
;; :END:
(elpaca (treepy :repo "Luis-Henriquez-Perez/treepy.el" :fetcher github :ref "191d84c"))
;; ***** ts
;; :PROPERTIES:
;; :ID:       8d7e178b-3f62-44a9-9f87-1437e05b7056
;; :END:
(elpaca (ts :fetcher github :repo "alphapapa/ts.el" :ref "b7ca357"))
;; ***** undo-tree
;; :PROPERTIES:
;; :ID:       89eebee7-4ab1-4bea-bf3d-86c4f6505f9d
;; :END:
(elpaca (undo-tree :host github :repo "emacs-straight/undo-tree" :fetcher github :ref "e326c61"))
;; ***** vc-auto-commit
;; :PROPERTIES:
;; :ID:       acb8e72e-7c5d-44a0-b0ab-5b44e1614f8e
;; :END:
(elpaca (vc-auto-commit :fetcher github :repo "thisirs/vc-auto-commit" :ref "56f4780"))
;; ***** vertico
;; :PROPERTIES:
;; :ID:       a7038ffb-6fee-44a0-a444-b2814a139859
;; :END:
(elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref "956c81b"
                 :files (:defaults "extensions/vertico-buffer.el" "extensions/vertico-quick.el" "extensions/vertico-directory.el")))
;; ***** which-key
;; :PROPERTIES:
;; :ID:       e1324ac4-9fed-4170-b74b-ed851c5ce652
;; :END:
(elpaca (which-key :repo "justbur/emacs-which-key" :fetcher github :ref "428aedf"))
;; ***** with-editor
;; :PROPERTIES:
;; :ID:       7c9c0da9-2067-47ab-9bc0-c26781e42ade
;; :END:
(elpaca (with-editor :fetcher github :repo "magit/with-editor" :ref "139ef39"))
;; ***** with-emacs
;; :PROPERTIES:
;; :ID:       545d26ae-c678-4287-891c-430e9fc3c652
;; :END:
(elpaca (with-emacs :fetcher github :repo "twlz0ne/with-emacs.el" :ref "9f99bec"))
;; ***** workgroups2
;; :PROPERTIES:
;; :ID:       edd5f503-8382-401b-90e8-333508cc1c50
;; :END:
(elpaca (workgroups2 :repo "pashinin/workgroups2" :fetcher github :ref "c9403c6"))
;; ***** xelb
;; :PROPERTIES:
;; :ID:       8fc4cc36-7a32-4447-82ee-6f2fa7c392f4
;; :END:
(elpaca (xelb :host github :repo "emacs-straight/xelb" :fetcher github :ref "f5880e6"))
;; ***** xr
;; :PROPERTIES:
;; :ID:       a3176d37-294b-4438-8748-5c9d76270f57
;; :END:
(elpaca (xr :host github :repo "emacs-straight/xr" :fetcher github :ref "277c549"))
;; ***** ssh-agency
;; :PROPERTIES:
;; :ID:       20231005T152039.806912
;; :END:
;; This package automates the process of starting up =ssh-agent= and connecting to
;; it "so that pushes and pulls from magit will not require any passphrase".  This
;; is exactly what I had been looking for so I could [[][auto push and commit]].
(elpaca (ssh-agency :fetcher github :repo "magit/ssh-agency" :ref "a5377e4"))
;; ***** zone-matrix
;; :PROPERTIES:
;; :ID:       1e1b8d60-dc2d-43fb-8116-8873b9fa83fe
;; :END:
(elpaca (zone-matrix :fetcher github :repo "emacsmirror/zone-matrix" :ref "e1fc8c7"))
;; ***** zone-rainbow
;; :PROPERTIES:
;; :ID:       17efd0b5-8398-472f-bb14-48c6d033f563
;; :END:
(elpaca (zone-rainbow :repo "kawabata/zone-rainbow" :fetcher github :ref "2ba4f1a"))
;; ***** zone-sl
;; :PROPERTIES:
;; :ID:       68d37858-81c2-4ab6-9b9d-2e39dfa32467
;; :END:
(elpaca (zone-sl :repo "kawabata/zone-sl" :fetcher github :ref "7ec22e3"))
;; ***** zoom-frm
;; :PROPERTIES:
;; :ID:       5de43a82-6590-4b4c-b56a-b3e3ae42aab2
;; :END:
(elpaca (zoom-frm :fetcher github :repo "emacsmirror/zoom-frm" :ref "59e2fce" ))
;; ***** zoom-window
;; :PROPERTIES:
;; :ID:       17315221-2ef9-4a59-8cac-b88f4915ea6a
;; :END:
(elpaca (zoom-window :fetcher github :repo "emacsorphanage/zoom-window" :ref "474ca47"))
;; ***** lgr
;; This package provides functions and macro to log information.  It's the most
;; recent package for logging I could find.
(elpaca (lgr :fetcher github :repo "Fuco1/Emacs-lgr" :commit "4ab6c22"))
;; ***** setup
;; :PROPERTIES:
;; :ID:       20230920T190421.807596
;; :END:
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
(elpaca (setup :repo "https://git.sr.ht/~pkal/setup" :ref "b2e3f3a"))
