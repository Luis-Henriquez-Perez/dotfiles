;;; oo-elpaca-lib.el --- elpaca setup -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/Luis-Henriquez-Perez/dotfiles
;;
;; License: GPLv3
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; requirements
(require 'oo-base-lib)
;;; bootstrap
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "packages/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order `(elpaca
                          :repo "https://github.com/progfolio/elpaca.git"
                          :ref "9478158"
                          :files (:defaults (:exclude "extensions"))
                          :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (plist-get order :ref))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(require 'elpaca)
;;; do not process too many orders at once
;; Doing so causes me performance problems.
(setq elpaca-queue-limit 5)
;; Wait longer.
(setq elpaca-wait-interval 0.5)
;;; recipes
;;;; elpaca
;;;; outli
(elpaca (outli :fetcher github :repo "jdtsmith/outli"))
;;;; edwina
;; There are [[several]] emacs packages that save and restore window configuration. But
;; before being able to save and restore window configurations you need to create a
;; window configuration and that process can be repetitive and tedious. This
;; package actually automates the building of a window configuration from one
;; window.  This package provides tools to configure =display-buffer= so that when
;; called I can open a buffer in a [[file:screenshots/edwina-usage.png][master-slave layout]] (see [[file:screenshots/edwina-usage.png][edwina's usage]]).
(elpaca (edwina :fetcher gitlab :repo "ajgrf/edwina" :ref "f95c31b" :branch "master"))
;;;; loop
(elpaca (loop :fetcher github :repo "https://github.com/Wilfred/loop.el"))
;;;; htmlize
;; This package converts emacs buffers to html.  I use it in combination with the
;; command-line utility [[][wkhtmltoimage]] to take [[id:20230802T153110.062543][snapshots]] of emacs buffers.
;; There is a similar built-in emacs package [[][html-fontify]] that I have tried
;; to use, but I encountered [[][an error]].
(elpaca (htmlize :fetcher github :repo "hniksic/emacs-htmlize" :ref "dd27bc3" :branch "master"))
;;;; org-appear
;; This package allows for the editing of org emphasis markers and links in a
;; "do-what-I-mean" type way.  Typically when I edit an org link or markup I have
;; to manually toggle [[][]] and then edit it. That's because I tell org mode to
;; display links and emphasis markers in a "prettified" way.  Although it's more
;; pleasant to the eye to see emphasis markers this way; it is harder to edit
;; them.  So what will end up happening is I toggle.  What this package does is
;; make it so if your cursor is on the emphasis marker or link, it's visibility
;; automatically toggled.
(elpaca (org-appear :fetcher github :repo "awth13/org-appear" :ref "eb9f9db" :branch "master"))
;;;; outshine
;; Outshine is an Emacs package that allows for the use of org mode in non org

;; features.  It promises a kind of "have your cake and eat it too" type thing
;; where you can utilize the features of org mode--which include markup support,
;; link support, headline refiling, etc--while not paying the price of needing
;; an initial tangling script.
;; (elpaca (outshine :fetcher github :repo "alphapapa/outshine" :ref "bf1eed1"))
;;;; outorg
;; Outshine depends on the package Outorg.  Outorg is what outshine uses to
;; convert back and forth between a file with outshine syntax and an org file.
;; (elpaca (outorg :fetcher github :repo "alphapapa/outorg" :ref "ef0f86f"))
;;;; navi-mode
;; This package is a dependency of `outorg'.  It has to do with providing
;; navigation commands for `outshine'.
;; (elpaca (navi-mode :fetcher github :repo "alphapapa/navi" :ref "cf97e1e"))
;;;; orglink
;; This package lets me display org links in non org buffer.  In practice I use
;; this for buffers with outshine enabled.
;; (elpaca (orglink :fetcher github :repo "tarsius/orglink" :ref "afbeffd"))
;;;; chezmoi
;; This package provides some functions to interface with =chezmoi=, a dotfile
;; manager.  At first--and even still to be honest I wanted to use Org as a
;; dotfile manager; but org although having some useful dotfile management tools
;; (such as tangling) is not designed to be a dotfile manager.  Whereas,
;; =chezmoi= was actually designed to solve many dotfile problems.  In the
;; future, I'd like to scrap =chezmoi= and use a dotfile manager written in
;; Elisp; preferably with the optional of optionally using org mode tools for
;; dotfile management.
(elpaca (chezmoi :fetcher github :repo "tuh8888/chezmoi.el" :ref "1389782"))
;;;; lua-mode
;; Emacs doesn't have a mode for lua built-in--not to say that's a bad
;; thing.  This package provides such a mode.
(elpaca (lua-mode :fetcher github :repo "immerrr/lua-mode" :ref "7eb8eaa"))
;;;; org-bookmark-heading
;; This package provides bookmark functions catered specifically for org files.

;; =point= as well as surrounding text that can be searched for in text files.
;; Obviously, this is not perfect but because a bookmark may easily become
;; Broken when you edit the region.  With org you can do better because each
;; Headline has a unique ID.  This package provides the function for storing the
;; information necessary and it provides the function to jump to that ID.
(elpaca (org-bookmark-heading :fetcher github :repo "alphapapa/org-bookmark-heading" :ref "4e97fab"))
;;;; cape
;; This package provides several =capfs= and utility functions for working with
;; =capfs=.  By the way =capfs= stands for a [[][completion at point function]].
;; I am particular interested in a function it provides to merge =capfs=.
(elpaca (cape :repo "minad/cape" :branch "main" :fetcher github :ref "4645762"))
;;;; filladapt
(elpaca (filladapt :repo "git://git.sv.gnu.org/emacs/elpa" :branch "externals/filladapt" :ref "802c194"))
;;;; burly
;; Burly provides functions that can save a window configuration via bookmarks.
;; There are a plethora of packages out there provide ways to manage window
;; configurations; to name a few: [[about:blank][eyebrowse]], [[https://github.com/knu/elscreen][persp-mode]], [[https://github.com/knu/elscreen][elscreen]], [[https://elpa.gnu.org/packages/wconf.html][wconf]].  What
;; makes Burly different is that instead of making its own abstraction for
;; window configuration saving, it uses bookmarks which already have existing
;; support in Emacs.
(elpaca (burly :fetcher github :repo "alphapapa/burly.el" :branch "master" :ref "f570fa8"))
;;;; dogears
(elpaca (dogears :fetcher github :repo "alphapapa/dogears.el" :files (:defaults (:exclude "helm-dogears.el"))))
;;;; captain
;; This package provides me with auto-capitalization.  It checks each word as I
;; type and after I finish typing that word it checks whether or not it should
;; be capitalized.
(elpaca (captain :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo "captain" :branch "externals/captain" :ref "364ee98"))
;;;; lambda-themes
;; These are some beautiful and elegant themes.  They are on the minimal side, but I
;; like that very much.
(elpaca (lambda-themes :repo "Lambda-Emacs/lambda-themes" :branch "main" :fetcher github :ref "7342250"))
;;;; consult
(elpaca (consult :repo "minad/consult" :fetcher github :branch "main" :ref "fae9b50"))
;;;; textsize
(elpaca (textsize :repo "WJCFerguson/textsize" :fetcher github :branch "master" :ref "df91392"))
;;;; on.el
;; This package provides hooks inspired by [[][Doom Emacs]].  Generally, the main
;; idea behind the hooks these package offers is to provide ways to improve
;; performance usually by spreading out the loading of features.  The hooks it
;; provides are [[][]].
(elpaca (on :host github :repo "ajgrf/on.el" :branch "master" :ref "3cf623e"))
;;;; ligature
(elpaca (ligature :fetcher github :repo "mickeynp/ligature.el" :ref "3d14604"))
;;;; org
(elpaca (org :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :ref "f731d45"))
;;;; refine
(elpaca (refine :repo "Wilfred/refine" :fetcher github :ref "d72fa50"))
;;;; ace-window
(elpaca (ace-window :repo "abo-abo/ace-window" :fetcher github :ref "c7cb315"))
;;;; aggressive-fill-paragraph
(elpaca (aggressive-fill-paragraph :fetcher github :repo "davidshepherd7/aggressive-fill-paragraph-mode" :ref "4a620e6"))
;;;; aggressive-indent
(elpaca (aggressive-indent :repo "Malabarba/aggressive-indent-mode" :fetcher github :ref "b0ec004"))
;;;; all-the-icons
(elpaca (all-the-icons :repo "domtronn/all-the-icons.el" :fetcher github :ref "be99987"))
;;;; all-the-icons-completion
(elpaca (all-the-icons-completion :repo "iyefrat/all-the-icons-completion" :fetcher github :ref "286e2c0"))
;;;; anaphora
;; This package provides me with common [[https://en.wikipedia.org/wiki/Anaphoric_macro][anaphoric macros]].
(elpaca (anaphora :repo "rolandwalker/anaphora" :fetcher github :ref "3b2da3f"))
;;;; async
(elpaca (async :repo "jwiegley/emacs-async" :fetcher github :ref "9a8cd0c"))
;;;; auth-source-pass
(elpaca (auth-source-pass :fetcher github :repo "DamienCassou/auth-source-pass" :ref "aa7f171"))
;;;; auto-capitalize
(elpaca (auto-capitalize :fetcher github :repo "emacsmirror/auto-capitalize" :ref "0ee14c7"))
;;;; avy
(elpaca (avy :repo "abo-abo/avy" :fetcher github :ref "e92cb37"))
;;;; buffer-expose
(elpaca (buffer-expose :host github :repo "clemera/buffer-expose" :fetcher github :ref "c4a1c74"))
;;;; centered-cursor-mode
(elpaca (centered-cursor-mode :fetcher github :repo "andre-r/centered-cursor-mode.el" :ref "ebaeb80"))
;;;; centered-window
(elpaca (centered-window :fetcher github :repo "anler/centered-window-mode" :old-names (centered-window-mode) :ref "80965f6"))
;;;; corfu
(elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref "a59c41d"
               :files (:defaults "extensions/corfu-history.el" "extensions/corfu-quick.el")))
;;;; counsel
(elpaca (counsel :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))
;;;; dash
(elpaca (dash :fetcher github :repo "magnars/dash.el" :ref "7a9c937"))
;;;; dash-functional
(elpaca (dash-functional :fetcher github :repo "magnars/dash.el" :ref "7a9c937"))
;;;; dashboard
(elpaca (dashboard :fetcher github :repo "emacs-dashboard/emacs-dashboard" :ref "36c8da4"))
;;;; decide
(elpaca (decide :fetcher github :repo "lifelike/decide-mode" :ref "668fa55"))
;;;; default-text-scale
(elpaca (default-text-scale :fetcher github :repo "purcell/default-text-scale" :ref "bfc0987"))
;;;; dirvish
(elpaca (dirvish :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;;;; dirvish-collapse
(elpaca (dirvish-collapse :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;;;; dirvish-icons
(elpaca (dirvish-icons :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;;;; dirvish-media
(elpaca (dirvish-media :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;;;; dirvish-subtree
(elpaca (dirvish-subtree :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))
;;;; doct
(elpaca (doct :repo "progfolio/doct" :fetcher github :ref "15974ad"))
;;;; edit-indirect
(elpaca (edit-indirect :fetcher github :repo "Fanael/edit-indirect" :ref "bdc8f54"))
;;;; ednc
(elpaca (ednc :repo "sinic/ednc" :fetcher github :ref "d1a3c37"))
;;;; elfeed
(elpaca (elfeed :repo "skeeto/elfeed" :fetcher github :ref "de4b64b"))
;;;; elfeed-org
(elpaca (elfeed-org :repo "remyhonig/elfeed-org" :fetcher github :ref "77b6bbf"))
;;;; elfeed-score
(elpaca (elfeed-score :fetcher github :repo "sp1ff/elfeed-score" :ref "5fff415"))
;;;; elisp-demos
(elpaca (elisp-demos :fetcher github :repo "xuchunyang/elisp-demos" :ref "ed9578d"))
;;;; elisp-refs
(elpaca (elisp-refs :repo "Wilfred/elisp-refs" :branch "master" :fetcher github :ref "bf3cca8"))
;;;; ellocate
(elpaca (ellocate :fetcher github :repo "walseb/ellocate" :ref "8140508"))
;;;; embark
(elpaca (embark :repo "oantolin/embark" :fetcher github :ref "5d0459d"))
;;;; emms
(elpaca (emms :fetcher github :url "https://git.savannah.gnu.org/git/emms.git" :repo "emacsmirror/emms" :ref "5c3226b"))
;;;; eros
(elpaca (eros :fetcher github :repo "xiongtx/eros" :ref "dd89102"))
;;;; eshell-up
(elpaca (eshell-up :fetcher github :repo "peterwvj/eshell-up" :ref "9c100ba"))
;;;; eshell-z
(elpaca (eshell-z :fetcher github :repo "xuchunyang/eshell-z" :ref "337cb24"))
;;;; evil
(elpaca (evil :repo "emacs-evil/evil" :fetcher github :ref "cc9d688"))
;;;; evil-cleverparens
(elpaca (evil-cleverparens :fetcher github :repo "luxbock/evil-cleverparens" :ref "8c45879"))
;;;; evil-easymotion
(elpaca (evil-easymotion :repo "PythonNut/evil-easymotion" :fetcher github :ref "f96c2ed"))
;;;; evil-goggles
(elpaca (evil-goggles :repo "edkolev/evil-goggles" :fetcher github :ref "08a2205"))
;;;; evil-magit
(elpaca (evil-magit :fetcher github :repo "emacs-evil/evil-magit" :ref "f4a8c8d"))
;;;; evil-surround
(elpaca (evil-surround :repo "emacs-evil/evil-surround" :fetcher github :old-names (surround) :ref "346d4d8"))
;;;; expand-region
(elpaca (expand-region :repo "magnars/expand-region.el" :fetcher github :ref "ea6b4cb"))
;;;; exwm
(elpaca (exwm :branch "master" :host github :repo "ch11ng/exwm" :fetcher github :ref "b62d5e7"))
;;;; exwm-edit
(elpaca (exwm-edit :repo "agzam/exwm-edit" :fetcher github :ref "2fd9426"))
;;;; exwm-firefox-core
(elpaca (exwm-firefox-core :fetcher github :repo "walseb/exwm-firefox-core" :ref "e2fe2a8"))
;;;; exwm-firefox-evil
(elpaca (exwm-firefox-evil :fetcher github :repo "walseb/exwm-firefox-evil" :ref "14643ee"))
;;;; exwm-float
(elpaca (exwm-float :fetcher gitlab :repo "mtekman/exwm-float.el" :ref "eb1b60b"))
;;;; f
(elpaca (f :fetcher github :repo "rejeep/f.el" :ref "1814209"))
;;;; fennel-mode
(elpaca (fennel-mode :fetcher sourcehut :repo "technomancy/fennel-mode" :ref "da958db"))
;;;; figlet
(elpaca (figlet :fetcher github :repo "jpkotta/figlet" :ref "19a3878"))
;;;; frame-cmds
(elpaca (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds" :ref "b803354"))
;;;; frame-fns
(elpaca (frame-fns :fetcher github :repo "emacsmirror/frame-fns" :ref "b675ee5"))
;;;; gcmh
(elpaca (gcmh :repo "koral/gcmh" :fetcher gitlab :ref "0089f9c"))
;;;; git-auto-commit-mode
(elpaca (git-auto-commit-mode :fetcher github :repo "ryuslash/git-auto-commit-mode" :ref "a6b6e0f"))
;;;; git-commit
(elpaca (git-commit :fetcher github :repo "magit/magit" :old-names (git-commit-mode) :ref "86eec7b"))
;;;; git-gutter+
(elpaca (git-gutter+ :fetcher github :repo "nonsequitur/git-gutter-plus" :ref "b772699"))
;;;; goto-chg
(elpaca (goto-chg :repo "emacs-evil/goto-chg" :fetcher github :ref "2af6121"))
;;;; grugru
;; =Grugru= is a package that lets me switch.
(elpaca (grugru :repo "ROCKTAKEY/grugru" :fetcher github :ref "92e588e"))
;;;; helm
(elpaca (helm :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))
;;;; helm-core
(elpaca (helm-core :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))
;;;; helm-system-packages
(elpaca (helm-system-packages :repo "emacs-helm/helm-system-packages" :fetcher github :ref "e93f4ae"))
;;;; helpful
(elpaca (helpful :repo "Wilfred/helpful" :branch "master" :fetcher github :ref "6f8991a"))
;;;; hide-mode-line
(elpaca (hide-mode-line :repo "hlissner/emacs-hide-mode-line" :fetcher github :ref "8888882"))
;;;; highlight-quoted
(elpaca (ht :fetcher github :repo "Wilfred/ht.el" :ref "2850301"))
;;;; hydra
(elpaca (hydra :repo "abo-abo/hydra" :fetcher github :ref "2d55378"))
;;;; ialign
(elpaca (ialign :fetcher github :repo "mkcms/interactive-align" :ref "bc4d30d"))
;;;; idle-require
(elpaca (idle-require :fetcher github :repo "nschum/idle-require.el" :ref "33592bb"))
;;;; iedit
(elpaca (iedit :repo "victorhge/iedit" :fetcher github :ref "27c6186"))
;;;; ivy
(elpaca (ivy :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))
;;;; key-chord
(elpaca (key-chord :fetcher github :repo "emacsorphanage/key-chord" :ref "7f7fd7c"))
;;;; lispy
(elpaca (lispy :repo "abo-abo/lispy" :fetcher github :ref "1ad128b"))
;;;; lispyville
(elpaca (lispyville :fetcher github :repo "noctuid/lispyville" :ref "0f13f26"))
;;;; loopy
(elpaca (loopy :fetcher github :repo "okamsn/loopy" :ref "31dc58f"))
;;;; lv
(elpaca (lv :repo "abo-abo/hydra" :fetcher github :ref "2d55378"))
;;;; macrostep
(elpaca (macrostep :fetcher github :repo "joddie/macrostep" :ref "424e373"))
;;;; magit
(elpaca (magit :fetcher github :repo "magit/magit" :ref "86eec7b"))
;;;; magit-section
(elpaca (magit-section :fetcher github :repo "magit/magit" :ref "86eec7b"))
;;;; map
(elpaca (map :host github :repo "emacs-straight/map" :fetcher github :ref "dc4f657"))
;;;; marginalia
(elpaca (marginalia :repo "minad/marginalia" :fetcher github :ref "b65d66e"))
;;;; markdown-mode
(elpaca (markdown-mode :fetcher github :repo "jrblevin/markdown-mode" :ref "c338cdf"))
;;;; mini-modeline
(elpaca (mini-modeline :repo "kiennq/emacs-mini-modeline" :fetcher github :ref "7dcd0ab"))
;;;; mmt
(elpaca (mmt :repo "mrkkrp/mmt" :fetcher github :ref "d772956"))
;;;; modus-themes
(elpaca (modus-themes :fetcher github :repo "protesilaos/modus-themes" :ref "38236a9"))
;;;; notmuch
(elpaca (notmuch :url "https://git.notmuchmail.org/git/notmuch" :fetcher git :ref "a5f7efd"))
;;;; orderless
(elpaca (orderless :repo "oantolin/orderless" :fetcher github :ref "cbc0109"))
;;;; org-auto-tangle
(elpaca (org-auto-tangle :repo "yilkalargaw/org-auto-tangle" :fetcher github :ref "2494a6f"))
;;;; org-ml
(elpaca (org-ml :repo "ndwarshuis/org-ml" :fetcher github :ref "385e3be"))
;;;; org-ql
(elpaca (org-ql :fetcher github :repo "alphapapa/org-ql" :ref "d7ada53"))
;;;; org-remark
(elpaca (org-remark :host github :repo "emacs-straight/org-remark" :ref "7e72e86"))
;;;; org-super-agenda
(elpaca (org-super-agenda :fetcher github :repo "alphapapa/org-super-agenda" :ref "f5e80e4"))
;;;; org-superstar
(elpaca (org-superstar :fetcher github :repo "integral-dw/org-superstar-mode" :ref "7f83636"))
;;;; ov
(elpaca (ov :fetcher github :repo "emacsorphanage/ov" :ref "c5b9aa4"))
;;;; paredit
(elpaca (paredit :fetcher nil :url "https://mumble.net/~campbell/git/paredit.git" :repo "https://mumble.net/~campbell/git/paredit.git" :ref "d0b1a2f"))
;;;; pass
(elpaca (pass :fetcher github :repo "NicolasPetton/pass" :ref "a095d24"))
;;;; password-generator
(elpaca (password-generator :fetcher github :repo "vandrlexay/emacs-password-genarator" :ref "c1da979"))
;;;; password-store
(elpaca (password-store :fetcher github :repo "zx2c4/password-store" :ref "f152064"))
;;;; password-store-otp
(elpaca (password-store-otp :repo "volrath/password-store-otp.el" :fetcher github :ref "04998c8"))
;;;; pinentry
(elpaca (pinentry :host github :repo "emacs-straight/pinentry" :fetcher github :ref "cd942f7"))
;;;; plural
(elpaca (plural :fetcher github :repo "emacsmirror/plural" :ref "b91ce15"))
;;;; popup
(elpaca (popup :fetcher github :repo "auto-complete/popup-el" :ref "bd5a0df"))
;;;; popwin
(elpaca (popwin :fetcher github :repo "emacsorphanage/popwin" :ref "215d6cb"))
;;;; rainbow-delimiters
(elpaca (rainbow-delimiters :fetcher github :repo "Fanael/rainbow-delimiters" :ref "f43d48a"))
;;;; redacted
(elpaca (redacted :fetcher github :repo "bkaestner/redacted.el" :ref "156311e"))
;;;; restart-emacs
(elpaca (restart-emacs :fetcher github :repo "iqbalansari/restart-emacs" :ref "1607da2"))
;;;; s
(elpaca (s :fetcher github :repo "magnars/s.el" :ref "43ba8b5"))
;;;; search-web
(elpaca (search-web :repo "tomoya/search-web.el" :fetcher github :ref "c4ae86a"))
;;;; shut-up
(elpaca (shut-up :fetcher github :repo "cask/shut-up" :ref "081d6b0"))
;;;; smartparens
(elpaca (smartparens :fetcher github :repo "Fuco1/smartparens" :ref "63695c6"))
;;;; spell-number
(elpaca (spell-number :fetcher github :repo "emacsmirror/spell-number" :ref "3ce612d"))
;;;; super-save
(elpaca (super-save :fetcher github :repo "bbatsov/super-save" :ref "886b551"))
;;;; swiper
(elpaca (swiper :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))
;;;; swiper-helm
(elpaca (swiper-helm :repo "abo-abo/swiper-helm" :fetcher github :ref "93fb6db"))
;;;; tempel
(elpaca (tempel :repo "minad/tempel" :fetcher github :ref "b4bb703"))
;;;; transient
(elpaca (transient :fetcher github :repo "magit/transient" :ref "90e640f"))
;;;; transpose-frame
(elpaca (transpose-frame :fetcher github :repo "emacsorphanage/transpose-frame" :ref "12e523d"))
;;;; treepy
(elpaca (treepy :repo "Luis-Henriquez-Perez/treepy.el" :fetcher github :ref "191d84c"))
;;;; ts
(elpaca (ts :fetcher github :repo "alphapapa/ts.el" :ref "b7ca357"))
;;;; undo-tree
(elpaca (undo-tree :host github :repo "emacs-straight/undo-tree" :fetcher github :ref "e326c61"))
;;;; vc-auto-commit
(elpaca (vc-auto-commit :fetcher github :repo "thisirs/vc-auto-commit" :ref "56f4780"))
;;;; vertico
(elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref "956c81b"
                 :files (:defaults "extensions/vertico-buffer.el" "extensions/vertico-quick.el" "extensions/vertico-directory.el")))
;;;; which-key
(elpaca (which-key :repo "justbur/emacs-which-key" :fetcher github :ref "428aedf"))
;;;; with-editor
(elpaca (with-editor :fetcher github :repo "magit/with-editor" :ref "139ef39"))
;;;; with-emacs
(elpaca (with-emacs :fetcher github :repo "twlz0ne/with-emacs.el" :ref "9f99bec"))
;;;; workgroups2
(elpaca (workgroups2 :repo "pashinin/workgroups2" :fetcher github :ref "c9403c6"))
;;;; xelb
(elpaca (xelb :host github :repo "emacs-straight/xelb" :fetcher github :ref "f5880e6"))
;;;; xr
(elpaca (xr :host github :repo "emacs-straight/xr" :fetcher github :ref "277c549"))
;;;; ssh-agency
;; This package automates the process of starting up =ssh-agent= and connecting to
;; it "so that pushes and pulls from magit will not require any passphrase".  This
;; is exactly what I had been looking for so I could [[][auto push and commit]].
(elpaca (ssh-agency :fetcher github :repo "magit/ssh-agency" :ref "a5377e4"))
;;;; zone-matrix
(elpaca (zone-matrix :fetcher github :repo "emacsmirror/zone-matrix" :ref "e1fc8c7"))
;;;; zone-rainbow
(elpaca (zone-rainbow :repo "kawabata/zone-rainbow" :fetcher github :ref "2ba4f1a"))
;;;; zone-sl
(elpaca (zone-sl :repo "kawabata/zone-sl" :fetcher github :ref "7ec22e3"))
;;;; zoom-frm
(elpaca (zoom-frm :fetcher github :repo "emacsmirror/zoom-frm" :ref "59e2fce" ))
;;;; zoom-window
(elpaca (zoom-window :fetcher github :repo "emacsorphanage/zoom-window" :ref "474ca47"))
;;;; lgr
;; This package provides functions and macro to log information.  It's the most
;; recent package for logging I could find.
(elpaca (lgr :fetcher github :repo "Fuco1/Emacs-lgr" :commit "4ab6c22"))
;;;; compat
(elpaca (compat :fetcher github :repo "emacs-compat/compat"))
;;;; list-utils
(elpaca (list-utils :fetcher github :repo "rolandwalker/list-utils"))
;;;; zoutline
(elpaca (zoutline :fetcher github :repo "abo-abo/zoutline"))
;;;; setup
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

(elpaca (noflet :fetcher github :branch "master" :repo "nicferrier/emacs-noflet" :ref "7ae84dc3257637af7334101456dafe1759c6b68a"))
;;; 
;; Try to force elpaca to install the packages immediately because I have
;; already specified all of them.
(elpaca-process-queues)
(elpaca-wait)
