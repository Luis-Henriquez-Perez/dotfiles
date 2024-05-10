;;; oo-init-elpaca.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is where I use elpaca to install all of my packages.
;;
;;; Code:
;;; requirements
(eval-and-compile (require 'oo-elpaca-bootstrap))
;;; recipes
;; This adds extra operators for evaluating.
;; Eldev complains about `buttercup' already being loaded.  So I have to think
;; about how I will go about installing it for normal usage.  In any case the
;; only reason I want it is for proper indenting and syntax highlighting when I
;; am actually editing my test files.
;; TODO: add bindings for lorem ipsum.
;; (elpaca aas)
;; (elpaca (once :host github :repo "emacs-magus/once"))
;; This is temporary--ideally I would want.
(elpaca yeetube)
(elpaca beacon)
(elpaca edwina)
(elpaca htmlize)
(elpaca edit-indirect)
(elpaca annalist)
(elpaca evil-nerd-commenter)
(elpaca org-tidy)
(elpaca polymode)
(elpaca emmet-mode)
(elpaca org-ml)
(elpaca sly)
(elpaca restart-emacs)
(elpaca cider)
(elpaca orglink)
;; (elpaca edwina)
(elpaca pcre2el)
(elpaca eshell-prompt-extras)
(elpaca ace-jump-helm-line)
(elpaca ace-link)
(elpaca aggressive-indent)
;; (elpaca ah)
(elpaca anaphora)
;; (elpaca benchmark-init)
(elpaca burly)
;; (elpaca buttercup)
(elpaca cyberpunk-theme)
(elpaca minimal-theme)
(elpaca cape)
(elpaca caps-lock)
(elpaca (captain :repo "git://git.sv.gnu.org/emacs/elpa" :branch "externals/captain" :ref "364ee98"))
(elpaca chezmoi)
(elpaca consult)
(elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref ...))
(elpaca dashboard)
(elpaca dirvish)
(elpaca doom-themes)
(elpaca dumb-jump)
(elpaca eat)
(elpaca ef-themes)
(elpaca elfeed)
(elpaca embark)
(elpaca embark-consult)
(elpaca emms)
;; (elpaca erefactor)
(elpaca eros)
(elpaca eshell-up)
(elpaca eshell-z)
(elpaca esup)
(elpaca evil)
(elpaca evil-cleverparens)
(elpaca (evil-easymotion :fetcher github :repo "Luis-Henriquez-Perez/evil-easymotion"))
(elpaca evil-exchange)
(elpaca evil-extra-operator)
(elpaca evil-goggles)
(elpaca (evil-magit :fetcher github :repo "emacs-evil/evil-magit" :ref "f4a8c8d"))
(elpaca evil-surround)
(elpaca evil-textobj-line)
(elpaca evil-textobj-syntax)
(elpaca expand-region)
(elpaca fennel-mode)
(elpaca filladapt)
(elpaca fit-text-scale)
(elpaca gcmh)
(elpaca git-gutter)
(elpaca github-theme)
(elpaca goto-chg)
(elpaca grugru)
(elpaca helm)
(elpaca helm-system-packages)
(elpaca helpful)
(elpaca highlight-quoted)
(elpaca humanoid-themes)
(elpaca idle-require)
(elpaca leuven-theme)
(elpaca lgr)
(elpaca lispy)
(elpaca lispyville)
(elpaca lorem-ipsum)
(elpaca macrostep)
(elpaca magit)
(elpaca marginalia)
(elpaca markdown-mode)
(elpaca material-theme)
(elpaca meow)
(elpaca modus-themes)
(elpaca monokai-theme)
(elpaca no-littering)
(elpaca notmuch)
(elpaca (on :host github :repo "ajgrf/on.el"))
(elpaca orderless)
(elpaca org-appear)
(elpaca org-bookmark-heading)
(elpaca org-superstar)
(elpaca (outli :fetcher github :repo "jdtsmith/outli"))
(elpaca rainbow-delimiters)
(elpaca redacted)
(elpaca s)
(elpaca shut-up)
(elpaca smartparens)
(elpaca spacemacs-theme)
(elpaca ssh-agency)
(elpaca standard-themes)
(elpaca super-save)
(elpaca tempel)
(elpaca tempel-collection)
(elpaca transpose-frame)
(elpaca (treepy :repo "Luis-Henriquez-Perez/treepy.el" :fetcher github :ref "191d84c"))
(elpaca vc-auto-commit)
(elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref ...))
(elpaca which-key)
(elpaca ws-butler)
(elpaca xr)
;;; process
(elpaca-process-queues)
(elpaca-wait)
;;; provide
(provide 'oo-init-elpaca)
;;; oo-init-elpaca.el ends here
