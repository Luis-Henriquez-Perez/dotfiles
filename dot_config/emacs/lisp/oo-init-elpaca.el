;;; oo-init-elpaca.el --- initialize elpaca -*- lexical-binding: t; -*-
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
;; This is temporary--ideally I would want.
;; (elpaca aas)
(elpaca ace-jump-helm-line)
(elpaca ace-link)
(elpaca aggressive-indent)
;; (elpaca ah)
(elpaca anaphora)
(elpaca annalist)
(elpaca beacon)
;; (elpaca benchmark-init)
(elpaca burly)
;; (elpaca buttercup)
(elpaca cape)
(elpaca caps-lock)
(elpaca (captain :repo "git://git.sv.gnu.org/emacs/elpa" :branch "externals/captain" :ref "364ee98"))
(elpaca chezmoi)
(elpaca cider)
(elpaca consult)
(elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github))
(elpaca cyberpunk-theme)
(elpaca dashboard)
(elpaca denote)
(elpaca dirvish)
(elpaca doom-themes)
(elpaca dumb-jump)
(elpaca eat)
(elpaca edit-indirect)
(elpaca edwina)
;; (elpaca edwina)
(elpaca ef-themes)
(elpaca elfeed)
(elpaca embark)
(elpaca embark-consult)
(elpaca emmet-mode)
(elpaca emms)
;; (elpaca erefactor)
(elpaca eros)
(elpaca eshell-prompt-extras)
(elpaca eshell-up)
(elpaca eshell-z)
(elpaca esup)
(elpaca evil)
(elpaca evil-cleverparens)
(elpaca (evil-easymotion :fetcher github :repo "Luis-Henriquez-Perez/evil-easymotion"))
(elpaca evil-exchange)
(elpaca evil-goggles)
(elpaca evil-iedit-state)
(elpaca evil-lisp-state)
(elpaca (evil-magit :fetcher github :repo "emacs-evil/evil-magit" :ref "f4a8c8d"))
(elpaca evil-nerd-commenter)
(elpaca evil-surround)
(elpaca evil-textobj-line)
(elpaca evil-textobj-syntax)
(elpaca expreg)
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
(elpaca htmlize)
(elpaca humanoid-themes)
(elpaca idle-require)
(elpaca leuven-theme)
(elpaca lgr)
(elpaca lispy)
(elpaca lispyville)
(elpaca loopy)
(elpaca lorem-ipsum)
(elpaca macrostep)
(elpaca (magit :depth nil :tag "v3.3.0"))
(elpaca marginalia)
(elpaca markdown-mode)
(elpaca material-theme)
(elpaca meow)
(elpaca minimal-theme)
(elpaca modus-themes)
(elpaca monokai-theme)
(elpaca no-littering)
(elpaca notmuch)
(elpaca (on :host github :repo "ajgrf/on.el"))
;; (elpaca (once :host github :repo "emacs-magus/once"))
(elpaca orderless)
(elpaca org-appear)
(elpaca org-bookmark-heading)
(elpaca org-ml)
(elpaca org-superstar)
(elpaca org-tidy)
(elpaca orglink)
(elpaca (outli :fetcher github :repo "jdtsmith/outli"))
(elpaca pcre2el)
(elpaca polymode)
(elpaca rainbow-delimiters)
(elpaca redacted)
(elpaca restart-emacs)
(elpaca s)
(elpaca shut-up)
(elpaca sly)
(elpaca smartparens)
(elpaca spacemacs-theme)
(elpaca ssh-agency)
(elpaca standard-themes)
(elpaca super-save)
(elpaca tempel)
(elpaca tempel-collection)
(elpaca textsize)
(elpaca transpose-frame)
(elpaca (treepy :repo "Luis-Henriquez-Perez/treepy.el" :fetcher github))
(elpaca vc-auto-commit)
(elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github))
(elpaca wgrep)
(elpaca which-key)
(elpaca ws-butler)
(elpaca xr)
(elpaca yeetube)
;;; process
(elpaca-process-queues)
(elpaca-wait)
;;; provide
(provide 'oo-init-elpaca)
;;; oo-init-elpaca.el ends here
