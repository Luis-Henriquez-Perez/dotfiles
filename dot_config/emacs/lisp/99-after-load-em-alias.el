;;; 99-after-load-em-alias.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This file contains all of my eshell aliases.
;;
;;; Code:
(require 'em-alias)
;; I am not sure whether to leave this here or create another configuration file
;; that loads when `em-alias' is loaded.
;; https://olddeuteronomy.github.io/post/eshell-aliases-and-prompt/
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#aliases
;; (eshell/alias "e" "find-file $1")
;; For now do this, but I just really want to scroll up.  I do not want to
;; actually delete the buffer contents.  I mean I guess its O.K. since the
;; contents should be saved in eshell-history, but its more secure to actuall
;; have the physical buffer contents.
(eshell/alias "clear" "eshell/clear t")
(eshell/alias "ff" "find-file $1")
(eshell/alias "fo" "find-file-other-window $1")
(eshell/alias "ffow" "find-file-other-window $1")
(eshell/alias "open" "find-file $1")
(eshell/alias "d" "dired $1")
;; https://howardism.org/Technical/Emacs/eshell-why.html
(eshell/alias "less" "view-file $1")
;; https://stackoverflow.com/questions/10566532/how-can-bash-execute-a-command-in-a-different-directory-context
;; TODO: Allow arguments to commands.  I ommited them for the sake o.
(eshell/alias "emacs-test" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "etest" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "estatus" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "emacs-compile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "ecompile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "emacs-eval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "eclean" "{cd $user-emacs-directory; eldev clean}")
(eshell/alias "eeval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "apply-emacs" "chezmoi apply ~/.config/emacs --force")
(eshell/alias "update-emacs" "apply-emacs && eclean && ecompile")

;; TODO: figure out a naming system for these commands
;; TODO: write a command for publishing to blog.  This is preliminary.
;; The publish command has to be called from the emacs directory.  I want to be
;; able to type "publish" and have it publish my blog.  Now as I am writing this
;; I feel like its cool if I had a command to publish and push to github.  Maybe
;; something like "update-website"?

;; [[info:eshell#Dollars Expansion][eshell#Dollars Expansion]]

;; I am not 100% sure I need to switch to the html directory first.
;; TODO: Determine why the exit-code fo this is non-zero.  Potentially give it
;; an exit-code of 0.
(eshell/alias "publish" "{cd $(expand-file-name \"html\" \"~/Documents/blog\") ; (shut-up (org-publish \"blog\" t))}")
(eshell/alias "epublish" "{cd $(expand-file-name \"html\" \"~/Documents/blog\") ; (shut-up (org-publish \"blog\" t))}")

(eshell/alias "install" "sudo pacman -S --noconfirm $1")
(eshell/alias "uninstall" "sudo pacman -Rs --noconfirm $1")

;; TODO: Do not show me descriptions when searching...
(eshell/alias "search" "pacman -Ss $1")
(eshell/alias "search-quiet" "pacman -Ssq $1")

;; TODO: maybe make a general update command for email, packages, etc.
(eshell/alias "update" "sudo pacman -Syu")
(eshell/alias "update-system" "sudo pacman -Syu")
(eshell/alias "update-email" "mbsync -a")
(eshell/alias "list-wifi" "nmcli dev wifi list")
(eshell/alias "up" "eshell-up $1")
(eshell/alias "pk" "eshell-up-peek $1")
;; chezmoi apply ~/.config/emacs/lisp
;; chezmoi apply ~/.configuration/
;; (eshell/alias "update-wallpaper" "")
;;; provide
(provide '99-after-load-em-alias)
;;; 99-after-load-em-alias.el ends here
