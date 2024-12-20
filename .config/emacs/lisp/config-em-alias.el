;;; config-em-alias.el --- em-alias configuration -*- lexical-binding: t; -*-
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
;;;; requirements
(require 'em-alias)
;;;; git
;; https://stackoverflow.com/questions/927358/how-do-i-undo-the-most-recent-local-commits-in-git#927386
;; TODO allow the specification of how many steps to undo
(eshell/alias "git-undo" "git reset HEAD~")
;;;; emacs maintenance
(eshell/alias "emacs-test" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "etest" "(let ((default-directory user-emacs-directory)) ${eldev -d test $1})")
(eshell/alias "estatus" "(let ((default-directory user-emacs-directory)){eldev -d test $1})")
(eshell/alias "emacs-compile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "ecompile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "emacs-eval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "eclean" "{cd $user-emacs-directory; eldev clean}")
(eshell/alias "eeval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "apply-emacs" "chezmoi apply ~/.config/emacs --force")
(eshell/alias "update-emacs" "apply-emacs && eclean && ecompile")
;;;; archlinux
(eshell/alias "orphan" "pacman -Qtd $*")
(eshell/alias "files" "pacman -Ql $1")
(eshell/alias "pac" "sudo pacman --noconfirm $*")
(eshell/alias "pacman" "sudo pacman -S --noconfirm $*")
(pop eshell-command-aliases-list)
(eshell/alias "install" "sudo pacman -S --noconfirm $*")
(eshell/alias "remove" "sudo pacman -Rns --noconfirm $*")
(eshell/alias "uninstall" "sudo pacman -Rns --noconfirm $*")
(eshell/alias "search" "pacman -Ss $*")
(eshell/alias "search-quiet" "pacman -Ssq $*")
(eshell/alias "update" "sudo pacman -Syu")
(eshell/alias "update-system" "sudo pacman -Syu")

(eshell/alias "update-email" "mbsync -a")

(eshell/alias "list-wifi" "nmcli dev wifi list")
(eshell/alias "listwifi" "nmcli dev wifi list")
;;;; dotfiles
(eshell/alias "dotadd" "dot add $1 && dot commit -m \"Add $1.\" $1 && git push")
;; Define a commit-undo
(eshell/alias "add" "dot add $1 && dot commit -m \"Add $1.\" $1 && dot push $1")
(eshell/alias "dot" (format "%s --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $*"
                            (executable-find "git")))
(eshell/alias "dots" "dot status --porcelain")
;;;; blogging
(eshell/alias "publish" "{cd $(expand-file-name \"html\" \"~/Documents/blog\") ; (shut-up (org-publish \"blog\" t))}")
(eshell/alias "epublish" "{cd $(expand-file-name \"html\" \"~/Documents/blog\") ; (shut-up (org-publish \"blog\" t))}")
;;;; miscellaneous
(eshell/alias "up" "eshell-up $1")
(eshell/alias "pk" "eshell-up-peek $1")
(eshell/alias "unpack" "mv $1/* . && rmdir $1")
;; rsync -e "ssh -p 65002" -uvr html/ u150683034@195.35.38.189:/home/u150683034/domains/luishp.blog/public_html
;; rsync -e 'ssh -p PORT_NUMBER' -uvr html/ username@IP_ADDRESS:/path/to/destination/
;; chezmoi apply ~/.config/emacs/lisp
;; chezmoi apply ~/.configuration/
;; (defun! oo-download-audio ()
;;   (interactive)
;;   (set! default-directory (expand-file-name "~/Music"))
;;   (set! url (shell-quote-argument (yeetube-get-url)))
;;   (set! command (format "/usr/bin/yt-dlp -x --audio-format wav --embed-thumbnail %s" url))
;;   (call-process-shell-command command))
;; (eshell/alias "update-wallpaper" "")
;; (call-process-shell-command "/usr/bin/yt-dlp -x --audio-format wav --embed-thumbnail https\\://youtube.com/watch\\?v\\=DT61L8hbbJ4")
;; https://youtube.com/watch?v=soJLOqC7_FU
;; (call-process-shell-command "/usr/bin/yt-dlp -x --audio-format wav --embed-thumbnail")
;; ffmpeg -i your_video.mp4 -vf "select=eq(pict_type\,PICT_TYPE_I)" -vsync vfr thumbnail%04d.png
;; convert -delay 5 -loop 0 thumbnail*.png animation.gif
;; maim -i $(xdotool getactivewindow) screenshot.png
;; TODO: A command that will combine rm/rmdir in one so it will remove a file or
;; directory.  It should default to moving the targets to trash.

;; TODO: A command for moving everything in a directory to outer directory.

;; TODO: Determine how to make the output optional and in that case just use the
;; filename of the original file.  Obviously prompt for what to do if the
;; filename already exists.
(eshell/alias "html-to-org" "pandoc -f html -t org $1 -o $2")
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
;; I decided that I almost always prefer opening the file in another window.  So
;; I am replacing ff with `find-file-other-window'
;; (eshell/alias "ff" "find-file $1")
(eshell/alias "ff" "fo $1")
(eshell/alias "fo" "find-file-other-window $1")
(eshell/alias "ffow" "find-file-other-window $1")
(eshell/alias "open" "find-file $1")
(eshell/alias "d" "dired $1")
;; https://howardism.org/Technical/Emacs/eshell-why.html
;; https://stackoverflow.com/questions/10566532/how-can-bash-execute-a-command-in-a-different-directory-context
;; TODO: Allow arguments to commands.  I ommited them for the sake of.
;;; provide
(provide 'config-em-alias)
;;; config-em-alias.el ends here
