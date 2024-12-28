#!/bin/sh
# Filename: .bash_profile
# Author: Luis Henriquez-Perez <luis@luishp.xyz>
# Created: 2024-12-28 14:30:00

# [[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$PATH:/home/luis/.local/bin"

# https://lem-project.github.io/installation/ncurses/linux/
export PATH=$PATH:~/.roswell/bin

# Profile file, runs on login. Environmental variables are set here.

# Add all directories in `~/.local/bin` to $PATH
export PATH="$PATH:$(find ~/.local/bin -type d | paste -sd ':' -)"

unsetopt PROMPT_SP 2>/dev/null

# https://github.com/White-Oak/arch-setup-for-dummies/blob/master/setting-up-ssh-agent.md
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Default programs:
# export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c -a emacs"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XINITRC="$XDG_CONFIG_HOME/x11/xinitrc"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch-config"
export MBSYNCRC="$XDG_CONFIG_HOME/mbsync/config"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/.password-store"
export LIBGL_ALWAYS_SOFTWARE=1

# Start graphical server on user's current tty if not already running.
[ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx "$XINITRC"
