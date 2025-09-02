# Profile file, runs on login. Environmental variables are set here.
# This file should be written in such a way that it is safe to run repeatedly.
case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) PATH="$PATH:$HOME/.local/bin" ;;
esac

case ":$PATH:" in
  *":$HOME/.roswell/bin:"*) ;;
  *) PATH="$PATH:$HOME/.roswell/bin" ;;
esac

# Maybe I need this for plank?
export XDG_SESSION_DESKTOP=x11

# export ALTERNATE_EDITOR=""
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export HISTFILE="$XDG_CACHE_HOME/bash_history"
export LESSHISTFILE="$XDG_CACHE_HOME/less_history"
# export FEH="$XDG_CACHE_HOME/"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/config"
export MBSYNCRC="$XDG_CONFIG_HOME/isync/mbsyncrc"
export CARGO_HOME="$XDG_DATA_HOME/cargo"

export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c -a emacs"
export TERMINAL="alacritty"
export BROWSER="qutebrowser"

# I am not sure about doing this.  I do not know if the consistency outweights
# the additional configuration and complexity.  I think it is superficial and
# opinionated that he home directory is messy because of these files.
# export XINITRC="$XDG_CONFIG_HOME/x11/xinitrc"
export PASSWORD_STORE_DIR="$HOME/.password-store"
export LIBGL_ALWAYS_SOFTWARE=1
# https://github.com/White-Oak/arch-setup-for-dummies/blob/master/setting-up-ssh-agent.md
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Stop creating a .w3m file in my home directory.
export W3M_DIR="$XDG_CONFIG_HOME/.config/w3m"
# Do not create .python_history file in my home directory.
export PYTHON_HISTORY="$HOME/.cache/python_history"
export PARALLEL_HOME="$HOME/.cache/parallel"
