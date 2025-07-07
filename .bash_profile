#!/bin/sh
# Filename: .bash_profile
# Author: Luis Henriquez-Perez <luis@luishp.xyz>
# Created: 2024-12-28 14:30:00

[[ -f ~/.profile ]] && . ~/.profile
[[ -f ~/.bashrc ]] && . ~/.bashrc

# Start graphical server on user's current tty if not already running.
[ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx "$XINITRC"
