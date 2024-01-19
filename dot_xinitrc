#!/bin/sh

## xinitrc
# The program [[https://wiki.archlinux.org/title/Xinit#xinitrc][xinit]] lets me manually start the [[https://wiki.archlinux.org/title/Xorg][Xorg]] display server.
# The =.xinitrc= is the shell script use to start up the client programs for an
# invocation of =xinit= if a custom script is not provided.  When I use it I invoke
# =startx=.
### set the cursor
xsetroot -cursor_name left_ptr
### load xprofile
# Make sure this is before the 'exec' command or it won't be sourced.
# I have taken this from the [[https://wiki.archlinux.org/title/Xprofile][archwiki article]] on =xprofile=.  It recommends
# loading the =xprofile= from the =xinitrc=.  I am still a bit confused.
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile
### set the window manager dynamically
# https://bbs.archlinux.org/viewtopic.php?id=139353
# Add code so that this can open in multiple different window managers.
# One of my first questions is: can I do this in guile?

# This is adapted from the archwiki's xinit article on switching
# between window environments. https://wiki.archlinux.org/title/Xinit#Switching_between_desktop_environments/window_managers.
# Here Xfce is kept as default
session=${1:-awesome}

case $session in
    qtile             ) exec qtile start;;
    stumpwm           ) exec ~/stumpwm.ros;;
    awesome|awesomewm ) exec awesome;;
    i3|i3wm           ) exec i3;;
    kde               ) exec startplasma-x11;;
    xfce|xfce4        ) exec startxfce4;;
    # No known session, try to run it as command
    *                 ) exec $1;;
esac
