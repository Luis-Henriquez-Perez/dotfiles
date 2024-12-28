# -*- mode: sh -*-

## .bashrc
# I used the bullet headings in this file because having mutiple comment
# syntaxes next to each other does not work in this file.  Is it a bug?  I
# confirmed additionally that this is also the case for lua files.  I think I
# have to either try to fix this myself in the outshine code or just switch to
# the bullet syntax, at least for non-elisp files.

### something I do not yet understand
# No idea what this stuff does.
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

### Wallpaper file
# USED_WALLPAPER_FILE=$HOME/.cache/wallpapers
export CURRENT_WALLPAPER="$HOME/.cache/wallpapers/current_wallpaper.jpg"
