#!/bin/bash
## universal update script

# The host is based on arch.
if [ -d /etc/pacman.d/ ]
then
    sudo pacman -Syu
fi

# The host is based on Debian or Ubuntu.
if [ -d /etc/apt ]
then
    sudo apt update
    sudo apt dist-upgrade
fi
