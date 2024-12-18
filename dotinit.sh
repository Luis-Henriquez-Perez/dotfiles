#!/bin/bash
# Filename: dotfiles_init.sh
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-16 12:26:13
# Description: initialize dotfiles from bare git repo

# Setup the aliases
# alias dot='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
dot () {
  git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
}

echo "$HOME/dotfiles" >> .gitignore

# Clone the repo
git clone --bare  $HOME/.dotfiles

dot checkout

if [ $? = 0 ]; then
  echo "Checked out config.";
else
    mkdir -p .config-backup
    echo "Backing up pre-existing dot files.";
    dot checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi;

dot checkout

dot config --local status.showUntrackedFiles no
