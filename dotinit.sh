#!/bin/bash
# Filename: dotfiles_init.sh
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-16 12:26:13
# Description: Initialize dotfiles from bare git repo

DOTFILES_URL="https://github.com/Luis-Henriquez-Perez/dotfiles.git"
DOTFILES_DIR="$HOME/.dotfiles"
BACKUP_DIRECTORY="$HOME/.dotfiles_backup"

dot () {
  git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
}

# Clone the repo
echo "Cloning bare Git repository from $DOTFILES_URL"
printf "Cloning bare git repo from %s\n" "$DOTFILES_URL"
git clone --bare $DOTFILES_URL $HOME/.dotfiles

dot checkout

if [ $? = 0 ]; then
  echo "Checked out dotfiles.";
else
    mkdir -p "$BACKUP_DIRECTORY"
    printf "Backing up pre-existing dotfiles into %s\n" "$BACKUP_DIRECTORY"
    dot checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi;

dot checkout

dot config --local status.showUntrackedFiles no
