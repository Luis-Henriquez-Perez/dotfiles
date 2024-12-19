#!/bin/bash
# Filename: dotfiles_init.sh
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-16 12:26:13
# Description: Initialize dotfiles from bare git repo.

DOTFILES_URL="https://github.com/Luis-Henriquez-Perez/dotfiles.git"
DOTFILES_DIR="$HOME/.dotfiles"
BACKUP_DIRECTORY="$HOME/.dotfiles_backup"

dot () {
  git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
}

echo "Cloning bare Git repository from $DOTFILES_URL"
git clone --branch master--bare-repo --bare $DOTFILES_URL $DOTFILES_DIR

if dot checkout; then
  echo "Successfully Checked out dotfiles."
else
  echo "Conflicts detected! Backing up pre-existing dotfiles to $BACKUP_DIRECTORY..."
  mkdir -p "$BACKUP_DIRECTORY"

  for file in $(dot checkout 2>&1 | grep -E "^\s+.+" | awk '{print $1}'); do
    echo
    mv "$HOME/$file" "$BACKUP_DIRECTORY/" || echo "Warning: Could not move $file"
  done

  dot checkout
fi;

dot config --local status.showUntrackedFiles no

echo "Dotfiles initialized successfully."
