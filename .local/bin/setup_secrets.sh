#!/bin/should have
# Filename: setup_secrets.sh
# Author: Luis Henriquez-Perez <luis@luishp.xyz>
# Created: 2024-12-29 18:23:01
# Description: Initialize secrets in external drive to computer.
# This script takes the secrets I have in my external drive and applies them to
# a local computer.

SOURCE="/mnt/external_drive"
TARGET="$HOME"

echo "Set up passwords..."
if [ -d "$SOURCE/.password-store" ]; then
  rsync -av "$SOURCE/.password-store" "$TARGET"
else
  echo "Warning: .password-store not found on source."
fi

echo "Set up gpg key..."
rsync -av .gpg "$HOME"

echo "Set up ssh keys..."
rsync -av .ssh "$HOME"

echo "Done"
