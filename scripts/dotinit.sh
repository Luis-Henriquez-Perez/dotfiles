#!/bin/bash
# Filename: dotinit.sh
# Author: Luis Henriquez-Perez <luis@luishp.xyz>
# Created: 2024-12-23 16:33:00
# Description: Initialize dotfiles from git repo.

REPO="https://github.com/Luis-Henriquez-Perez/dotfiles.git"
# THROWAWAY_DIR=$(mktemp -u "throwaway_dir_XXXXXX")

# Clone the repository with a separate git directory
cd "$HOME" || { echo "Error: Unable to change to HOME directory."; exit 1; }
echo "Cloning $REPO..."
git clone -n --separate-git-dir="$HOME/.git" "$REPO" throwaway || { echo "Error: Failed to clone $REPO."; exit 1; }
rm -rf throwaway

# Configure Git to hide untracked files
if ! git config --local status.showUntrackedFiles no; then
    echo "Error: Failed to configure Git to hide untracked files.";
    exit 1;
fi

echo "Attempting to checkout files..."
OUTPUT=$(git checkout 2>&1)

while true; do
    if ! echo "$OUTPUT" | grep -q "error: The following untracked"; then
        echo "Checkout succeeded."
        break
    fi
    # Create a backup directory for conflicting files
    BACKUP_DIR=$(mktemp -d "$HOME/backup_dir_XXXXXX")
    CONFLICTING_FILES=$(echo "$OUTPUT" | grep -E "^\s+.+" | awk '{print $1}')

    # Move conflicting files to the backup directory
    for file in $CONFLICTING_FILES; do
        FILE_PATH="$HOME/$file"
        if [[ -f "$FILE_PATH" ]]; then
            if mv "$FILE_PATH" "$BACKUP_DIR"; then
                echo "Moved $FILE_PATH -> $BACKUP_DIR"
            else
                echo "Error: Unable to move $FILE_PATH to $BACKUP_DIR"
                exit 1
            fi
        else
            echo "Warning: $FILE_PATH does not exist or is not a regular file."
        fi
    done
    OUTPUT=$(git checkout 2>&1)
done
