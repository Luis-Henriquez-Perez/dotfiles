#!/bin/bash
# Filename: dotfiles_init.sh
# Author: Luis Henriquez-Perez <luis@luishp.xyz>
# Created: 2024-12-23 16:33:00
# Description: Initialize dotfiles from git repo.

BACKUP_DIR=$(mktemp -d ~/backup_dir_XXXXXX)
OUTPUT=$(git checkout develop 2>&1)

while true; do
    # I do not check for the exit code because it seems that git produces a 0
    # excited code for checking out even if it was unsuccessful.  So I actually
    # have to check for the specific error message.
    if ! echo "$OUTPUT" | grep -q "error: The following untracked"; then
        echo "Checkout succeeded."
        break
    fi
    CONFLICTING_FILES=$(echo "$OUTPUT" | grep -E "^\s+.+" | awk '{print $1}')

    # When the number of conflicting files exceeds a certain amount the git
    # checkout message does not display them all.  That is why I need to use a
    # while loop to get them all.  Also sometimes the file name is cut
    # off--which is why I check to see if the file exists.
    for file in $CONFLICTING_FILES; do
        FILE_PATH="$HOME/$file"
        if [[ -f $FILE_PATH ]]; then
            if mv "$FILE_PATH" "$BACKUP_DIR"; then
                echo "Moved $FILE_PATH -> $BACKUP_DIR"
            else
                echo "Error: Unable to move $FILE_PATH to $BACKUP_DIR"
                exit 1
            fi
        else
            echo "Error: $FILE_PATH does not exist or is not a regular file."
        fi
    done

    OUTPUT=$(git checkout develop 2>&1)
done
