#!/bin/bash

# This is a script to symlink my dotfiles.

# Directory containing your dotfiles
DOTFILES_DIR=~/.local/share/chezmoi

# Target home directory
TARGET_DIR=~

# Function to create symlinks
find "$DOTFILES_DIR" -type f -not -path "$DOTFILES_DIR/.git/*" -print0 | while IFS= read -r -d '' file; do
    relative_path="${file#$DOTFILES_DIR/}"
    # Replace "dot_" prefix with "."
    transformed_path=$(echo "$relative_path" | sed 's|^dot_|.|;s|/dot_|/.|g')
    # echo "$relative_path"
    # echo "$transformed_path"
    target="$TARGET_DIR/$transformed_path"
    echo "$target"
    # ln -sfn "$file" "$target"
    echo "Linked: $file -> $target"
    # echo "$file"
done
# Calculate the relative path
# relative_path="${file#$DOTFILES_DIR/}"
# target="$TARGET_DIR/$relative_path"

# Ensure the parent directory exists
# mkdir -p "$(dirname "$target")"

# Create the symlink
