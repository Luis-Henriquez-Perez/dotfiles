#!/bin/bash

# This is a script to symlink my dotfiles.  I wrote is as part of an initiative
# to move away from chezmoi.  The reason is I want freedom and flexibility and I
# do not necessary want to be encumbered by the chezmoi abstractions.  95% of
# the usage I use for chezmoi is just chezmoi apply.  I barely use its other
# features and I find them unintuitive.

# Directory containing your dotfiles
DOTFILES_DIR=~/.local/share/chezmoi

# Target home directory
TARGET_DIR=~

# Flags
DRY_RUN=false
FORCE=false

# Function to display usage
usage() {
  echo "Usage: $0 [--dry-run] [--force]"
  echo "  --dry-run   Simulate actions without creating symlinks"
  echo "  --force     Force overwrite existing files or symlinks"
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --force)
      FORCE=true
      shift
      ;;
    --verbose)
      VERBOSE=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      usage
      ;;
  esac
done

# Function to create symlinks
find "$DOTFILES_DIR" -type f -not -path "$DOTFILES_DIR/.git/*" -print0 | while IFS= read -r -d '' file; do
    relative_path="${file#$DOTFILES_DIR/}"
    # Replace "dot_" prefix with "."
    transformed_path=$(echo "$relative_path" | sed 's|^dot_|.|;s|/dot_|/.|g')
    # echo "$relative_path"
    # echo "$transformed_path"
    target="$TARGET_DIR/$transformed_path"
    # Calculate the relative path
    # relative_path="${file#$DOTFILES_DIR/}"
    # target="$TARGET_DIR/$relative_path"

    # Ensure the parent directory exists

    # Create the symlink

    # Handling private files
    # if [[ "$relative_path" == private_* ]]; then
    #   chmod 600 "$target"
    #   echo "Set permissions to 600 for: $target"
    # fi
    # echo "$target"
    # ln -sfn "$file" "$target"
    # Check for dry-run
    # mkdir -p "$(dirname "$target")"
    if [ "$DRY_RUN" = true ]; then
        echo "[DRY-RUN] Would link: $file -> $target"
        continue
    fi
    if [ "$VERBOSE" = true ]; then
        echo "Linked: $file -> $target"
        continue
    fi
    # echo "$file"
done