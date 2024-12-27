#!/bin/bash

## chezmoi
# It is easy to loose track of dotfiles you have modified.  If you do happen to
# loose track, then you would need to find them with chezmoi status and add them
# with ~chezmoi add~.  This script is to automate this process.

# I have been considering having this script be more robust with rofi, allowing
# me to choose which files I want to automatically commit and push.
### declare target and source paths
target_path=$(chezmoi target-path)
source_path=$(chezmoi source-path)
### apply files that can be applied safetly
for file in $updated; do
    target="$target_path/$file"
    source=$(chezmoi source-path "$target")
    echo "Apply $source to $target."
    chezmoi apply "$target"
    # If the files in unapplied are not committed, commit them.
done
### set the directory
# I am using the full path for =chezmoi add=, so where I am does not matter for
# that.  But =git diff= needs to be in the source directory.
cd "$source_path"
### add modified files in target state
# Add files from the target state that have been modified /and/ whose
# corresponding source state file /has not/ been modified.  This suggests the
# file was edited after the source state file and that I want the changes in the
# target file to be applied.

# =MM= means that there is a modification made in the actual
# state that differentiates it from the target state and that calling =chezmoi
# apply= could actually /overwrite/ those changes in favor of the source state.
modified=$(chezmoi status | grep '^MM' | cut -c4-)

for file in $modified; do
    target="$target_path/$file"
    source_file=$(chezmoi source-path "$target")

    if git diff --quiet --exit-code -- "$source_file"; then
        chezmoi add "$target"
        echo "Added $target."
    else
        echo "Skipped $target as the source state is modified."
    fi
done
### delete files
deleted=$(chezmoi status | grep '^\(DA\)' | cut -c4-)

for file in $deleted; do
    source=$(chezmoi source-path "$file")
    rm "$source"
    echo "Remove $source from source directory."
done
