#!/bin/bash

## automatically commit and push my dotfiles
# This script is so that I do not have to always check to see which files have
# been modified in the target directory and adding them all.

### check whether dotfiles are modified
# There should be a global variable.

# Also I need to loop through all the files managed by chezmoi and check to see
# if their targets are modified.  If both target and source are modified
# potentially do nothing, and leave it to me to check.  I could also potentially.

# For every managed file.
# If modified and source is not modified chezmoi apply
### Declare variables
# The invocation ~chezmoi status~.
# https://www.chezmoi.io/reference/commands/status/
# Get the list of modified files from chezmoi status
modified=$(chezmoi status | grep '^MM' | cut -c4-)

if [ -z "$modified" ]; then
    echo "No modified files to process."
    exit 0
fi

target_path=$(chezmoi target-path)
source_path=$(chezmoi source-path)
### Add files to chezmoi

# Loop through each modified file
for file in $modified; do
    full_path="$target_path/$file"
    chezmoi add $full_path
    echo "Added $full_path"
done

# Navigate to the chezmoi directory
echo "changing directory to $source_path"
cd $source_path

staged_files=$(git diff --name-only --cached)

git reset

for file in $modified; do
    full_path="$source_path/$file"
    echo "staging $full_path"
    git add $full_path || { echo "Failed to stage $file"; exit 1; }
done

# commit_message="Add modified chezmoi files."
# for file in $modified; do
#     commit_message="$commit_message\n - $file"
# done

git commit -m "Add modified chezmoi files"

echo "auto committing: " $source_path

git push || { echo "Failed to push changes"; exit 1; }

echo "pushed " $source_path

for file in staged_files; do
    if ! echo "$modified" | grep -q "$file"; then
        git add $file || { echo "Failed to add $file"; exit 1; }
    fi
done
