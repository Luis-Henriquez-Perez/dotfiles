#!/bin/bash

## automatically commit and push my dotfiles
# It is easy to loose track of dotfiles you have modified.  If you do happen to
# loose track, then you would need to find them with chezmoi status and add them
# with ~chezmoi add~.  This script is to automate this process.
### check whether dotfiles are modified
# There should be a global variable.

# Also I need to loop through all the files managed by chezmoi and check to see
# if their targets are modified.  If both target and source are modified
# potentially do nothing, and leave it to me to check.  I could also potentially.

# For every managed file.
# If modified and source is not modified chezmoi apply
# Declare variables
# The invocation ~chezmoi status~ produces a.
# https://www.chezmoi.io/reference/commands/status/
# Get the list of modified files from chezmoi status
modified=$(chezmoi status | grep '^MM' | cut -c4-)

# If there are no modified files that need adding, we are done.
if [ -z "$modified" ]; then
    echo "No modified files to process."
    exit 0
fi

target_path=$(chezmoi target-path)
source_path=$(chezmoi source-path)
targets=()

# update the changes.
for file in $modified; do
    target="$target_path/$file"
    targets+=("$target")  # Append to array
    chezmoi add $target
    echo "Added $target."
done

# navigate to the chezmoi directory
echo "changing directory to $source_path"
cd $source_path

# Get a copy of the files that are staged.
staged_files=$(git diff --name-only --cached)

# Unstage everything.
git reset > /dev/null 2>&1

# Stage the files that we just added to chezmoi.
for target in "${targets[@]}"; do
    source_path=$(chezmoi source-path "$target")
    git add $source_path || { echo "Failed to stage $file"; exit 1; }
    echo "staged $source_path"
done

commit_message="Update chezmoi targets."
for file in $modified; do
    commit_message="$commit_message"$'\n'" - $file"
done

git commit -m "$commit_message" > /dev/null 2>&1

echo "auto committing: $source_path"

# And push them.
git push > /dev/null 2>&1 || { echo "Failed to push changes"; exit 1; }

current_branch=$(git branch --show-current)

# Get remote name associated with current branch
remote_name=$(git config branch."$current_branch".remote)

# Get remote URL, fallback to 'origin' if needed
remote_url=$(git remote get-url "$remote_name" 2>/dev/null)
remote_url=${remote_url:-$(git remote get-url origin 2>/dev/null)}

echo "pushed from $source_path to $remote_url"

# for file in staged_files; do
#     if ! echo "$modified" | grep -q "$file"; then
#         git add $file || { echo "Failed to add $file"; exit 1; }
#     fi
# done
