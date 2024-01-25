#!/bin/bash

## automatically commit and push my dotfiles
# It is easy to loose track of dotfiles you have modified.  If you do happen to
# loose track, then you would need to find them with chezmoi status and add them
# with ~chezmoi add~.  This script is to automate this process.

# I have been considering having this script be more robust with rofi, allowing
# me to choose which files I want to automatically commit and push.

# Get the list of modified files from chezmoi status

# = M= implies that something was modified in the source state. Because if there
# is a modification happening on =chezmoi apply= and there is no difference between
# the actual state and the target state, then it has to come from the source state.

# =MM= is the flag you have to watch out for, because with that one you could
# lose information.  It means that there is a modification made in the actual
# state that differentiates it from the target state and that calling =chezmoi
# apply= could actually /overwrite/ those changes in favor of the source state.
# It might be the case that its safe when a file is modified, but it could be
# that a file is modified /both/ in the source state and the target state in
# which case the source state would override the target state's changes.

# I want a workflow where I propagate the changes from the target state to the
# source state.  I was influenced by the =chezmoi= emacs package in this regard.

# I want to note that this is similar to the =chezmoi re-add= command except it
# is not blind, the re-add command will add every file that has been modified in
# the actual state overriding any changes to the source state.
modified=$(chezmoi status | grep '^MM' | cut -c4-)

if [ -z "$modified" ]; then
    echo "No modified files to process."
    exit 0
fi

target_path=$(chezmoi target-path)
source_path=$(chezmoi source-path)

cd "$source_path" || exit

# update the changes.
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

# # navigate to the chezmoi directory
# cd $source_path

# # Get a copy of the files that are staged.
# staged_files=$(git diff --name-only --cached)

# # Unstage everything.
# git reset > /dev/null 2>&1

# # Stage the files that we just added to chezmoi.
# for target in "${targets[@]}"; do
#     source=$(chezmoi source-path "$target")
#     git add $source || { echo "Failed to stage $file"; exit 1; }
#     echo "staged $source_path"
# done

# commit_message="Update chezmoi targets."
# for file in $modified; do
#     commit_message="$commit_message"$'\n'" - $file"
# done

# git commit -m "$commit_message" > /dev/null 2>&1

# echo "auto committing: $source_path"

# # And push them.
# git push > /dev/null 2>&1 || { echo "Failed to push changes"; exit 1; }

# current_branch=$(git branch --show-current)

# # Get remote name associated with current branch
# remote_name=$(git config branch."$current_branch".remote)

# # Get remote URL, fallback to 'origin' if needed
# remote_url=$(git remote get-url "$remote_name" 2>/dev/null)
# remote_url=${remote_url:-$(git remote get-url origin 2>/dev/null)}

# echo "pushed from $source_path to $remote_url"

# for file in staged_files; do
#     if ! echo "$modified" | grep -q "$file"; then
#         git add $file || { echo "Failed to add $file"; exit 1; }
#     fi
# done
