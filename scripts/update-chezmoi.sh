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
### set the directory
# I am using the full path for =chezmoi add=, so where I am does not matter for
# that.  But =git diff= needs to be in the source directory.
cd "$source_path"
### stage the files
# Get a copy of the files that are staged.
staged_files=$(git diff --name-only --cached)

# Unstage everything.
echo "Unstage everything."
git reset > /dev/null 2>&1

# Stage the files that we just added to chezmoi.
# for target in "${targets[@]}"; do
#     source=$(chezmoi source-path "$target")
#     git add $source || { echo "Failed to stage $file"; exit 1; }
#     echo "staged $source_path"
# done
### apply modified and added files in source directory
# Apply files in the source directory that can be applied without
# risk of overwriting data.  This happens when either a new file has been added
# to the source directly (= A=) or an existing file has been modified in the
# source directory (= M=) when its target counterpart has not been modified.  In
# both cases no information is lost so its safe to apply.
updated=$(chezmoi status | grep '^\( A\| M\)' | cut -c4-)

for file in $updated; do
    target="$target_path/$file"
    source=$(chezmoi source-path "$target")
    echo "Apply $source to $target."
    chezmoi apply "$target" --force
    # Check if the file is uncommitted
    # If the files in unapplied are not committed, commit them.
    if git status --porcelain | grep -q "^?? $file\|^ M $file"; then
        # Add the file to staging
        git add "$file"
        # Commit the file
        git commit -m "Update $file."
        echo "Committed changes in $file."
    else
        echo "$file is already committed."
    fi
done
### add modified files in target state
# Add files from the target state that have been modified /and/ whose
# corresponding source state file /has not/ been modified.  This suggests the
# file was edited after the source state file and that I want the changes in the
# target file to be applied.

# =MM= means that there is a modification made in the actual
# state that differentiates it from the target state and that calling =chezmoi
# apply= could actually /overwrite/ those changes in favor of the source state.
# modified=$(chezmoi status | grep '^MM' | cut -c4-)

# for file in $modified; do
#     target="$target_path/$file"
#     source_file=$(chezmoi source-path "$target")

#     if git diff --quiet --exit-code -- "$source_file"; then
#         chezmoi add "$target"
#         echo "Added $target."
#     else
#         echo "Skipped $target as the source state is modified."
#     fi
# done
### delete files
# deleted=$(chezmoi status | grep '^\(DA\)  | cut -c4-)

# for file in $deleted; do
#     source="$source_path/$file"
#     rm "$source"
#     echo "Remove $source from source directory."
# done
### restage files
### acknowledgements
# For a lot of this I took as an example =chezmoi-write= from the chezmoi
# package.  But there is a difference: =chezmoi-write= has access to the file,
# whereas with this command I do not.  The commands =chezmoi status= and
# =chezmoi managed= only output partial paths.  To get the full path you combine
# the output of =chezmoi target-path= and each of =chezmoi-managed=.  And then
# to get the source you can apply it to the result of that.
# Add files which have not been added.
### notes
# I am trying here to automate the easy ones.  Therefore, = A=, = M=, =DA= I
# think I would almost always want applied. =MM= I would have to consider more
# closely, probably having to do =chezmoi diff=.  These are the files that have
# been modified in the source state and the target state--something that I do
# not recommend doing.  I would almost always prefer modifying target files.
# Sometimes modify the source files.  But never modify both because then you
# have to diff them.

# = A= and = M= are do not require confirmation chezmoi.  But =DA=, signaling
# the recreation of a file that has been deleted in the actual filesystem, but
# would be added back

# List unapplied files (added in source state but not in target state)
# \|DA.

# Basically do the changes that are safe to do--by which I mean cannot result in
# the loss of information.
# I do not think any of these actions would change the source state.  All of
# them propagate the source state to the target state: = A= and =DA= a add a
# file whereas = M= modifies an existing file in the target state.  So I should
# not need any version controlling here.

# = M= implies that something was modified in the source state. Because if there
# is a modification happening on =chezmoi apply= and there is no difference between
# the actual state and the target state, then it has to come from the source state.

# I want a workflow where I propagate the changes from the target state to the
# source state.  I was influenced by the =chezmoi= emacs package in this regard.

# I want to note that this is similar to the =chezmoi re-add= command except it
# is not blind, the re-add command will add every file that has been modified in
# the actual state overriding any changes to the source state.  Of course it
# does prompt you when is in danger of being overridden.
