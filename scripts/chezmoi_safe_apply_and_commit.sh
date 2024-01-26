#!/bin/bash

# Apply files in the source directory that can be applied without risk of
# overwriting data.  This happens when either a new file has been added to the
# source director (= A=) or an existing file has been modified in the source
# directory (= M=) and its target counterpart has not been modified.  In both
# cases no information is lost only added so its safe to assume I would want
# these files applied.

source_path=$(chezmoi source-path)
target_path=$(chezmoi target-path)
updated=$(chezmoi status | grep '^\( A\| M\)' | cut -c4-)

if [ -z "$updated" ]; then
    echo "No files to process."
    exit 0
fi

# Git commands have to be invoked from the source directory itself.
cd "$source_path" || exit

# Save the files that are already staged beforehand and then unstage everything
# so that I do not end up committing something I should not.
staged_files=$(git diff --name-only --cached)
has_committed=false

echo "Unstage everything."
git reset > /dev/null 2>&1

for file in $updated; do
    target="$target_path/$file"
    source=$(chezmoi source-path "$target")
    echo "Apply $source to $target."
    chezmoi apply "$target"
    # If the files in unapplied are not committed, commit them.
done

# If I ended up committing something, try to push the changes.
# if [ "$has_committed" = true ]; then
#     if git push; then
#         echo "Successfully pushed changes."
#     else
#         echo "Failed to push committed changes."
#     fi
# fi

# # Stage the files that we just added to chezmoi.
# echo "Re-staging previously staged files."
# for file in $staged_files; do
#     git add $file
#     echo "Restage $file."
# done

# if git status --porcelain | grep -q "^?? $file\|^ M $file"; then
#     # Add the file to staging
#     git add "$file"
#     # Commit the file
#     git commit -m "Update $file." > /dev/null 2>&1
#     has_committed=true
#     echo "Commit changes in $file."
# else
#     echo "Not commit needed for $file, already up-to-date."
# fi

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
