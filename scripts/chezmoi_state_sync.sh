#!/bin/bash

## chezmoi
# The point of this script is to keep =chezmoi status= empty.
### declare target and source paths
target_path=$(chezmoi target-path)
source_path=$(chezmoi source-path)
### apply files that can be applied safetly
for file in $updated; do
    target="$target_path/$file"
    source=$(chezmoi source-path "$target")
    chezmoi apply "$target"
    echo "Applied $source to $target."
    # If the files in unapplied are not committed, commit them.
done
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

    chezmoi add "$target"
    echo "Added $target."
done
### delete files
# Delete files I have deleted in the target directory.
# to_forget=$(chezmoi status | grep '^\(DA\)' | cut -c4-)

# for file in $to_forget; do
#     # source=$(chezmoi source-path "$target_path/$file")
#     target="$target_path/$file"
#     chezmoi forget "$target"
#     echo "Forgetting $target from source directory."
# done
