#!/bin/bash

# Add files which have not been added.

# List unapplied files (added in source state but not in target state)
unapplied=$(chezmoi status | grep '^ \?A' | cut -c4-)

# Exit if no unapplied files
if [ -z "$unapplied" ]; then
    echo "No unapplied files to process."
    exit 0
fi

# Iterate through unapplied files and apply changes
for file in $unapplied; do
    # Apply changes to the specific file
    # echo "Applying changes to $file."
    source=$(chezmoi source-path "$file")
    chezmoi -nv apply --source-path "$source"
done
