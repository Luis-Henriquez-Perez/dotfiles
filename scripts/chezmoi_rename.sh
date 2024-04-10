#!/bin/bash

# It is awkward to rename a file in the target directory.  Since the file is not
# being directly version-controlled like in as with git.  This script is to try
# to automate the renaming process.  I am not sure how I will apply it though so
# that its automatic.

# https://github.com/twpayne/chezmoi/issues/2850
# I do not completely get the explanations for renaming.  I do not understand
# the multiple machine thing.  Do I even have to do chezmoi forget or can I just
# delete? I do not know.
# https://github.com/twpayne/chezmoi/issues/2850

source_path=$(chezmoi source-path)

# Check if enough arguments are given
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <old-filename> <new-filename>"
    exit 1
fi

# Assign arguments to variables
old_filename=$1
new_filename=$2

# Step 1: Rename file and add the new file to chezmoi
mv "$old_filename" "$new_filename"
chezmoi add "$new_filename"

# Step 2: Remove the old file
echo "$old_filename" >> ~/.local/share/chezmoi/.chezmoiremove
chezmoi add ~/.local/share/chezmoi/.chezmoiremove

# Optional: Commit and push changes if using a version control system
# read -p "Do you want to commit and push changes to your repository? (y/n) " -n 1 -r

# if [[ $REPLY =~ ^[Yy]$ ]]
# then
#     git -C "$source_path" add .
#     git -C "$source_path" commit -m "Rename $old_filename to $new_filename"
#     git -C "$source_path" push
# fi
