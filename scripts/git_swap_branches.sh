#!/bin/bash
# Filename: git_swap_branches.sh
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-17 17:11:56
# Description: Swap the names of two git branches.

# Function to print usage and exit
usage() {
    echo "Usage: $0 <branch1> <branch2>"
    echo "Swaps the names of two Git branches."
    exit 1
}

# Check if two arguments are provided
if [ "$#" -ne 2 ]; then
    usage
fi

BRANCH1="$1"
BRANCH2="$2"
TMP_BRANCH="temp-branch-for-swap"

# Check if inside a Git repository
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "Error: Not inside a Git repository."
    exit 1
fi

# Ensure both branches exist
if ! git show-ref --verify --quiet "refs/heads/$BRANCH1"; then
    echo "Error: Branch '$BRANCH1' does not exist."
    exit 1
fi

if ! git show-ref --verify --quiet "refs/heads/$BRANCH2"; then
    echo "Error: Branch '$BRANCH2' does not exist."
    exit 1
fi

# Check out BRANCH1
echo "Checking out '$BRANCH1'..."
git checkout "$BRANCH1" || exit 1

# Rename BRANCH1 to a temporary branch
echo "Renaming '$BRANCH1' to '$TMP_BRANCH'..."
git branch -m "$TMP_BRANCH" || exit 1

# Rename BRANCH2 to BRANCH1
echo "Renaming '$BRANCH2' to '$BRANCH1'..."
git branch -m "$BRANCH2" "$BRANCH1" || exit 1

# Rename the temporary branch to BRANCH2
echo "Renaming '$TMP_BRANCH' to '$BRANCH2'..."
git branch -m "$TMP_BRANCH" "$BRANCH2" || exit 1

# Confirm success
echo "Successfully swapped branches '$BRANCH1' and '$BRANCH2'."
