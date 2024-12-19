#!/bin/bash
# Filename: script_install_packages.sh
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-19 15:54:58
# Description: Install my packages.

# File containing the list of packages
PACKAGE_FILE="packages.txt"

# Check if the package file exists
if [ ! -f "$PACKAGE_FILE" ]; then
    echo "Package list file '$PACKAGE_FILE' not found!"
    exit 1
fi

# Read the package list and install missing packages
while IFS= read -r package || [ -n "$package" ]; do
    # Skip empty lines and comments
    if [[ -z "$package" || "$package" =~ ^# ]]; then
        continue
    fi

    # Check if the package is already installed
    if pacman -Q "$package" &>/dev/null; then
        echo "Package '$package' is already installed."
    else
        echo "Installing package '$package'..."
        yay -S --noconfirm "$package"
    fi
done < "$PACKAGE_FILE"

echo "All packages are processed."
