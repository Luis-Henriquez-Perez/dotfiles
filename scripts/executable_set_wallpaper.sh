#!/bin/bash

# Directory containing wallpapers
wallpaper_dir="$HOME/Pictures/wallpapers/"
used_wallpapers_file="$HOME/.cache/used_wallpapers_file"

# Ensure the wallpaper file exists.
touch "$used_wallpapers_file"

# Choose a random wallpaper from the directory that is not among the chosen wallpapers.
all=$(find "$wallpaper_dir" -type f \( -name "*.jpg" -o -name "*.png" \))
# Get the used wallpapers from the used_wallpapers_file.
used=$(cat "$used_wallpapers_file")
# Choose a wallpaper that's not used--the set-difference of (- all used)
# Chatgpt helped with this line, no idea what =comm= is.
chosen=$(comm -23 <(echo "$all" | sort) <(echo "$used" | sort) | shuf -n 1)

# If chosen is empty, then reset the used wallpapers file and choose again
if [[ -z $chosen ]]; then
    echo "No new wallpapers found. Resetting used wallpapers."
    # This clears the wallpaper file.
    > "$used_wallpapers_file"
    chosen=$(echo "$all" | shuf -n 1)
fi

# Append the chosen wallpaper to the used wallpapers file and set the wallpaper
echo "$chosen" >> "$used_wallpapers_file"
feh --bg-scale "$chosen"
## alternative script
# I found this from [[reddit][this reddit post]].  The interesting about it is
# that it handles spaces and unusual characters in wallpapers, which can
# definitely happen when you download them online.
# find /mnt/Gog/Papers -type f ( -name '.jpg' -o -name '.png' ) -print0 | shuf -n1 -z | xargs -0 feh --bg-fill --no-xinerama
## more references
# https://www.roboleary.net/2021/09/02/linux-change-wallpaper.html
