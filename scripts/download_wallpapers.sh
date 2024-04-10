#!/bin/bash

# This script is meant to automate the downloading of wallpapers.  When I run
# low on wallpapers, it will download wallpapers for me.

wallpaper_dir="$HOME/Pictures/wallpapers/"
wallpaper_download_script="$HOME/Downloads/wallhaven-downloader/wallhaven.sh"
wallpaper_count=$(find "$wallpaper_dir" -type f \( -name "*.jpg" -o -name "*.png" \) | wc -l)

# The script can only download wallpapers in multiples of 24.  So I only
# download more wallpapers if the wallpaper count goes below a certain
# threshold.  By the way I am not sure why it needs the multiples of 24.  I do
# not know what would happen if I changed =THUMBS= to 1 and tried specifying the
# count to something else.
BASE=24
wallpaper_max=$(( "$BASE" * 6 )) # 144 wallpapers
wallpaper_diff=$(( wallpaper_max - wallpaper_count))
wallpapers_to_download=$(( "$wallpaper_diff" / "$BASE" * "$BASE" ))

echo "There are currently $wallpaper_count wallpapers in $wallpaper_dir"
echo "This is $wallpaper_diff less than the $wallpaper_max."
echo "Therefore I am installing $wallpapers_to_download (* (/ $wallpaper_diff $BASE ) $BASE) wallpapers."

if [ "$wallpapers_to_download" -gt 0 ]; then
    echo "Downloading $wallpapers_to_download wallpapers..."
    "$wallpaper_download_script" \
        --number "$wallpapers_to_download" \
        --filter 110 \
        --location "$wallpaper_dir" \
        --categories 101 \
        --type standard \
        --resolution 1440x900 \
        --parallel 1 \
        --mode random
    wallpaper_count=$(find "$wallpaper_dir" -type f \( -name "*.jpg" -o -name "*.png" \) | wc -l)
    echo "There are now $wallpaper_count wallpapers."
else
    echo "There are wallpapers. Current count: $wallpaper_count"
fi
