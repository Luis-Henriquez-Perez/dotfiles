#!/bin/bash

## setting the wallpaper
# Directory containing wallpapers
wallpaper_dir="~/Pictures/wallpapers/"

# Choose a random wallpaper from the directory
wallpaper=$(ls "$wallpaper_dir" | shuf -n 1)

echo "$wallpaper"
# Set the wallpaper using feh (replace with your wallpaper tool)
# feh --bg-scale "$wallpaper_dir/$wallpaper"

## alternative script
# I found this from [[reddit][this reddit post]].
# find /mnt/Gog/Papers -type f ( -name '.jpg' -o -name '.png' ) -print0 | shuf -n1 -z | xargs -0 feh --bg-fill --no-xinerama

## more references
# https://www.roboleary.net/2021/09/02/linux-change-wallpaper.html
