#!/bin/bash

# Directory containing wallpapers
wallpaper_dir="~/dotfiles/wallpapers/"

# Choose a random wallpaper from the directory
wallpaper=$(ls "$wallpaper_dir" | shuf -n 1)

# Set the wallpaper using feh (replace with your wallpaper tool)
feh --bg-scale "$wallpaper_dir/$wallpaper"
