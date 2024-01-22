## Second draft that accounts for spaces

wallpaper_dir="$HOME/Pictures/wallpapers/"
used_wallpapers_file="$HOME/.cache/sh_used_wallpapers_file"

# Ensure the wallpaper file exists.
touch "$used_wallpapers_file"

# Choose a random wallpaper from the directory that is not among the chosen wallpapers.
# Using find with -print0 and mapfile to handle spaces in filenames.
mapfile -d $'\0' all < <(find "$wallpaper_dir" -type f -print0)

# Get the used wallpapers from the used_wallpapers_file.
# Read the file into an array, assuming one line per wallpaper.
mapfile -t used < "$used_wallpapers_file"

# Prepare the used wallpapers for comparison
printf "%s\n" "${used[@]}" | sort > /tmp/used_sorted

# Choose a wallpaper that's not used--the set-difference of (- all used)
# Choose a wallpaper that's not in the used list
chosen=$(comm -23 <(printf "%s\n" "${all[@]}" | sort) /tmp/used_sorted | shuf -n 1)

# If chosen is empty, then reset the used wallpapers file and choose again
if [[ -z $chosen ]]; then
    echo "No new wallpapers found. Resetting used wallpapers."
    > "$used_wallpapers_file"
    chosen=$(printf "%s\n" "${all[@]}" | shuf -n 1)
fi

# Append the chosen wallpaper to the used wallpapers file and set the wallpaper
echo "$chosen" >> "$used_wallpapers_file"
feh --bg-scale "$chosen"
