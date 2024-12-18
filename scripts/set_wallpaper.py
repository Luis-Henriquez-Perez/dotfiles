# This script chooses a random wallpaper from the wallpapers I have and sets it.
# It registers the used wallpapers in `used_wallpapers_file`.

import os
import glob
import random
from log import get_logger

logger = get_logger(__name__)
used_wallpapers_file = os.path.expanduser("~/.cache/used_wallpapers_file_1")

wallpaper_dir = os.path.expanduser("~/Pictures/wallpapers/")

all_wallpapers = glob.glob(os.path.join(wallpaper_dir, '*.jpg')) + \
                 glob.glob(os.path.join(wallpaper_dir, '*.png'))

os.makedirs(os.path.dirname(used_wallpapers_file), exist_ok=True)
open(used_wallpapers_file, 'a').close()

with open(used_wallpapers_file, 'r') as f:
    used_wallpapers = f.read().splitlines()

print(f"List of used wallpapers: {used_wallpapers!r}")

unused_wallpapers = list(set(all_wallpapers) - set(used_wallpapers))

if unused_wallpapers:
    chosen_wallpaper = random.choice(unused_wallpapers)
else:
    print("No wallpapers found, resetting used wallpapers...")
    with open(used_wallpapers_file, 'w') as f:
        f.write('')
    chosen_wallpaper = random.choice(all_wallpapers)

with open(used_wallpapers_file, 'a') as f:
    f.write(chosen_wallpaper + '\n')

print(f"Chosen wallpaper: {chosen_wallpaper!r}")
os.system(f"feh --bg-scale '{chosen_wallpaper}'")
