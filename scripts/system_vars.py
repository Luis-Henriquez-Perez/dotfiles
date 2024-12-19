# Filename: system_vars.py
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-19 16:50:44
# Description: Provide functions for storing and retrieving variables.
# This gives me a much needed way of perserving state across scripts.

import json

variable_file = "~/.cache/system-variables.json"

def setvar(var, value):
    with open(variable_file, "w") as file:
        json.dump(data, file, indent=4)  # `indent=4` makes it human-readable

def getvar(var):
    with open(variable_file, "r") as file:
        data = json.load(file)

if __name__ == '__main__':
    # Access data
    print(data["current_wallpaper"])
