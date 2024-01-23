#!/bin/bash

# I usually want to run emacs scripts with basic setting set that prevent emacs
# from creating lockfiles or directories.  The command gets a bit long and
# repeating it is a pain, so I try to shorten it here.
base-settings-file=$()

# Sometimes too I want the messages buffer to be the initial buffer, and other
# times I would just rather it write it to a file.

# The base settings just stop emacs from creating the auto-save-directory and.
emacs -q -l base-settings-file -l other-file
