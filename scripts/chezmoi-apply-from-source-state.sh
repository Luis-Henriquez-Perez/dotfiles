#!/bin/bash

# source state --> target state

# For a lot of this I took as an example =chezmoi-write= from the chezmoi
# package.  But there is a difference: =chezmoi-write= has access to the file,
# whereas with this command I do not.  The commands =chezmoi status= and
# =chezmoi managed= only output partial paths.  To get the full path you combine
# the output of =chezmoi target-path= and each of =chezmoi-managed=.  And then
# to get the source you can apply it to the result of that.
# Add files which have not been added.

# I am trying here to automate the easy ones.  Therefore, = A=, = M=, =DA= I
# think I would almost always want applied. =MM= I would have to consider more
# closely, probably having to do =chezmoi diff=.  These are the files that have
# been modified in the source state and the target state--something that I do
# not recommend doing.  I would almost always prefer modifying target files.
# Sometimes modify the source files.  But never modify both because then you
# have to diff them.

# = A= and = M= are do not require confirmation chezmoi.  But =DA=, signaling
# the recreation of a file that has been deleted in the actual filesystem, but
# would be added back

# List unapplied files (added in source state but not in target state)
# \|DA.

# Basically do the changes that are safe to do--by which I mean cannot result in
# the loss of information.
# I do not think any of these actions would change the source state.  All of
# them propagate the source state to the target state: = A= and =DA= a add a
# file whereas = M= modifies an existing file in the target state.  So I should
# not need any version controlling here.
unapplied=$(chezmoi status | grep '^\( A\| M\|DA\)' | cut -c4-)
# Exit if no unapplied files
if [ -z "$unapplied" ]; then
    echo "No unapplied files to process."
    exit 0
fi

target_base=$(chezmoi target-path)

# Iterate through unapplied files and apply changes
for file in $unapplied; do
    chezmoi apply "$target_base/$file" --force
done
