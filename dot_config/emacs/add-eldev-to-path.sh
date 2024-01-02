# * install
# This installs =eldev= to =$PATH=.  I took it from the [[https://github.com/emacs-eldev/eldev][eldev github page]].  The result
# should look like [[file:snapshots/succesfully-installed-eldev.png][this]].  I'll also note that I recommend you do this asap if you
# intend on using =eldev= because it is easier to actually see if things are working
# with this.
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh

# In addition to this the instructions say to add in case it already isn't.

# * add to path
# This line is 
# export PATH="/home/luis/.local/bin:$PATH"
