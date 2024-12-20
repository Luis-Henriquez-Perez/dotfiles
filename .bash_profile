#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# Created by `pipx` on 2024-10-01 19:40:09
export PATH="$PATH:/home/luis/.local/bin"

#.bash_profile
# https://github.com/White-Oak/arch-setup-for-dummies/blob/master/setting-up-ssh-agent.md
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
