#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
. "$HOME/.cargo/env"

# Added by @kolayne (copied from ~/.zshrc)
[ -f "/home/nertsal/.ghcup/env" ] && source "/home/nertsal/.ghcup/env" # ghcup-env
alias config='/usr/bin/git --git-dir=/home/nertsal/.cfg/ --work-tree=/home/nertsal'

source /home/nertsal/.config/broot/launcher/bash/br
