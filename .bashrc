# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !
export PAGER="most"
eval "$(starship init bash)"

alias dn="nmcli dev show | grep DNS"
alias fc="sudo free && sync && sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches' && free"
alias psm="sudo ps_mem"
alias al="alsi -l"
alias nv1="sudo nvme smart-log /dev/nvme0"
alias yu='yaourt'
alias tl='ssh root@192.168.1.23'
alias tl2='ssh root@192.168.1.24'
alias build='dpkg-buildpackage -rfakeroot -b'
alias ps="ps -ef"
alias sp='watch -n.1 "cat /proc/cpuinfo | grep \"^[c]pu MHz\""'
alias up='doas emerge --ask --update --deep --with-bdeps=y @world'
alias sy='doas emerge --sync'
alias mods='lspci -k | grep use'
alias v='nvim'
alias vim='nvim'
alias dm='doas rc-service display-manager restart'

# Changing "ls" to "exa"
alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
# Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'
# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

colorscript random
bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

if [ -f /etc/bash_completion ]; then
   . /etc/bash_completion
fi
