export TERM="xterm-256color"              # getting proper colors
export HISTORY_IGNORE="(ls|cd|pwd|exit|sudo reboot|history|cd -|cd ..)"
export PAGER="most"

export ZSH="/home/roidm/.oh-my-zsh"

### PATH
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.local/share/applications" ] ;
  then PATH="$HOME/.local/share/applications:$PATH"
fi

ZSH_THEME="spaceship"
#ZSH_THEME="robbyrussell"

plugins=(command-not-found
         git
         zsh-syntax-highlighting 
         zsh-autosuggestions
         history
         zsh-interactive-cd)

source $ZSH/oh-my-zsh.sh

alias clean='doas emerge --ask --depclean'
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

### BASH INSULTER ###
if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

colorscript random
