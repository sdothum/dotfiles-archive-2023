#!/bin/bash
# .bash_aliases
# Steven Hum - Parallel Systems Inc.
#set -x

source ~/.profile &

#stty erase ^?
left_shift_key () { [ -f /etc/X11/Xmodmap ] && xmodmap /etc/X11/Xmodmap; }

alias colemak='setxkbmap us -variant colemak; left_shift_key; echo colemak'
#alias qwfpgj='setxkbmap us -variant basic; left_shift_key; echo qwerty'
alias qwerty='setxkbmap us -variant basic; left_shift_key; echo qwerty'

# enable X11 ansi colours
export TERM=xterm-256color

#
# ANSI terminal colours
#
export ESC='\033'
export _black=$"$ESC[0;30m"
export _gray=$"$ESC[1;30m"
export _white=$"$ESC[0;37m"
export __white=$"$ESC[1;37m"

# _normal
# __bright
export _blue=$"$ESC[0;34m"
export __blue=$"$ESC[1;34m"
export _cyan=$"$ESC[0;36m"
export __cyan=$"$ESC[1;36m"
export _green=$"$ESC[0;32m"
export __green=$"$ESC[1;32m"
export _magenta=$"$ESC[0;35m"
export __magenta=$"$ESC[1;35m"
export _red=$"$ESC[0;31m"
export __red=$"$ESC[1;31m"
export _yellow=$"$ESC[0;33m"
export __yellow=$"$ESC[1;33m"

export _nocolor=$"$ESC[0;0m"

# prompts
#PS1="\[$_red\]\T \[$_cyan\][\u] \[$_green\]\w \[$_nocolor\]\\$ "
PS1="\[$_red\]\T \[$_cyan\][\u] \[$_yellow\]\w \[$_nocolor\]\\$ "
set -o vi
export PS2="> "
export PS3="#? "
export PS4="+"

#
# Session settings
#

# shell history conf
HISTCONTROL=ignoreboth
HISTFILE=~/.bash_history
HISTFILESIZE=9999
#HISTIGNORE='&:ls:ls *:[bf]g:exit'
HISTSIZE=9999
shopt -s histappend histreedit histverify
alias h='history'

# paths
alias path='echo $PATH'
CDPATH=.:..:../..:~:/home:/

#
# Lazy default session xhost config - not secure
#

#if ps -ax | grep -v grep | egrep -q 'X11|XDarwin'; then
#	/usr/X11R6/bin/xhost + 1>/dev/null 2>/dev/null
#	export DISPLAY=:0.0
#	stty erase ^?
#fi

# browser
export BROWSER=lynx
export HTTP_PROXY=localhost:8118
export XDG_DOWNLOAD_DIR=/net/downloads/http

# disable 'xterm-color' for host
export DOMAIN=
domain() { echo $1 | grep '\.' || echo $1$DOMAIN; }

ftp() { /usr/bin/ftp $(domain $1) ; }
ping() { /bin/ping $(domain $1) ; }
#function ssh() { TERM=vt100 /usr/bin/ssh -X $(domain $1) ; }
telnet() { TERM=vt100 /usr/bin/telnet $(domain $1) ; }
traceroute() { /usr/sbin/traceroute $(domain $1) ; }

#
# Printer setup
#

export LPR=-l
# See scripts/startup/setprinter.sh
export LPR_DESTINATION
alias lpr='/usr/bin/lpr $LPR -P $LPR_DESTINATION'

export PR='-e4 -F -l63 -o8 -w100'
pr() { /usr/bin/pr $PR $@ | awk '{ print $0 "\r" }' ; }
prn() { /usr/bin/pr $PR -n $@ | awk '{ print $0 "\r" }' ; }

#
# Aliases
#

export LSCOLORS=gxfxcxdxbxegedabagacad
alias l='ls --color -m'
alias la='ls --color -aF'
alias ls='ls --color -F'
alias lr='ls --color -FR'
alias ll='ls --color -laF'
alias lt='ls --color -ltF'

alias ..='cd ..'
alias ...='cd .. ; cd ..'
# '-i' prompt option can cause scripts to wait for a response (appearing to hang!)
alias cp='cp -i'
usrlocal() { [ $# -gt 0 ] && [ -d /usr/local/bin/$1 ] && sudo rm -Rf /usr/local/bin/$1 ; sudo mv -f ~/downloads/$1 /usr/local/bin/ && cd /usr/local/bin/ && echo $1 && ll $1 ; }
alias mv='mv -i'
alias rm='rm -i'

alias a='ack-grep --color --color-filename=red --ignore-case'
alias ack=ack-grep
alias g='grep --color=auto -i'
alias l='less'
alias m='more'
alias sdiff='/usr/bin/sdiff -w125'

# system control
alias ducks='du -cks * | sort -rn | egrep -v "^0|total"'
alias k='/home/shum/bin/killall.sh'
alias ssnet='sudo netstat -tlnp'
p() { /bin/ps -ef | /bin/egrep -i "$@" 2>/dev/null | /bin/egrep -v egrep ; }
alias su='sudo su'

# editors
alias e=emacs
alias em='emacs -nw'
alias omm=ommwriter
alias n=nano
alias s=scribes
alias st=sublime-text
alias sst='sudo /home/shum/bin/sublime-text'
alias v=vim
alias ssv='sudo /usr/bin/vim'
export EDITOR='gvim  -f'

# suppress non-root warnings
find() { /usr/bin/find $@ 2>/dev/null ; }
alias hugs='hugs -98 +t +Q +k'
alias mysql='mysql -h localhost -u root -p'
alias pdf=evince
alias top=/usr/bin/htop
alias tree='tree -ACF'

# ruby
export RI='--format ansi --no-pager'

# colored man pages
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
      man "$@"
}

#
# End of .bash_aliases (start of package additions)
#
