
# Zsh Environment
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ............................................................... User functions

# source autoload functions for non-interactive zsh instances
# fpath=(~/.zsh/**/* $fpath)
# for i in ~/.zsh/**/*
# do
#   [[ -f $i ]] && autoload -Uz ${i##*/}
# done
source ~/.zsh/colors.zsh

# make zsh help available to dmenu
autoload -U run-help
unalias run-help
alias help='run-help'

# ................................................................. User session

# default shell
export SHELL=/usr/bin/zsh
export XTERM_SHELL=/usr/bin/zsh
export KEYTIMEOUT=1 

# for xmonad onhost
export HOST=$(hostname)
# default printer
export PRINTER=HP_LaserJet_1320_series

# paths
export CDPATH=.:..:../..:~:~/.config:~/stow:/usr:/
export PATH=$PATH:~/.cabal/bin:~/.gem/ruby/2.3.0/bin:/bin:/sbin:/usr/sbin:/usr/bin/core_perl:/usr/local/games

# gpg key
[[ -S ~/.gnupg/S.gpg-agent ]] && export GPG_AGENT_INFO=~/.gnupg/S.gpg-agent
export PASSWORD_STORE_CLIP_TIME=60

# suppress no newline % symbol marker
export PROMPT_EOL_MARK=''

# ..................................................................... Internet

# default browser
# export BROWSER=chromium
# export BROWSER=luakit
# export BROWSER=qutebrowser
export BROWSER=vimb

# proxies
[[ -z $(pidof privoxy) ]] || export HTTP_PROXY=localhost:8118
[[ -z $(pidof squid3) ]] || export HTTP_PROXY=localhost:3128

export XDG_DOWNLOAD_DIR=/net/downloads/http
export NNTPSERVER=news.sunnyusenet.com

# ..................................................................... Defaults

# default editor
export EDITOR='gvim -f'
export XIVIEWER='feh'
export PLAYER='mpv'
# less prompt
export LESS='-RX -P ?B %f  %lt-%lb/%L  %Pb\%: [pipe]  %lt-%lb/\.\.'
export PAGER='less'

# lua
export LUA_INIT="@$HOME/.luarc"
# ruby
export RI='--format ansi --no-pager'
