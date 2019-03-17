# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ................................................................. Command line

# turn off
set fish_greeting

# only shells with vi mode accepted here!
# fish_vi_key_bindings ^/dev/null  # suppress fish_vi_key_bindings eval error!!
fish_vi_key_bindings

# fuzzy searches
test -e /usr/share/fish/completions/autojump.fish
  and source /usr/share/fish/completions/autojump.fish

# ............................................................ Shell environment

# default shell
set -x SHELL /usr/bin/fish
set -x XTERM_SHELL /usr/bin/fish
set -x KEYTIMEOUT 1

# paths
set -x CACHEDIR $HOME/.cache
set -x CDPATH . .. ../.. ~ ~/.config ~/stow /usr / ^/dev/null
echo $PATH | grep -q "$HOME/.gem/ruby/(rubyver)/bin"
  or set -x PATH $PATH ~/.gem/ruby/(rubyver)/bin ~/.cabal/bin /bin /sbin /usr/sbin /usr/bin/core_perl /usr/local/games ^/dev/null

# ........................................................... System environment

set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8

# gpg key
test -S ~/.gnupg/S.gpg-agent
  and set -x GPG_AGENT_INFO ~/.gnupg/S.gpg-agent
set -x PASSWORD_STORE_CLIP_TIME 60

# ..................................................................... Hardware

# unset to use qwerty keyboard wasd mappings
set -x BEAKL true
# set -x COLEMAK true
set -x TERM xterm-256color

# .......................................................... Network environment

# default printer
set -x PRINTER HP_LaserJet_1320_series
set -x PROOF $HOME/.proof

# main server
set -x SERVER luna

# ..................................................................... Internet

# default browser changes require login (Ctrl-d) for X11 autostart
# set -x BROWSER chromium
# set -x BROWSER luakit
# set -x BROWSER qutebrowser
# set -x BROWSER surf
set -x BROWSER vimb
# set -x BROWSER vimbt

# proxies
# [ -z (pidof privoxy) ] ;or set -x HTTP_PROXY localhost:8118
# [ -z (pidof squid) ] ;or set -x HTTP_PROXY http://localhost:3128/
connected luna ;and set -x HTTP_PROXY http://luna:3128/

set -x XDG_DOWNLOAD_DIR /net/downloads/http
set -x NNTPSERVER news.sunnyusenet.com

# tt-rss
set -x SELF_URL_PATH 'http://luna:8000/tt-rss/'

# ..................................................................... Defaults

# default editor
# set -x EDITOR 'vi -e'
set -x EDITOR 'vim'
set -x VISUAL 'gvim -f'
# fzf-vim theme
set -x FZF_DEFAULT_OPTS "--cycle --reverse  --prompt='    ─────  ' --ansi --color=dark,hl:#dc322f,hl+:#dc322f,fg+:232,bg+:#fdf6e3,bg:#fdf6e3,marker:160,prompt:#268bd2,info:#268bd2"
set -x NNN_USE_EDITOR 1
set -x NNN_SHOW_HIDDEN 1
set -x NNN_RESTRICT_0B 1

set -x XIVIEWER 'feh'
set -x PLAYER 'mpv'
# less prompt
set -x LESS '-RX -P ?B %f  %lt-%lb/%L  %Pb\%: [pipe]  %lt-%lb/\.\.'
set -x PAGER 'less'

# ..................................................... Development environments

# lua
set -x LUA_INIT "@$HOME/.luarc"
# ruby
set -x RI '--format ansi --no-pager'
# for xmonad onhost
set -x HOST (hostname)
# commom configurations (dotfiles)
set -x STOW $HOME/stow

# ...................................................................... Session

set -x SESSION $HOME/.session
# proxy override
test -e $SESSION/http_proxy ;and set -x HTTP_PROXY (cat $SESSION/http_proxy)

set -x XDG_DOWNLOAD_DIR /net/downloads/http
set -x NNTPSERVER news.sunnyusenet.com

# tt-rss
set -x SELF_URL_PATH 'http://luna:8000/tt-rss/'

# ..................................................................... Defaults

set -x http_proxy $HTTP_PROXY

console_login
user_login
# clear 'fish' tmux window name
pidof tmux >/dev/null ;and begin console ;or tmux rename-window '' ;end
