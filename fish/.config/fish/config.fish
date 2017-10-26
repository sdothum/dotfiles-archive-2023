# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ................................................................. Command line

# turn off
set fish_greeting

# only shells with vi mode accepted here! (suppress fish_vi_key_bindings eval error!!)
fish_vi_key_bindings ^/dev/null

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
echo $PATH | grep -q "$HOME/.local/bin"
  or set -x PATH $PATH ~/.local/bin ~/.cabal/bin ~/.gem/ruby/(rubyver)/bin /bin /sbin /usr/sbin /usr/bin/core_perl /usr/local/games ^/dev/null

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
set -x PLANCK true

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
set -x BROWSER vimbt

# proxies
# [ -z (pidof privoxy) ] ;or set -x HTTP_PROXY localhost:8118
# [ -z (pidof squid) ] ;or set -x HTTP_PROXY http://localhost:3128/
set -x HTTP_PROXY http://luna:3128/

set -x XDG_DOWNLOAD_DIR /net/downloads/http
set -x NNTPSERVER news.sunnyusenet.com

# ..................................................................... Defaults

# default editor
# set -x EDITOR 'vi -e'
set -x EDITOR 'gvim -f'
set -x VISUAL 'gvim -f'
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

# ...................................................................... Session

console_login
user_login
# clear 'fish' tmux window name
pidof tmux >/dev/null ;and tmux rename-window ''
