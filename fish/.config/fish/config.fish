#=======================================================
# .config/fish/config.fish (derived from .bash_aliases)
# Steven Hum - Parallel Systems Inc.
#=======================================================

# bash "shift" equivalent: $argv[(seq 2 (echo $argv | wc -w))]
# e.g. set args $argv[(seq 2 (echo $argv | wc -w))]

set fish_color_autosuggestion brown

# clear default shell greeting (redundant re-execution)
set --export fish_greeting

# vim plugins using system() calls, fail with fish!
# set shell here in case any other apps depend on sh
#set --export SHELL /bin/sh

# for xmonad onhost
set --export HOST (hostname)

# enable X11 ansi colours (but not for syslogin as that causes spurious fish output.. just being anal)
# tmux sets TERM to screen-256color
# !p xinit; or set --export TERM xterm-256color
set --export PRINTER HP_LaserJet_1320_series

# paths
set --export CDPATH . .. ../.. ~ ~/.config ~/sync ~/sync/user /usr /
#set --export PATH ~/bin /usr/local/bin /usr/bin /bin /sbin /usr/sbin /usr/local/games /usr/games
#set --export PATH ~/bin /usr/bin /bin /sbin /usr/sbin /usr/local/bin /usr/local/games /usr/games
set --export PATH ~/sync/bin /usr/bin /bin /sbin /usr/sbin /usr/local/bin /usr/bin/core_perl /usr/local/games /usr/games ~/.cabal/bin ~/.xmonad/bin ~/.gem/ruby/2.1.0/bin


# browser
set --export BROWSER w3m
# [ -z (pidof privoxy) ]; or set --export HTTP_PROXY localhost:8118
# [ -z (pidof squid3) ]; or set --export HTTP_PROXY localhost:3128
set --export XDG_DOWNLOAD_DIR /data/downloads
set --export NNTPSERVER news.eternal-september.org

# editors
set --export EDITOR gvim -f
# my.rubyroom margin calculation for 9 and 10 pt
#set --export CHARSIZE (echo "{scale=6; 1024/144}" | bc)
set --export CHARSIZE (echo "{scale=6; 1024/126}" | bc)
# ls
set --export LSCOLORS gxfxcxdxbxegedabagacad

# lua
set --export LUA_INIT @$HOME/.luarc
# ruby
set --export RI '--format ansi --no-pager'

# stringer rss aggegator
set --export STRINGER_DATABASE stringer_live
set --export STRINGER_DATABASE_USERNAME stringer
set --export STRINGER_DATABASE_PASSWORD stringer
set --export RACK_ENV production

## experimental vi mode bindings
#. ~/.config/fish/functions/vi-mode.fish
#function fish_user_keybindings
#    vi_mode_insert
#end
#fish_user_keybindings

# bspwm settings
set --export BSPWM_SOCKET /tmp/bspwm-socket
set --export XDG_CONFIG_HOME ~/.config

alias am yaourt
alias pm pacman

# fix tmux home/end keys
# see TERM above, screen-256 fixes xterm-256 bind conflict
# if [ -n "$TMUX" ]
#     bind "[1~" beginning-of-line
#     bind "[4~" end-of-line
# end

# final user config initialization
status --is-interactive; and [ -f ~/.config/fish/user.fish ]; and . ~/.config/fish/user.fish

#=======================================================
# End of config.fish 
#=======================================================
