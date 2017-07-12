#!/usr/bin/fish
## notion startup script (based on openbox autostart script)

# for vim plugins using system() calls
#set --export SHELL /bin/sh

# :. set primary monitor and set maximum resolution
# see /etc/gdm/Init/Default and .xinitrc
#
#xrandr --output DVI-1 --primary --mode 2560x1600 --rate 60 --right-of DVI-0
#
# :.

# startup daemon in background if not already running
# note: still must force bg command for sleep statements to prevent display delay e.g. "(... sleep ...) &"
function startup; !p "$argv[1]"; and echo $argv; and eval "$argv &"; end
function stagger; echo $argv; sleep $argv[1]; startup $argv[(seq 2 (echo $argv | wc -w))] &; end
function terminal; echo $argv; sleep 3s; xfce4-terminal -T "$argv[1]" --role "$argv[2]" -e "$argv[3]" &; end

function hostrole; [ (hostname) = $argv[1] ]; and echo $argv[2]; or echo $argv[3]; end
function host!p; [ (hostname) = $argv[1] ]; and !p $argv[2]; end

## Start session manager
startup lxsession

## Launch panel
#startup stalonetray -p
killall trayion; [ (hostname) = luna ]; and startup trayion -iconsize 28; or startup trayion -iconsize 20

## Enable power management
startup xfce4-power-manager

## Start Thunar Daemon
startup thunar --daemon

## Set desktop wallpaper
#nitrogen --restore &cmd

## Enable Eyecandy - off by default, uncomment one of the commands below.
## Note: cairo-compmgr prefers a sleep delay, else it tends to produce
## odd shadows/quirks around tint2 & Conky.
#stagger 10s cb-compmgr --cairo-compmgr &
#startup cb-compmgr --xcompmgr
killall compton; stagger 10s compton &

## Launch network manager applet
stagger 3s nm-applet &

## Detect and configure touchpad. See 'man synclient' for more info.
if [ (egrep -iq 'touchpad' /proc/bus/input/devices) ]
    synclient VertEdgeScroll=1 &
    synclient TapButton1=1 &
end

## Start xscreensaver
!p xbmc; and startup xscreensaver -no-splash

## Start Conky after a slight delay
# :. dual column for netbook...
#stagger 3s conky -q &
stagger 3s conkywonky &
# :.

# :. determine audio card number before initializing mixer
# no longer required with pulseaudio
#[ (hostname) = luna ]; and ~/bin/asound-usb.sh
# :.
## Start volumeicon after a slight delay
killall volumeicon; stagger 3s volumeicon &

## Start Clipboard manager
stagger 3s parcellite &

## Bad Nautilus, minimises the impact of running Nautilus under
## an Openbox session by applying some gconf settings. Safe to delete.
#cb-bad-nautilus &

## The following command will set-up a keyboard map selection tool when
## running in a live session.
#cb-setxkbmap-live &

## cb-welcome - post-installation script, will not run in a live session and
## only runs once. Safe to remove.
#stagger 10s cb-welcome --firstrun &

## cb-fortune - have Statler say a little adage
#stagger 120s cb-fortune &

# Autostart the Dropbox deamon
#stagger 30s ~/.dropbox-dist/dropboxd &
!p .dropbox-dist/dropbox; and stagger 15s ~/.dropbox-dist/dropboxd &

# :. all my stuff !!
# extend left shift key to international backslash key
# remap caps lock to backspace
[ -f /etc/X11/Xmodmap ]; and xmodmap /etc/X11/Xmodmap

# background processes
# autokey and guake startup on their own...
startup synapse -s
#startup autokey
!p /bin/autokey; and autokey &
startup artha

# allow rtorrent and sendmail to restart if lock left dangling
rm -f ~/.nzbget.paused 2>/dev/null
rm -f ~/.session/rtorrent.lock 2>/dev/null
rm -f ~/.msmtpqueue/.lock 2>/dev/null
#screen &
set --export TERM xterm-256color
#startup tmux attach

# wiki server
!p thedarnedestthing; and thedarnedestthing.sh

# some other apps
set pong (/bin/ping -c 1 ftp.debian.org | grep -c "64 bytes")
[ "$pong" -lt "1" ]; and set delay 30s; or set delay 0s
[ (hostname) = luna ]; and startup luakit; or stagger $delay luakit &

startup gvim
startup pidgin

# using terminal to startup frame designated cli apps
# note: a red activity message will clutter the main windows until the applications receive focus
!p mutt; and terminal Mail mail "fish -c mutt" &
#!p irssi; and terminal Mail chat irssi &
!p newsbeuter; and terminal "RSS Reader" (hostrole luna rss document) "fish -c news" &
host!p luna slrn; and terminal Usenet usenet slrn &
host!p luna nzbget; and terminal NZB nzb "fish -c nzb" &
host!p luna rtorrent; and terminal BitTorrent bittorrent rtorrent &
killall offlineimap; and terminal IMAP (hostrole luna imap mail) "fish -c imap" &
host!p luna ncmpcpp; and terminal Music music ncmpcpp &
host!p luna pyradio; and terminal Radio radio pyradio &
host!p luna htop; and terminal Process process htop &

