#!/usr/bin/dash

# X11 Initialization
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# Executed by startx (run your window manager from here):
#   startx -- -layout Multihead
# Note: .xinitrc takes precedence over .xsession, use either but ensure dbus is enabled for thunar

# ................................................................... User setup

# Kill any dangling shell sessions from prior xsessions
killall bash

# Where the default home folders have not already been created, generate them
if [ -x /usr/bin/xdg-user-dirs-update ]; then
   /usr/bin/xdg-user-dirs-update
fi

# ...................................................................... Daemons

# Enable cut and paste
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
# X11 display managers usually start pulseaudio.. but we're using tiling window managers :-)
pulseaudio --start
# Enable automounting removable devices
udiskie --use-udisks2 &

# ........................................................................ D-Bus

# Ensure that the D-Bus Communication System is running properly to fix file
# management, authentication, and other essential system processes
# if which dbus-launch 2>&1 >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
#     eval "$(dbus-launch --sh-syntax --exit-with-session)"
#     echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
# fi

# ................................................................ Gnome keyring

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# ........................................................................... UI

# Ensure that GTK themes are applied uniformly in the Desktop Environment
export GTK2_RC_FILES="$HOME/.gtkrc-2.0" 
export _JAVA_AWT_WM_NONREPARENTING=1

# Initialize keyboard
# [ $(hostname) = luna ] || setxkbmap us -variant colemak
# [ -e $HOME/.Xmodmap ] && xmodmap $HOME/.Xmodmap
# Ensure fonts are available to applications
xset +fp $HOME/.fonts
# xset fp rehash

# Set arrowhead pointer instead of "X"
xsetroot -cursor_name left_ptr

# Autohide mouse pointer
# postponed, as this conflicts with tabbed vimb, see autostart
# which unclutter >/dev/null && unclutter -idle 1 -jitter 2 -root &

# utf-8
xrdb -load $HOME/.Xresources

# Login manager default
DEFAULTSESSION=herbstluftwm
windowmanager=$1
[ $windowmanager ] || windowmanager=$(cat $HOME/.windowmanager)
#echo $windowmanager >$HOME/.xsession.trace

# ...................................................................... Display

# Setup multihead display
# [ $(hostname) = luna ] && xrandr --output DFP10 --mode 1680x1050 --rate 60 --output DFP11 --primary --mode 2560x1600 --rate 60 --right-of DFP10
[ $(hostname) = luna ] && [ $windowmanager != budgie ] && xrandr --output DVI-1 --primary --mode 2560x1600 --rate 60 --right-of DVI-0

# ............................................................... Window manager

setpath() {
  path=$1
  [ -d $1/bin ] && path=$path:$1/bin
  export PATH=$(echo $path:$(echo $PATH | sed "s|$path:||g"))
}

# unset to use qwerty keyboard wasd mappings
export COLEMAK_DH=true

case $windowmanager in
  2bwm) # Set a nice background.
        xsetroot -solid grey20
        # Load resources.
        xrdb -load $HOME/.Xresources
        # Start window manager in the background. If it dies, X still lives.
        2bwm &
        # Start a terminal in the foreground. If this dies, X dies.
        exec urxvt ;;

  awesome) exec awesome ;;

  bspwm) killall panel 2>/dev/null
         rm -f /tmp/bspwm-socket 2>/dev/null
         rm -f /tmp/bspwm.lock
         setpath $HOME/.config/bspwm
         sxhkd -c $HOME/.config/bspwm/sxhkdrc &
         exec bspwm >/tmp/bspwm.log 2>&1 ;;

  budgie) $HOME/.screenlayout/budgie.sh
          autostart
          exec budgie-session ;;

  cinnamon) exec gnome-session-cinnamon ;;
  goomwwm)  exec goomwwm ;;
  dwm)      exec dwm ;;

  herbstluftwm) pkill -KILL -f 'herbst'
                rm -f /tmp/herbstluftwm:*
                # enable debug trace by referencing log file
                export TRACE=/tmp/herbstluftwm.log
                export PANEL_BOTTOM=true
                # export PANEL_MARGIN=true
                # setroot (wallpaper) last wallpaper (blank) last blank wall, otherwise random blank wall
                export SETROOT=wallpaper
                setpath $HOME/.config/herbstluftwm
                . $HOME/.config/herbstluftwm/config/ENV
                # initial build install?
                which xdotool >/dev/null || urxvt &
                exec herbstluftwm >/tmp/herbstluftwm.log 2>&1 ;;

  gnome)  exec gnome-session ;;
  i3)     exec i3 ;;
  kde)    exec startkde ;;
  lxde)   exec lxsession ;;
  mate)   exec mate-session ;;
  notion) exec notion ;;

  openbox) sed 's/_user_/'$(whoami)'/' $HOME/.config/obmenu-generator/schema.pl -i
           # ensure that obmenu-generator (a pipe menu) is set to the user
           # needs only to be run once, so last sed command hashes the sed commands!
           sed 's/_user_/'$(whoami)'/' $HOME/.config/obmenu-generator/config.pl -i exec openbox-session ;;

  ratpoison) exec ratpoison ;;
  razor-qt)  exec razor-session ;;
  spectrwm)  exec spectrwm ;;
  stumpwm)   exec stumpwm ;;
  subtle)    exec subtle ;;
  wmii)      exec wmii ;;

  wmutils) export SXHKD_SHELL=/usr/bin/dash
           sxhkd -c $HOME/.config/wmutils/sxhkdrc &
           setpath $HOME/.config/wmutils
           wew | yawee &
           urxvt &
           exec xwait ;;

  xfce)   exec xfce4-session ;;
  xmonad) exec xmonad 2>/tmp/xmonad.log ;;

  *) exec $DEFAULTSESSION ;;
esac

# vim: set ft=sh: #
