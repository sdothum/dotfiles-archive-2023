#!/bin/sh
# manual X11 window manager startup
# startx -- -layout Multihead
# note: .xinitrc takes precedence over .xsession, use either but ensure dbus is enabled for thunar

# setup multihead display
[ $(hostname) = luna ] && xrandr --output DVI-1 --primary --mode 2560x1600 --rate 60 --right-of DVI-0
eval $(dbus-launch --sh-syntax --exit-with-session)

windowmanager=$(cat ~/.windowmanager)
#echo $windowmanager > ~/.xsession.trace
case $windowmanager in
  i3)     exec ck-launch-session dbus-launch --exit-with-session i3
          ;;
  notion) exec ck-launch-session dbus-launch --exit-with-session /usr/bin/notion
          ;;
  xfce4)  exec ck-launch-session dbus-launch --exit-with-session xfce4-session
          ;;
  *)      exec ck-launch-session dbus-launch --exit-with-session openbox-session
          ;;
esac
