#!/usr/bin/fish
# script must match default user shell (#!/bin/sh is ignored for some reason by gdm)

# setup multihead display
test (hostname) = "luna"; and xrandr --output DVI-1 --primary --mode 2560x1600 --rate 60 --right-of DVI-0
eval (dbus-launch --sh-syntax --exit-with-session)

set windowmanager (cat ~/.windowmanager)
#echo $windowmanager >~/.xsession.trace
switch $windowmanager
  case i3;      exec ck-launch-session i3
  #case notion;  exec ck-launch-session /usr/local/bin/notion
  case notion;  exec ck-launch-session /usr/bin/notion
  case xfce4;   exec ck-launch-session xfce4-session
  case '*';     exec ck-launch-session openbox-session
end
