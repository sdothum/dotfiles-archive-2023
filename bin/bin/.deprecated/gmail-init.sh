#!/bin/sh
# purge mail and repull from gmail

killall offlineimap
rm -rf ~/.offlineimap/*
rm -rf ~/.mail/gmail/*
rm -rf ~/.config/mail/mutt/cache/*
tail -f ~/tmp/imapfilter.log &
tmux split-window
offlineimap -c ~/.config/mail/offlineimaprc -u blinkenlights

