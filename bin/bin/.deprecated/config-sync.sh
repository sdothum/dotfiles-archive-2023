#!/bin/sh
# run "config-sync.sh --delete" to delete extraneous files
echo
echo "::: $(date '+%Y.%m.%d %H:%M:%S') :::"
echo
rsync -av $@ ~/Dropbox/debian/bin ~/ 
#rsync -av --delete ~/Dropbox/debian/.config/etc/apt ~/.config/etc/ 
#rsync -av --delete ~/Dropbox/debian/.config/etc/lynx-cur ~/.config/etc/ 
#rsync -av --delete ~/Dropbox/debian/.config/etc/news ~/.config/etc/ 
#rsync -av --delete ~/Dropbox/debian/.config/etc/polipo ~/.config/etc/ 
#rsync -av --delete ~/Dropbox/debian/.config/etc/privoxy ~/.config/etc/ 
#rsync -av --delete ~/Dropbox/debian/.config/etc/X11 ~/.config/etc/ 
rsync -av $@ ~/Dropbox/debian/.config/etc ~/.config/ 
rsync -av $@ ~/Dropbox/debian/.config/usr ~/.config/ 
