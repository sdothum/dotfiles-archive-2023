#!/bin/sh
# sync this crunchbang configuration
set -x
rsync -av --delete ~/.config ~/Dropbox/crunchbang.$(hostname)/
rsync -av --no-r ~/* ~/.* ~/Dropbox/crunchbang.$(hostname)/
