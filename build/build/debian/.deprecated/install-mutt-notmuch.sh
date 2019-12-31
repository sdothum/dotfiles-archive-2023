#!/bin/bash
# application mutt-notmuch.sh
set -x

sudo apt-get -y install libmail-box-perl

cd /data/depot/mail
rm -Rf mutt-notmuch-f4152bf 2>/dev/null
tar xvzf mutt-notmuch-f4152bf.tar.gz

cd mutt-notmuch-f4152bf
cp -f mutt-notmuch /usr/local/bin/
# placing search directory for sidebar reference under gmail causes infinite symlink recursion by notmuch processing .mail directory
ln -s ~/.cache/mutt_results ~/.mail/.notmuch/=Search 2>/dev/null
