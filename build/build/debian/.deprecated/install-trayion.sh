#!/bin/bash
# application mutt-notmuch.sh
set -x

cd /data/depot/x11/notion
unzip laynor-trayion-v1.4-4-gcb51e28.zip

cd laynor-trayion-cb51e28
sed -e 's,/usr/X11R6,/usr/bin/X11,g' -e 's,\${X11_PREFIX}/include,/usr/include/X11,g' -e 's,\${X11_PREFIX}/lib,/usr/lib/X11,g' Rules.make >Rules.make.sed
mv Rules.make.sed Rules.make
make
sudo make install
