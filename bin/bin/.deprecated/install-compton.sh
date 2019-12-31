#!/bin/sh
# lighter weight compositing manager

cd /net/depot/x11/compton
rm -Rf chjj-compton-d52f7a0
unzip chjj-compton-d52f7a0.zip

cd chjj-compton-d52f7a0
make
sudo make install
