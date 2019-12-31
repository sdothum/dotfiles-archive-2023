#!/bin/bash
# lighter weight compositing manager
set -x

cd /data/depot/x11/compton
unzip chjj-compton-d52f7a0.zip

cd chjj-compton-d52f7a0
make
sudo make install
