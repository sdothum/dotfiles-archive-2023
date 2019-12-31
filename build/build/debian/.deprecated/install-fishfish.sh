#!/bin/bash
# beta fish build
set -x

sudo apt-get doxygen libncurses5-dev autoconf libxt-dev

cd /data/depot/system/fish
unzip fish-shell-fish-shell-OpenBeta_r2-122-gb1281c3.zip

cd fish-shell-fish-shell-b1281c3
autoconf
./configure
make
sudo make install
