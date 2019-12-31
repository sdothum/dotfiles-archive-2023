#!/bin/bash
# gtk engine for ffuu theme

cd ~/depot/desktop
sudo aptitude install libgtk2.0-dev
tar xvzf 121881-equinox-1.50.tar.gz
cd equinox-1.50/
./configure --prefix=/usr --enable-animation
make
sudo make install