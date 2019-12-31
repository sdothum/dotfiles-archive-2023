#!/bin/bash
# exposure blending hdr manager

sudo apt-get install python-pyexiv2

cd /data/depot/photography/macrofusion
tar xzvf macrofusion_0.7.3.orig.tar.gz

sudo mv macrofusion-0.7.3 /usr/local/bin/
sudo ln -s /usr/local/bin/macrofusion-0.7.3/macrofusion.py /usr/local/bin/macrofusion
