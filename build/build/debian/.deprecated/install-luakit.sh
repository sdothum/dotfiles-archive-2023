#!/bin/bash
# luakit browser
set -x

sudo apt-get -y install libwebkit-dev libunique-dev libsqlite3-dev help2man lua5.1 liblua5.1-filesystem0 pkg-config

# browser
cd /data/depot/www/luakit
rm -Rf mason-larobina-luakit-2f6f69c 2>/dev/null
tar xvfz mason-larobina-luakit-2012.03.25-0-g2f6f69c.tar.gz

cd mason-larobina-luakit-2f6f69c
# make error may require: sed 's/help2man -N /help2man -N --no-discard-stderr /' Makefile >Makefile.sed && mv Makefile.sed Makefile
#LUA_PKG_NAME=luajit make USE_LUAJIT=1
make USE_LUAJIT=1
#sudo LUA_PKG_NAME=luajit make install
sudo make install
# add system wide link library path
sudo ldconfig /usr/local/lib

