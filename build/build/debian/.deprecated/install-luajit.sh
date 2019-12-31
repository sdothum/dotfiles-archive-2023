#!/bin/bash
# luajit compiler for browser
set -x

# luajit code optimizer
cd /data/depot/www/luakit
rm -Rf LuaJIT-2.0.0-beta9 2>/dev/null
tar xvzf LuaJIT-2.0.0-beta9.tar.gz

cd LuaJIT-2.0.0-beta9
make
sudo make install
sudo ln -sf /usr/local/bin/luajit-2.0.0-beta9 /usr/local/bin/luajit
