#!/bin/sh
# nanoki wiki

cd /usr/local/nanoki
lua Nanoki.luac /home/shum/Dropbox/thedarnedestthing localhost 1080 >>~/tmp/nanoki.log 2>&1 &
#luajit -v Nanoki.lua /home/shum/Dropbox/thedarnedestthing localhost 1080 >>~/tmp/nanoki.log 2>&1 &
