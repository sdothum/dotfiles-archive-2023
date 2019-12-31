#!/bin/sh
# manual config file substitution
# e.g. ./original.sh 

# backup original config file
if [ ! -f $1.original ]; then
	if [ -f $1 ]; then
		cp -v $1 $1.original
	fi
fi

# replacement file from my .config
cp -v /home/shum/.config$1 $1
