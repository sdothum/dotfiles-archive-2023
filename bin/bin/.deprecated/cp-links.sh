#!/bin/sh
# copy links to /usr/local/bin
#set -x
cd ~/bin
for i in *; do
	if [ -L $i ]; then
		cp -vf $i /usr/local/bin 2>/dev/null
	fi
done
