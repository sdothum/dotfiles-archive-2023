#!/bin/sh
# link .config
# /usr/local/bin/ln-config.sh .filename [subfolder]
#set -x
if [[ -z $* ]]; then
	echo /usr/local/bin/config.sh .filename [subfolder]
	exit
fi

cd ~
if [[ ! $2. = . ]]; then
	subfolder=$2/
fi

if [[ -f .${1##*.} -o -d .${1##*.} ]]; then 
	if [[ ! -f ~/.config/$subfolder${1##*.} ]]; then
		mv -v $1 ~/.config/$subfolder${1##*.}
		ln -sv ~/.config/$subfolder${1##*.} $1
	fi
fi

if [[ -L $1 ]]; then
	ls -l $1
else
	echo "-> $1 not moved and linked to .config" 
fi
