#!/bin/sh
# convert lossless itunes library to flac
# find . -name *.m4a -print0 | xargs -0 -P 4 -n 1 toflac.sh

codec=${1##*.}

case $codec in
	m4a)	ffmpeg -i "$1" -f flac "${1%.m4a}.flac"
			;;
	mp4)	ffmpeg -i "$1" -f flac "${1%.mp4}.flac"
			;;
	aiff)	ffmpeg -i "$1" -f flac "${1%.aiff}.flac"
			;;
	*)		echo "- warning - no toflac.sh ffmpeg conversion defined for lossless $codec: $1"
			;;
esac
