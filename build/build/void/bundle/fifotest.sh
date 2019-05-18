#!/usr/bin/dash
# sdothum - 2016 (c) wtfpl

xbps-install -S
xbps-install sutils

trap 'rm -f fifo' INT 
mkfifo fifo

clock -i 1 -sf "T> %-I:%M:%S%P" >fifo &
clock -i 1 -sf "D> %a %-d %b '%y" >fifo &

cat fifo
