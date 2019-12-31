#!/bin/sh
# lvextend swap +size (in Mbytes or Gbytes)
# best done in recovery mode

lvs
if [ $1. = . ]; then
	echo "lvextend-swap.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	swapoff -v /dev/mapper/vg1-swap
	/sbin/lvextend -v -L $1 /dev/mapper/vg1-swap
	/sbin/mkswap -v1 -f /dev/mapper/vg1-swap
	swapon -v /dev/mapper/vg1-swap
	lvs
fi
