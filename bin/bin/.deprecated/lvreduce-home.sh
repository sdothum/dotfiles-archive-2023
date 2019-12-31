#!/bin/sh
# lvreduce /home size (in Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-home.sh size (in Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /home
	e2fsck -f /dev/mapper/vg1-home
	/sbin/resize2fs -p /dev/mapper/vg1-home $1
	/sbin/lvreduce -v -L $1 /dev/mapper/vg1-home
	e2fsck -f /dev/mapper/vg1-home
	/sbin/resize2fs -p /dev/mapper/vg1-home 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-home
	e2fsck -f /dev/mapper/vg1-home
	mount /home
	df -h
fi
