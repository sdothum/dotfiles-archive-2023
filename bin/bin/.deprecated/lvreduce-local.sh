#!/bin/sh
# lvreduce /local size (in Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-local.sh size (in Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /local
	e2fsck -f /dev/mapper/vg1-local
	/sbin/resize2fs -p /dev/mapper/vg1-local $1
	/sbin/lvreduce -v -L $1 /dev/mapper/vg1-local
	e2fsck -f /dev/mapper/vg1-local
	/sbin/resize2fs -p /dev/mapper/vg1-local 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-local
	e2fsck -f /dev/mapper/vg1-local
	mount /local
	df -h
fi
