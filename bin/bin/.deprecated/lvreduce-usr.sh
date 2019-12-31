#!/bin/sh
# lvreduce /usr size (in Mbytes or Gbytes)
# best done in recovery mode

df -h
if [ $1. = . ]; then
	echo "lvreduce-usr.sh size (in Mbytes or Gbytes)"
	exit
fi
if [ ${1%%[MGT]} -gt 0 ]; then
	umount /usr
	e2fsck -f /dev/mapper/vg1-usr
	/sbin/resize2fs -p /dev/mapper/vg1-usr $1
	/sbin/lvreduce -v -L $1 /dev/mapper/vg1-usr
	e2fsck -f /dev/mapper/vg1-usr
	/sbin/resize2fs -p /dev/mapper/vg1-usr 
	/sbin/fsck.ext4 -v -f /dev/mapper/vg1-usr
	e2fsck -f /dev/mapper/vg1-usr
	mount /usr
	df -h
fi
